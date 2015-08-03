{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import Control.Monad.Logger                 (liftLoc, runLoggingT)
import Database.Persist.Sqlite              (createSqlitePool, runSqlPool,
                                             sqlDatabase, sqlPoolSize)
import Import
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)
import Control.Monad.Random (newStdGen)
import Control.Distributed.Process.Backend.SimpleLocalnet (initializeBackend)
import Control.Distributed.Process.Backend.SimpleLocalnet (newLocalNode)
import Control.Distributed.Process.Node (forkProcess)
import Control.Concurrent.Lifted (threadDelay)
import Control.Distributed.Process.Backend.SimpleLocalnet (findPeers)
import Control.Monad.Loops (whileJust_)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Home
import Handler.Admin
import Handler.Settings
import Handler.Register
import Handler.Report
import qualified Data.HashSet as HS
import Control.Distributed.Process (expect)
import Control.Distributed.Process (send)
import Control.Distributed.Process.Closure (mkClosure)
import Control.Distributed.Process (spawn)
import Control.Distributed.Process (spawnLocal)
import qualified Control.Distributed.Process as CH

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and return a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)
    appRandomGen <- newIORef =<< newStdGen
    appBackend <- initializeBackend "localhost"
                    (appDistribPort appSettings) remoteTable
    appLocalNode <- newLocalNode appBackend
    appEvalQueue <- newTBMQueueIO 20
    queuePid  <- forkProcess appLocalNode $ do
      whileJust_ (atomically $ readTBMQueue appEvalQueue) $ \ (spec, input, chan) -> do
        let AppSettings{..} = appSettings
        them <- expect
        void $ spawnLocal $ do
          (p, recv) <- CH.newChan
          send them $
            RatingSettings (map unpack appPackageDBs) appTrustedPkgs appDistrustedPkgs
            (unpack spec) (unpack input) p
          let loop = do
                coming <- CH.receiveChan recv
                atomically $ writeTBMQueue chan coming
                case coming of
                  Finished    -> atomically $ closeTBMQueue chan
                  Exception{} -> atomically $ closeTBMQueue chan
                  _ -> loop
          loop

    ps <- newIORef . HS.fromList =<< findPeers appBackend 1000000
    void $ forkProcess appLocalNode $ forever $ do
      olds <- readIORef ps
      forM_ olds $ \p -> void $ spawn p $ $(mkClosure 'evaluateReport) queuePid
      threadDelay (5*10^(6 :: Integer))
      incomings <- liftIO $ HS.fromList <$> findPeers appBackend 1000000
      let news = incomings `HS.difference` olds
      writeIORef ps news

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createSqlitePool
        (sqlDatabase $ appDatabaseConf appSettings)
        (sqlPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applyng some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadAppSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadAppSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB

