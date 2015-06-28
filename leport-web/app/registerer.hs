{-# LANGUAGE ExtendedDefaultRules, NoImplicitPrelude, OverloadedStrings #-}
module Main where
import           Control.Monad.Logger    (runStdoutLoggingT)
import           Crypto.PasswordStore    (makePassword)
import qualified Data.Text.Encoding      as T
import           Database.Persist.Sqlite (sqlDatabase, withSqliteConn)
import           Import
import           Prelude                 (putChar)
import           System.IO               (hGetEcho)
import           System.IO               (hSetEcho)
import           System.IO               (hFlush)
import           Types
import           Yesod.Auth.HashDB       (defaultStrength)

default (String)

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

insertUser :: MonadIO m => (Text, Text, Text, Access) -> ReaderT SqlBackend m ()
insertUser (ident, sName, passwd, access) =
  insert_ $ User ident sName (Just passwd) access

prompt :: Text -> IO ()
prompt s = putStr (s ++ ": ") >> hFlush stdout

main :: IO ()
main = do
  prompt "Enter ID"
  u <- getLine
  prompt "Enter Screen Name"
  s <- getLine
  prompt "Enter Pass"
  p <- withEcho False getLine
  putChar '\n'
  prompt "Enter Roll"
  a0 <- getLine :: IO String
  let a = if  toLower a0 == "admin" then Admin else Normal
  settings <- loadAppSettingsArgs [configSettingsYmlValue] useEnv
  pw <- makePassword p defaultStrength
  runStdoutLoggingT $
    withSqliteConn
      (sqlDatabase $ appDatabaseConf settings) $ \conn -> flip runReaderT conn $ do
        runMigration migrateAll
        insertUser (u,s,T.decodeUtf8 pw,a)
