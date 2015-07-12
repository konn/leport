{-# LANGUAGE PatternGuards #-}
module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.Message   (AuthMessage (InvalidLogin))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Auth.HashDB (authHashDB)
import Handler.Fay
import Language.Haskell.Exts (Module)
import Language.Haskell.Exts (ParseResult (..))
import Language.Haskell.Exts (SrcLoc (..))
import Language.Haskell.Exts (prettyPrint)
import Merger
import Language.Haskell.Exts (defaultParseMode,fixities)
import Language.Haskell.Exts (parseModuleWithMode)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        mauth <- maybeAuth

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addScript $ StaticR js_codemirror_js
            addScript $ StaticR js_codemirror_haskell_js
            addScript $ StaticR js_codemirror_addon_closebrackets_js
            addScript $ StaticR js_codemirror_addon_foldcode_js
            addScript $ StaticR js_codemirror_addon_matchbrackets_js
            addScript $ StaticR js_codemirror_addon_show_hint_js
            addStylesheet $ StaticR css_codemirror_css
            addStylesheet $ StaticR css_show_hint_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized

    isAuthorized SettingsR _ = requireNormal
      
    isAuthorized AdminR _ = requireAdmin

    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

requireNormal :: (YesodAuthPersist master, PersistEntity (AuthEntity master), Typeable (AuthEntity master), AuthId master ~ Key (AuthEntity master)) => HandlerT master IO AuthResult
requireNormal = do
    ma <- maybeAuth
    case ma of
      Nothing -> return $ Unauthorized "You must be logged in."
      Just _  -> return $ Authorized

requireAdmin :: (YesodAuthPersist master, AuthEntity master ~ User, AuthId master ~ Key User) => HandlerT master IO AuthResult
requireAdmin = do
  ma <- maybeAuth
  case ma of
    Nothing -> return $ Unauthorized "You must be logged in."
    Just (Entity _ usr)
      | Admin <- userAccess usr -> return Authorized
    _ -> return $ Unauthorized "Your have to be admin."

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = False

    authenticate creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        return $ case x of
            Just (Entity uid _) -> Authenticated uid
            Nothing -> UserError InvalidLogin

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authHashDB (Just . UniqueUser) ]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

instance YesodJquery App where
  urlJqueryJs = const $ Right "//ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js"

instance YesodFay App where
  yesodFayCommand = handleFay
  fayRoute = FaySiteR

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

setDanger :: (MonadHandler m) => Html -> m ()
setDanger msg = setMessage
                [shamlet|<div .alert .alert-danger .alert-dismissible role="alert">
                           <button type="button" .close data-dismiss="alert" aria-label="Close">
                             <span aria-hidden="true">&times;
                           <span .glyphicon .glyphicon-exclamation-sign aria-hidden="true"></span>
                           <span .sr-only>
                              Error:
                           #{msg}|]

setSuccess :: (MonadHandler m) => Html -> m ()
setSuccess msg = setMessage
                [shamlet|<div .alert .alert-success .alert-dismissible role="alert">
                           <button type="button" .close data-dismiss="alert" aria-label="Close">
                             <span aria-hidden="true">&times;
                           <span .glyphicon .glyphicon-ok-sign aria-hidden="true"></span>
                           <span .sr-only>
                              Success:
                           #{msg}|]

setInfo :: (MonadHandler m) => Html -> m ()
setInfo msg = setMessage
                [shamlet|<div .alert-dismissible .alert .alert-info role="alert">
                           <button type="button" .close data-dismiss="alert" aria-label="Close">
                             <span aria-hidden="true">&times;
                           <span .glyphicon .glyphicon-info-sign aria-hidden="true"></span>
                           <span .sr-only>
                              Info:
                           #{msg}|]


withModuleForm :: (t -> Text) -> FormResult t
               -> Handler (Either Html (t, Module, [String]))
withModuleForm f res = 
  case res of
    FormMissing -> return $ Left "データがありません。"
    FormFailure err -> return $ Left [shamlet|入力が不正です： #{unlines err}|]
    FormSuccess rep ->
      let spec = f rep
      in 
      case parseModuleWithMode defaultParseMode { fixities = Nothing } $ unpack spec of
        ParseFailed (SrcLoc _n row col) err ->
          return $ Left $ toHtml $ mconcat ["仕様がパーズ出来ません：", tshow row
                                           ,"行", tshow col
                                           ,"文字目: ",
                                            pack err]
        ParseOk m ->
          let props = mapMaybe (stripPrefix "prop_" . prettyPrint) $ extractFunNames m
          in return $ Right (rep, m, props)
