module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (BootstrapBasicForm))
import Yesod.Form.Bootstrap3 (renderBootstrap3)

getHomeR :: Handler Html
getHomeR = do
    addHeader "Access-Control-Allow-Origin" "*"
    muser <- maybeAuth
    reports <- case muser of
      Nothing -> return []
      Just (Entity k _) ->
        runDB $ selectList [] [Desc ReportCreated]
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Home"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = redirect HomeR
