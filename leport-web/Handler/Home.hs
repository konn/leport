module Handler.Home where

import Handler.Fay
import Import

getHomeR :: Handler Html
getHomeR = do
    addHeader "Access-Control-Allow-Origin" "*"
    muser <- maybeAuth
    reports <- case muser of
      Nothing -> return []
      Just (Entity k _) ->
        runDB $ selectList [ReportOwnerId ==. k] [Desc ReportCreated]
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Home"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = redirect HomeR
