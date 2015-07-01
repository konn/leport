module Handler.Admin where

import Import
import Settings.StaticFiles
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

getAdminR :: Handler Html
getAdminR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getAdminR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Administration"
        $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postAdminR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    redirect AdminR

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing
