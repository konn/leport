module Handler.Settings where

import Import
import Settings.StaticFiles
import Yesod.Auth.HashDB     (setPassword)
import Yesod.Auth.HashDB     (validatePass)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

-- This is a handler function for the GET request method on the SettingsR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getSettingsR :: Handler Html
getSettingsR = do
  (pwForm, pwEnc) <- generateFormPost chpasswdForm
  defaultLayout $ do
    setTitle "User Settings"
    $(widgetFile "settings")

postSettingsR :: Handler Html
postSettingsR = do
  ((mpw, _pwForm), _pwEnc) <- runFormPost chpasswdForm
  Entity uid usr  <- requireAuth
  case mpw of
    FormMissing -> setDanger "Form missing!"
    FormFailure err -> setDanger [shamlet|変更失敗：#{tshow err}|]
    FormSuccess (old, new) ->
      case validatePass usr old of
        Just False -> setDanger "Password incorrect!"
        _ -> runDB $ replace uid =<< setPassword new usr
  redirect SettingsR


chpasswdForm :: Form (Text, Text)
chpasswdForm =
  renderBootstrap3 BootstrapBasicForm $
  (,) <$> areq passwordField "Current Password" Nothing
      <*> areq passwordField "New Password"     Nothing
