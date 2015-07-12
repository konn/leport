module Handler.Admin where

import Import
import Control.Monad.Random
import Settings.StaticFiles
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Yesod.Auth.HashDB (setPassword)

postRemoveUserR :: Handler Html
postRemoveUserR = do
  ((musr, remWid), remEnc) <- runFormPost . removeAccountForm =<< otherUsers
  case musr of
    FormSuccess usr -> do
      runDB $ deleteBy (UniqueUser $ userIdent usr)
      setInfo [shamlet|削除完了：#{userIdent usr} (#{userScreenName usr})|]
      redirect AdminR
    FormFailure err -> do
      setDanger [shamlet|削除できませんでした：#{concat err}|]
      redirect AdminR
    FormMissing -> do 
      setDanger [shamlet|削除できませんでした|]
      redirect AdminR
  

getAdminR :: Handler Html
getAdminR = do
  (addWid, addEnc) <- generateFormPost newAccountForm
  (remWid, remEnc) <- generateRemoveForm
  defaultLayout $ do
    setTitle "Administration"
    $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = redirect AdminR

otherUsers :: Handler [User]
otherUsers = do
  Entity _ usr <- requireAuth
  runDB (map entityVal <$> selectList [UserIdent !=. userIdent usr] [])

generateRemoveForm :: Handler (Widget, Enctype)
generateRemoveForm =
  generateFormPost . removeAccountForm =<< otherUsers

postUserR :: Handler Html
postUserR = do
  ((musr, addWid), addEnc) <- runFormPost newAccountForm
  (remWid, remEnc) <- generateRemoveForm
  case musr of
    FormMissing -> setDanger "Some data missing" >> redirect AdminR
    FormFailure err -> setDanger [shamlet|入力が不正です： #{unlines err}|] >> redirect AdminR
    FormSuccess usr0 -> do
      let pwLetters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
      tmpPass <- pack <$> replicateM 10 (uniform pwLetters)
      usr <- setPassword tmpPass usr0
      r <- runDB $ insertUnique usr
      case r of
        Nothing -> do
          setDanger "既に同名のユーザが存在しています"
          defaultLayout $ do
            setTitle "Administration"
            $(widgetFile "admin")
        Just _ -> do
          setInfo $ [shamlet|登録完了：#{userScreenName usr} (PASS: #{tmpPass})|]
          redirect AdminR

removeAccountForm :: [User] -> Form User
removeAccountForm usrs = renderBootstrap3 BootstrapBasicForm $
  areq (selectFieldList [(userIdent usr <> " (" <> userScreenName usr <> ")", usr) | usr <- usrs])
  "User to be removed" Nothing

newAccountForm :: Form User
newAccountForm = renderBootstrap3 BootstrapBasicForm $
  User <$> areq textField "User ID" Nothing
       <*> areq textField "Screen Name" Nothing
       <*> pure Nothing
       <*> areq (selectFieldList [("Normal" :: Text, Normal), ("Admin", Admin)]) "Access" (Just Normal)

