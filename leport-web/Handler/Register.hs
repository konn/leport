{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Handler.Register where

import Data.List             (nub)
import Import
import Language.Haskell.Exts (prettyPrint)
import Merger
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Yesod.Form.Jquery

postRegisterR :: Handler Html
postRegisterR = do
  uid <- requireAuthId
  ((res, wid), enc) <- runFormPost $ registerForm uid
  mres <- withModuleForm reportSpec res
  case mres of
    Left err -> setDanger err >> serveRegister wid enc
    Right (rep, _m, tests) -> do
      ans <- runDB $ do
        mk <- insertUnique rep
        case mk of
          Nothing -> return Nothing
          Just k  -> do
            let per   = 100 `div` length tests
            forM_ tests $ \fun ->
              insert_ $ Rating (pack fun) k per
            return $ Just k
      case ans of
        Just k -> setSuccess "Report Created." >> redirect (ReportR k)
        Nothing -> setDanger "同名のレポートが既に存在しています。" >> redirect RegisterR

getRegisterR :: Handler Html
getRegisterR = do
  uid <- requireAuthId
  (wid, enc) <- generateFormPost $ registerForm uid
  serveRegister wid enc

serveRegister :: Widget -> Enctype -> HandlerT App IO Html
serveRegister widget enctype = defaultLayout $ do
  setTitle "新規レポート"
  addScriptEither . urlJqueryJs =<< getYesod
  $(widgetFile "register")


registerForm :: Key User -> Form Report
registerForm uid = renderBootstrap3 BootstrapBasicForm $
  Report <$> areq textField "Title" {fsId = Just "title"} (Just "New Report")
         <*> lift (liftIO getCurrentTime)
         <*> (unTextarea <$> areq textareaField "Spec" {fsId = Just "spec"} Nothing)
         <*> (maybe "" unTextarea <$> aopt textareaField "Note" {fsAttrs = [("class", "form-control"),("rows", "3")]} Nothing)
         <*> pure uid
