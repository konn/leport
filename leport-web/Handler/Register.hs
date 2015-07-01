{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Handler.Register where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Merger
import Language.Haskell.Exts (parseModule)
import Language.Haskell.Exts (ParseResult(..))
import Language.Haskell.Exts (prettyPrint)
import Data.List (nub)
import Language.Haskell.Exts (SrcLoc(..))

postRegisterR :: Handler Html
postRegisterR = do
  uid <- requireAuthId
  ((res, wid), enc) <- runFormPost $ registerForm uid
  case res of
    FormMissing -> do
      setDanger "データがありません。"
      serveRegister wid enc
    FormFailure err -> do
      setDanger [shamlet|入力が不正です： #{unlines err}|]
      serveRegister wid enc
    FormSuccess rep@Report{reportSpec=spec} -> 
      case parseModule $ unpack spec of
        ParseFailed (SrcLoc _n row col) err -> do
          setDanger $ toHtml $ "仕様がパーズ出来ません：" ++ tshow row ++ "行" ++ tshow col ++ "文字目: " ++ pack err
          serveRegister wid enc
        ParseOk m -> do
          ans <- runDB $ do
            mk <- insertUnique rep
            case mk of
              Nothing -> return Nothing
              Just k  -> do
                let tests = nub $ mapMaybe (stripPrefix "prop_" . prettyPrint) $ extractFunNames m
                    per   = 100 `div` length tests
                forM_ tests $ \fun ->
                  insert_ $ Rating (pack fun) k per
                return $ Just k
          case ans of
            Just k -> setSuccess "Report Created." >> redirect (ReportR k)
            Nothing -> do
              setDanger "同名のレポートが既に存在しています。"
              serveRegister wid enc

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

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing
