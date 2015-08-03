{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fno-warn-orphans #-}
module Handler.Report where

import Import
import Yesod.WebSockets (webSockets)
import Yesod.WebSockets (sendTextData, receiveData)
import Codec.Archive.Zip
import Yesod.WebSockets (WebSocketsT)
import Yesod.Form.Jquery
import qualified Data.ByteString.Lazy as LBS
import Yesod.Form.Bootstrap3 (renderBootstrap3)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout(BootstrapBasicForm))
import Control.Monad.Loops (whileJust_)

getReportR :: ReportId -> Handler Html
getReportR rid = do
  r@Report {..} <- runDB $ get404 rid
  webSockets $ rateReport r
  (wid, enc) <- generateFormPost $ updateForm $ Just reportSpec
  serveReportR rid r wid enc

serveReportR :: Key Report -> Report -> Widget -> Enctype -> HandlerT App IO Html
serveReportR rid Report{..} wid enc = do
  rates <- map entityVal <$> runDB (selectList [ RatingReportId ==. rid ] [])
  wsAddr <- appWSAddress . appSettings <$> getYesod
  root   <- appRoot . appSettings <$> getYesod
  defaultLayout $ do
    setTitle $ toHtml reportTitle
    addScriptEither . urlJqueryJs =<< getYesod 
    addScript $ StaticR js_bootstrap_min_js
    addScriptRemote "//ajax.googleapis.com/ajax/libs/angularjs/1.2.0-rc.3/angular.min.js"
    $(widgetFile "report")


postReportR :: ReportId -> Handler Html
postReportR rid = do
  r@Report {..} <- runDB $ get404 rid
  rates <- map entityVal <$> runDB (selectList [ RatingReportId ==. rid ] [])
  ((src, wid), enc) <- runFormPost $ updateForm Nothing
  ans <- withModuleForm id src
  case ans of
    Left err -> setDanger err >> serveReportR rid r wid enc
    Right (src', _m, tests) -> do
      runDB $ do
        let deads = filter ((`onotElem` map pack tests) . ratingFunction) rates
            newbies = filter (`onotElem` map ratingFunction rates) $ map pack tests
            newper = sum (map ratingRate deads) `div` length newbies
        update rid [ReportSpec =. src']
        mapM_ (deleteBy . UniqueRating rid . ratingFunction) deads
        mapM_ (\a -> void $ insertUnique $ Rating a rid newper) newbies
      setInfo "Updated report"
      redirect $ ReportR rid

rateReport :: (HandlerSite m ~ App, MonadLogger m, MonadMask m, MonadBaseControl IO m, MonadHandler m)
           => Report -> WebSocketsT m ()
rateReport Report{..} =
  loop `finally` sendTextData Finished
  where
    loop = receiveData >>= \case
      Single input -> do
        $logDebug ("executing single report")
        executeReport reportSpec input
      Multiple -> do
        zipped <- receiveData
        $logDebug $ "Rec'ved size: " <> tshow (length zipped)
        $logDebug $ tshow $ LBS.last zipped
        case toArchiveOrFail zipped of
          Left err -> sendTextData $ Exception ["Zip archive invalid", pack err]
          Right arch -> do
            sendTextData $ Information $
              "Files: " : map pack (filesInArchive arch)

executeReport :: (HandlerSite m ~ App,
                  MonadLogger m, MonadMask m, MonadBaseControl IO m, MonadHandler m)
              => Text -> Text -> WebSocketsT m ()
executeReport test ans = do
  q  <- liftIO $ newTBMQueueIO 10
  qs <- appEvalQueue <$> getYesod
  atomically $ writeTBMQueue qs (test, ans, q)
  whileJust_ (atomically $ readTBMQueue q) sendTextData

putReportR :: ReportId -> Handler Html
putReportR = redirect . ReportR

postDeleteReportR :: ReportId -> Handler Html
postDeleteReportR = deleteReportR

deleteReportR :: ReportId -> Handler Html
deleteReportR rid = join $ runDB $ do
  Report{..} <- get404 rid
  Entity uid usr <- lift requireAuth
  if userAccess usr == Admin || uid == reportOwnerId
    then do
      mapM_ (delete . entityKey) =<< selectList [RatingReportId ==. rid] []
      delete rid
      return $ do
        setSuccess "Report Deleted."
        redirect HomeR
    else return $ do
      setDanger "You have not permitted to delete report!"
      redirect $ ReportR rid

updateForm :: Maybe Text -> Form Text
updateForm msrc =
  renderBootstrap3 BootstrapBasicForm $
  unTextarea <$> areq textareaField "" {fsId = Just "spec"} (Textarea <$> msrc)
