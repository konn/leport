module Handler.Report where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getReportR :: ReportId -> Handler Html
getReportR rid = do
  Report {..} <- runDB $ get404 rid
  defaultLayout $ do
    addScriptEither . urlJqueryJs =<< getYesod 
    addScript $ StaticR js_bootstrap_min_js
    toWidget [julius|$("body").ready(function(){window['report_id'] = #{toJSON  rid};});|]
    $(widgetFile "report")
    $(fayFile "Report")

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

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing
