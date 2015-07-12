{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fno-warn-orphans #-}
module Handler.Report where

import Import
import Yesod.WebSockets (webSockets)
import Yesod.WebSockets (sendTextData, receiveData)
import Codec.Archive.Zip
import Language.Haskell.Exts.QQ
import Language.Haskell.Exts        (Decl, ImportDecl (..))
import Language.Haskell.Exts        (ImportSpec (IVar), Module (..))
import Language.Haskell.Exts        (ModuleName (..), ModulePragma (..))
import Language.Haskell.Exts        (Name (Ident))
import Language.Haskell.Exts        (Namespace (NoNamespace))
import Language.Haskell.Exts        (ParseResult (..), Tool (GHC), app)
import Language.Haskell.Exts        (name, nameBind)
import Language.Haskell.Exts        (prettyPrint, strE)
import Language.Haskell.Exts        (var, ParseMode(fixities))
import Language.Haskell.Interpreter (InterpreterError(..))
import Language.Haskell.Interpreter (errMsg)
import Merger
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Interpreter (runInterpreter)
import Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import Language.Haskell.Interpreter (interpret)
import Language.Haskell.Interpreter (as)
import qualified Test.QuickCheck as QC
import Language.Haskell.Exts (defaultParseMode)
import Language.Haskell.Exts (parseModuleWithMode)
import Yesod.WebSockets (WebSocketsT)
import Language.Haskell.Interpreter (loadModules)
import Language.Haskell.Interpreter (setTopLevelModules)
import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Control.Monad.Catch as ME
import Language.Haskell.Interpreter (InterpreterT)
import Control.Monad.Logger (monadLoggerLog)
import qualified Data.ByteString.Lazy as LBS
import System.IO.Temp (withSystemTempDirectory)
import System.IO (IOMode(ReadWriteMode))
import System.IO (openFile)
import qualified Language.Haskell.Exts as HSE
import Yesod.Form.Bootstrap3 (renderBootstrap3)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout(BootstrapBasicForm))

getReportR :: ReportId -> Handler Html
getReportR rid = do
  r@Report {..} <- runDB $ get404 rid
  webSockets $ rateReport r
  (wid, enc) <- generateFormPost $ updateForm $ Just reportSpec
  serveReportR rid r wid enc

serveReportR :: Key Report -> Report -> Widget -> Enctype -> HandlerT App IO Html
serveReportR rid Report{..} wid enc = do
  rates <- map entityVal <$> runDB (selectList [ RatingReportId ==. rid ] [])
  defaultLayout $ do
    setTitle $ toHtml reportTitle
    addScriptEither . urlJqueryJs =<< getYesod 
    addScript $ StaticR js_bootstrap_min_js
    addScriptRemote "//ajax.googleapis.com/ajax/libs/angularjs/1.2.0-rc.3/angular.min.js"
    -- $(fayFile "Report")
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

rateReport :: (MonadLogger m, MonadMask m, MonadBaseControl IO m, MonadHandler m)
           => Report -> WebSocketsT m ()
rateReport Report{..} =
  loop `finally` sendTextData Finished
  where
    loop = receiveData >>= \case
      Single input ->
        withModule reportSpec $ \test -> withModule input $ \rslt -> do
          $logDebug ("executing single report")
          executeReport test rslt
      Multiple -> do
        zipped <- receiveData
        $logDebug $ "Rec'ved size: " <> tshow (length zipped)
        $logDebug $ tshow $ LBS.last zipped
        case toArchiveOrFail zipped of
          Left err -> sendTextData $ Exception ["Zip archive invalid", pack err]
          Right arch -> do
            sendTextData $ Information $
              "Files: " : map pack (filesInArchive arch)

instance MonadLogger m => MonadLogger (InterpreterT m) where
  monadLoggerLog loc src lvl msg = lift $ monadLoggerLog loc src lvl msg

withFile' :: MonadBaseControl IO m => String -> (Handle -> m b) -> m b
withFile' fp = withAcquire (mkAcquire (openFile fp ReadWriteMode) hClose)

executeReport :: (MonadLogger m, MonadMask m, MonadBaseControl IO m, MonadHandler m)
              => Module -> Module -> WebSocketsT m ()
executeReport test ans
  = withSystemTempDirectory "tmp" $ \tdir ->
    let fp  = tdir ++ "/Check.hs"
        rfp = tdir ++ "/Main.hs"
    in withFile' fp $ \h -> withFile' rfp $ \hfp -> do
    $logDebug ("running: " <> pack fp)
    let propNs = extractProps test
        missing = map (Ident . drop 5.prettyPrint) propNs L.\\ (extractFunNames ans ++ extractFunNames test)
        missDec = map (\p -> nameBind noLoc p (app (var $ name "error") $ strE "Not Implemented")) missing
        lans = foldr addDecl ans missDec
        src = prettyPrint $ addPragmas [LanguagePragma noLoc [name "Safe"]
                                       ,OptionsPragma noLoc  (Just GHC) "-fpackage-trust"] $
              mergeModules "Check" test lans
    $logDebug $ "Check: " <> pack src
    hPutStrLn h src
    liftIO $ hClose h
    hPutStrLn hfp $ prettyPrint $ mainModule [hs|return ()|]
    liftIO $ hClose hfp
    $logDebug $ "Main: " <> pack (prettyPrint $ mainModule [hs|return ()|])
    $logDebug $ "written to: " ++ pack fp <> ", " <> pack rfp
    eith <- runInterpreter $ do
      mapM_ (unsafeSetGhcOption . ("-trust " ++)) trusted
      mapM_ (unsafeSetGhcOption . ("-distrust " ++)) distrusted
      $logDebug $ "Compiling: " <> pack fp <> ", " <> pack rfp
      loadModules [fp, rfp]
      $logDebug $ "Module Loaded."
      setTopLevelModules ["Main"]
      comp <- ME.try $ join $ liftIO <$> interpret "Main.main" (as :: IO ())
      case comp of
        Left err -> lift $ sendTextData (Exception $ showError err)
        Right () -> do
          $logDebug "successfully compiled."
          lift $ sendTextData $ Information ["Compilation successed."]
          $logDebug "booting..."
          loop propNs
    case eith of
      Right a -> return a
      Left err -> sendTextData $ Exception $ showError err
  where
    loop [] = return ()
    loop (prop:ps) = do
      $logDebug $ "checking: " <> (tshow prop)
      let cmd = prettyPrint [hs|QC.quickCheckWithResult QC.stdArgs{QC.chatty=False} $(var prop) `race` threadDelay (10*10^6) |]
      $logDebug $ "executing: " <> pack cmd
      r <- ME.try $ join $ liftIO <$> interpret cmd (as :: IO (Either QC.Result ()))
             -- `race` liftIO (threadDelay (10*10^(6 :: Int)))
      case r of
        Right (Left a)   -> lift (sendTextData $ fromQCResult (dropProp prop) a) >> loop ps
        Right (Right ()) -> lift $ sendTextData $
                            CheckResult (pack $ dropProp prop) False $
                            "Timeout (10secs)"
        Left err -> lift $ sendTextData $ Exception $ showError err

mainModule :: HSE.Exp -> Module
mainModule mbody = Module noLoc (ModuleName "Main") [] Nothing Nothing
                   [ImportDecl
                      noLoc (ModuleName "Check") False
                      False False Nothing (Just (ModuleName "C")) $ Just (True, [HSE.IAbs $ Ident "main"])
                   ,ImportDecl
                      noLoc (ModuleName "Control.Concurrent.Async") False
                      False False Nothing Nothing $ Just (False, [HSE.IAbs $ Ident "race"])
                   ,ImportDecl
                      noLoc (ModuleName "Control.Concurrent") False
                      False False Nothing Nothing $ Just (False, [HSE.IAbs $ Ident "threadDelay"])
                   ,ImportDecl noLoc (ModuleName "Test.QuickCheck") True False False Nothing
                          (Just (ModuleName "QC"))
                         (Just (False, [IVar NoNamespace (name "quickCheckWithResult"),
                                        IVar NoNamespace (name "property"),
                                        IVar NoNamespace (name "chatty"),
                                        IVar NoNamespace (name "stdArgs")]))
                   ,ImportDecl noLoc (ModuleName "Test.QuickCheck") False False False Nothing
                          Nothing
                         (Just (False, [IVar NoNamespace (name "Result")]))
                   ]
                   [[dec|main :: IO ()|],
                    [dec|main = $(mbody)|]]


showError :: (IsSequence c, Element c ~ Char) => InterpreterError -> [c]
showError (UnknownError err) = [pack err]
showError (WontCompile err)  = map (pack . errMsg) err
showError (NotAllowed err)   = [pack err]
showError (GhcException err) = [pack err]

dropProp :: Name -> String
dropProp = fromJust . stripPrefix "prop_" . prettyPrint


fromQCResult :: String -> QC.Result -> ReportEvent
fromQCResult n QC.Success {..} =
  CheckResult (pack n) True (pack output)
fromQCResult n QC.GaveUp { .. } =
  CheckResult (pack n) False ("Timedout: " ++ pack output)
fromQCResult n QC.Failure {..} =
  CheckResult (pack n) False (pack $ unlines ["Failed: " ++ output, "reason: " ++ reason, "seed: " ++ show usedSeed])
fromQCResult n QC.NoExpectedFailure {..} =
  CheckResult (pack n) False (pack $ "Unexpected error!: " ++ output)

withModule :: MonadIO m
           => Text
           -> (Module -> WebSocketsT m ())
           -> WebSocketsT m ()
withModule src f =
  case parseModuleWithMode defaultParseMode {fixities = Nothing} $ unpack src of
    ParseFailed loc err -> sendTextData $ Exception ["Parse error: " ++ tshow loc, pack err]
    ParseOk m -> f m

addDecl :: Decl -> Module -> Module
addDecl d (Module sl mn mps mws mes idls dls) = Module sl mn mps mws mes idls (d : dls)

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

extractProps :: Module -> [Name]
extractProps =
  L.nub . filter (("prop_" `isPrefixOf`) . prettyPrint) . extractFunNames

trusted :: [String]
trusted = ["base", "containers", "QuickCheck","fgl","array"
          ,"async","binary","attoparsec","case-insensitive"
          ,"deepseq","hashable","haskeline"
          ,"hoopl","html","mtl","integer-gmp"
          ,"old-locale","old-time","time","parallel","parsec"
          ,"pretty","primitive","random","regex-base","regex-compat"
          ,"regex-posix","rts","split","stm","syb","template-haskell"
          ,"text","transformers","unordered-containers","vector","xhtml"]

distrusted :: [String]
distrusted = ["process", "HTTP", "Cabal", "HUnit"
             ,"directory", "filepath", "ghc", "hint","ghc-prim"
             ,"network","terminfo","unix","zlib"]

addPragmas :: [ModulePragma] -> Module -> Module
addPragmas ps' (Module a b ps c d e f) =
  Module a b (L.nub $ ps' ++ ps) c d e f

addImports :: [ImportDecl] -> Module -> Module
addImports ims (Module a b c d e ims0 ds) =
  Module a b c d e (L.nub $ ims ++ ims0) ds

updateForm :: Maybe Text -> Form Text
updateForm msrc =
  renderBootstrap3 BootstrapBasicForm $
  unTextarea <$> areq textareaField "" {fsId = Just "spec"} (Textarea <$> msrc)
