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
import Language.Haskell.Exts (listE)
import Language.Haskell.Exts (defaultParseMode)
import Language.Haskell.Exts (parseModuleWithMode)
import Yesod.WebSockets (WebSocketsT)
import System.IO.Temp (withSystemTempFile)
import Language.Haskell.Interpreter (loadModules)
import Language.Haskell.Interpreter (setTopLevelModules)
import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Control.Monad.Catch as ME
import Language.Haskell.Interpreter (InterpreterT)
import Control.Monad.Logger (monadLoggerLog)
import qualified Data.ByteString.Lazy as LBS

getReportR :: ReportId -> Handler Html
getReportR rid = do
  r@Report {..} <- runDB $ get404 rid
  webSockets $ rateReport r
  defaultLayout $ do
    setTitle $ toHtml reportTitle
    addScriptEither . urlJqueryJs =<< getYesod 
    addScript $ StaticR js_bootstrap_min_js
    -- $(fayFile "Report")
    $(widgetFile "report")

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

executeReport :: (MonadLogger m, MonadMask m, MonadBaseControl IO m, MonadHandler m)
              => Module -> Module -> WebSocketsT m ()
executeReport test ans
  =  withSystemTempFile "Main.hs" $ \fp h -> do
  $logDebug ("running: " <> pack fp)
  let propNs = extractProps test
      props = listE $ map (app [hs|QC.property|] . var) propNs
      funcs = listE $ map strE $ mapMaybe (stripPrefix "prop_" . prettyPrint) propNs
      ma = [dec|main = return . zip $funcs =<< mapM (QC.quickCheckWithResult QC.stdArgs{chatty=False}) $(props)|]
      missing = map (Ident . drop 5.prettyPrint) propNs L.\\ (extractFunNames ans ++ extractFunNames test)
      missDec = map (\p -> nameBind noLoc p (app (var $ name "error") $ strE "Not Implemented")) missing
      ltes = addDecl ma test
      lans = foldr addDecl ans missDec
      src = prettyPrint $ addPragmas [LanguagePragma noLoc [name "Safe"]
                                     ,OptionsPragma noLoc  (Just GHC) "-fpackage-trust"] $
            addImports [ImportDecl noLoc (ModuleName "Test.QuickCheck") True False False Nothing
                        (Just (ModuleName "QC"))
                       (Just (False, [IVar NoNamespace (name "quickCheckWithResult"),
                                      IVar NoNamespace (name "property"),
                                      IVar NoNamespace (name "chatty"),
                                      IVar NoNamespace (name "stdArgs")]))] $ mergeModules "Main" ltes lans
  hPutStrLn h src
  liftIO $ hClose h
  $logDebug $ "written to: " ++ pack fp
  void $ runInterpreter $ do
    mapM_ (unsafeSetGhcOption . ("-trust " ++)) trusted
    mapM_ (unsafeSetGhcOption . ("-distrust " ++)) distrusted
    $logDebug $ "Compiling: " <> pack fp
    loadModules [fp]
    $logDebug $ "Module Loaded."
    setTopLevelModules ["Main"]
    comp <- ME.try $ interpret "()" (as :: ())
    case comp of
      Left err -> lift $ sendTextData (Exception $ showError err)
      Right () -> do
        $logDebug "successfully compiled."
        lift $ sendTextData $ Information ["Compilation successed."]
        $logDebug "booting..."
        loop propNs
  where
    loop [] = return ()
    loop (prop:ps) = do
      $logDebug $ "checking: " <> (tshow prop)
      let cmd = prettyPrint [hs|QC.quickCheckWithResult QC.stdArgs{chatty=False} $(var prop)|]
      r <- ME.try $ join $ liftIO <$> interpret cmd (as :: IO QC.Result)
      case r of
        Right a -> lift (sendTextData $ fromQCResult (dropProp prop) a) >> loop ps
        Left err -> lift $ sendTextData $ Exception $ showError err


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
distrusted = ["process", "HTTP", "Cabal", "HUnit", "OpenGL"
             ,"directory", "filepath", "ghc", "hint","ghc-prim"
             ,"network","terminfo","unix","zlib"]

addPragmas :: [ModulePragma] -> Module -> Module
addPragmas ps' (Module a b ps c d e f) =
  Module a b (L.nub $ ps' ++ ps) c d e f

addImports :: [ImportDecl] -> Module -> Module
addImports ims (Module a b c d e ims0 ds) =
  Module a b c d e (L.nub $ ims ++ ims0) ds
