{-# LANGUAGE ScopedTypeVariables #-}
module RPC (evaluateReport, remoteTable, evaluateReport__static,
            evaluateReport__sdict) where
import Types
import Lens
import Merger
import Orphans ()

import ClassyPrelude.Yesod
import Control.Distributed.Process (Process, RemoteTable, SendPort)
import Control.Distributed.Process (expect, sendChan)
import Control.Distributed.Process.Closure (remotable)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Lens ((&), (.~))
import qualified Control.Monad.Catch as ME
import Data.Generics
import qualified Data.List as L
import Data.Maybe (fromJust)
import Language.Haskell.Exts (Decl, ImportDecl (..), ImportSpec (IVar))
import Language.Haskell.Exts (Module (..), ModuleName (..))
import Language.Haskell.Exts (ModulePragma (..), Name (Ident), Namespace (..))
import Language.Haskell.Exts (Exp(..), ParseResult (..), Tool (GHC), app)
import Language.Haskell.Exts (name, nameBind, strE, var, ParseMode(fixities))
import Language.Haskell.Exts (defaultParseMode, parseModuleWithMode, prettyPrint)
import qualified Language.Haskell.Exts as HSE
import Language.Haskell.Exts.QQ     (hs, dec)
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Interpreter (InterpreterError(..), as)
import Language.Haskell.Interpreter (errMsg, interpret, loadModules)
import Language.Haskell.Interpreter (runInterpreter, setTopLevelModules)
import Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import System.IO (IOMode(ReadWriteMode), openFile)
import System.IO.Temp (withSystemTempDirectory)
import qualified Test.QuickCheck as QC
import Control.Distributed.Process (ProcessId)
import Control.Distributed.Process (getSelfPid)
import Control.Distributed.Process (send)

withFile' :: MonadBaseControl IO m => String -> (Handle -> m b) -> m b
withFile' fp = withAcquire (mkAcquire (openFile fp ReadWriteMode) hClose)

fromQCResult :: String -> QC.Result -> ReportEvent
fromQCResult n QC.Success {..} =
  CheckResult (pack n) True (pack output)
fromQCResult n QC.GaveUp { .. } =
  CheckResult (pack n) False ("Timedout: " ++ pack output)
fromQCResult n QC.Failure {..} =
  CheckResult (pack n) False (pack $ unlines ["Failed: " ++ output, "reason: " ++ reason, "seed: " ++ show usedSeed])
fromQCResult n QC.NoExpectedFailure {..} =
  CheckResult (pack n) False (pack $ "Unexpected error!: " ++ output)

withModule :: SendPort ReportEvent -> String -> (Module -> Process ()) -> Process ()
withModule port src f =
  case parseModuleWithMode defaultParseMode {fixities = Just $ myFixities} src of
    ParseFailed loc err -> sendChan port $ Exception ["Parse error: " ++ tshow loc, pack err]
    ParseOk m -> f m

myFixities :: [HSE.Fixity]
myFixities = HSE.preludeFixities
             ++ HSE.infixr_ 0 ["==>"]
             ++ HSE.infixr_ 1 [".&&.", ".||."]

extractProps :: Module -> [Name]
extractProps =
  L.nub . filter (("prop_" `isPrefixOf`) . prettyPrint) . extractFunNames

addPragmas :: [ModulePragma] -> Module -> Module
addPragmas ps' (Module a b ps c d e f) =
  Module a b (L.nub $ ps' ++ ps) c d e f


evaluateReport :: ProcessId -> Process ()
evaluateReport queue = forever $ do
  send queue =<< getSelfPid
  RatingSettings packdbs trusted distrusted specSrc input chan <- expect
  withSystemTempDirectory "tmp" $ \tdir ->
    let fp  = tdir ++ "/Check.hs"
        rfp = tdir ++ "/Main.hs"
    in withFile' fp $ \h -> withFile' rfp $ \hfp -> do
    $logDebug ("running: " <> pack fp)
    withModule chan specSrc $ \test -> withModule chan input $ \ans -> do
      let propNs = extractProps test
          funs = map (Ident . drop 5.prettyPrint) propNs
          missing = funs L.\\ (extractFunNames ans ++ extractFunNames test)
          missDec = map (\p -> nameBind noLoc p (app (var $ name "error") $ strE "Not Implemented")) missing
          lans = foldr addDecl ans missDec
          src = prettyPrint $
                (addPragmas [LanguagePragma noLoc [name "Safe"]
                           ,OptionsPragma noLoc  (Just GHC) "-fpackage-trust"] $
                lans) & _ModuleName .~ ModuleName "Check"
                      & _ExportSpecs .~ Nothing
          spec = mergeModules "Main" (mainModule [hs|return ()|]) $
                 everywhere (mkT $ foldr (.) id $ map qualifying funs)  test
      $logDebug $ "Check: " <> pack src
      hPutStrLn h src
      liftIO $ hClose h
      hPutStrLn hfp $ prettyPrint spec
      liftIO $ hClose hfp
      $logDebug $ "Main: " <> pack (prettyPrint $ spec)
      $logDebug $ "written to: " ++ pack fp <> ", " <> pack rfp
      eith <- runInterpreter $ do
        mapM_ (unsafeSetGhcOption . ("-trust " ++)) trusted
        mapM_ (unsafeSetGhcOption . ("-distrust " ++)) distrusted
        mapM_ (unsafeSetGhcOption . ("-package-db "++)) packdbs
        lift $ $logDebug $ "Compiling: " <> pack fp <> ", " <> pack rfp
        loadModules [fp, rfp]
        lift $ $logDebug $ "Module Loaded."
        setTopLevelModules ["Main"]
        comp <- ME.try $ join $ liftIO <$> interpret "Main.main" (as :: IO ())
        case comp of
          Left err -> lift $ sendChan chan $ Exception $ showError err
          Right () -> do
            $logDebug "successfully compiled."
            lift $ sendChan chan $ Information ["Compilation successed."]
            $logDebug "booting..."
            loop chan propNs
      case eith of
        Right a -> a <$ sendChan chan Finished
        Left err -> sendChan chan $ Exception $ showError err
  where
    loop _chan [] = return ()
    loop chan (prop:ps) = do
      $logDebug $ "checking: " <> (tshow prop)
      let cmd = prettyPrint [hs|QC.quickCheckWithResult QC.stdArgs{QC.chatty=False} $(var prop) `race` threadDelay (10*10^6) |]
      $logDebug $ "executing: " <> pack cmd
      r <- ME.try $ join $ liftIO <$> interpret cmd (as :: IO (Either QC.Result ()))
             -- `race` liftIO (threadDelay (10*10^(6 :: Int)))
      case r of
        Right (Left a)   -> lift (sendChan chan $ fromQCResult (dropProp prop) a) >> loop chan ps
        Right (Right ()) -> do
          lift $ sendChan chan $
            CheckResult (pack $ dropProp prop) False $
            "Timeout (10secs)"
          loop chan ps
        Left err -> lift $ sendChan chan $ Exception $ showError err

showError :: (IsSequence c, Element c ~ Char) => InterpreterError -> [c]
showError (UnknownError err) = [pack err]
showError (WontCompile err)  = map (pack . errMsg) err
showError (NotAllowed err)   = [pack err]
showError (GhcException err) = [pack err]

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
                          Nothing
                   ,ImportDecl noLoc (ModuleName "Test.QuickCheck.Exception") True False False Nothing
                          (Just (ModuleName "QC"))
                          Nothing
                   ,ImportDecl noLoc (ModuleName "Test.QuickCheck") False False False Nothing
                          Nothing
                         (Just (False, [IVar NoNamespace (name "Result")]))
                   ]
                   [[dec|main :: IO ()|],
                    [dec|main = $(mbody)|]]

qualifying :: Name -> HSE.Exp -> HSE.Exp
qualifying n (Var (HSE.UnQual m)) | n == m = HSE.qvar (ModuleName "C") n
qualifying _ e = e


addDecl :: Decl -> Module -> Module
addDecl d (Module sl mn mps mws mes idls dls) = Module sl mn mps mws mes idls (d : dls)

dropProp :: Name -> String
dropProp = fromJust . stripPrefix "prop_" . prettyPrint

remotable ['evaluateReport]

remoteTable :: RemoteTable
remoteTable = RPC.__remoteTable initRemoteTable
