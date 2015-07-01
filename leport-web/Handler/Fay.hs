{-# LANGUAGE RankNTypes, DeriveDataTypeable, StandaloneDeriving, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Fay where
import ClassyPrelude.Yesod
import Language.Haskell.Interpreter (loadModules)
import Fay.Convert (readFromFay)
import qualified Data.List as L
import Data.List (nub)
import System.IO.Temp
import Language.Haskell.Interpreter (setTopLevelModules)
import Import.NoFoundation
import Yesod.WebSockets
import Language.Haskell.Exts.QQ
import Language.Haskell.Exts               (Decl, ImportDecl (..))
import Language.Haskell.Exts               (ImportSpec (IVar), Module (..))
import Language.Haskell.Exts               (ModuleName (..), ModulePragma (..))
import Language.Haskell.Exts               (Name (Ident))
import Language.Haskell.Exts               (Namespace (NoNamespace))
import Language.Haskell.Exts               (ParseResult (..), Tool (GHC), app)
import Language.Haskell.Exts               (name, nameBind)
import Language.Haskell.Exts               (prettyPrint, strE)
import Language.Haskell.Exts               (var)
import Language.Haskell.Interpreter (InterpreterError(..))
import Language.Haskell.Interpreter (errMsg)
import Merger
import Language.Haskell.Exts (parseModule)
import HscTypes (HscEnv)
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Interpreter (runInterpreter)
import Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import Language.Haskell.Interpreter (interpret)
import Language.Haskell.Interpreter (as)
import qualified Test.QuickCheck as QC
import Language.Haskell.Exts (listE)
-- import Import.NoFoundation
import Language.Haskell.Interpreter (InterpreterT)
import qualified Control.Monad.Ghc as Ghc
import Language.Haskell.Interpreter (runGhc)
import Fay.Convert (showToFay)
import Data.Maybe (fromJust)
import Data.Aeson.Encode (encode)

deriving instance Typeable QC.Result


addDecl :: Decl -> Module -> Module
addDecl d (Module sl mn mps mws mes idls dls) = Module sl mn mps mws mes idls (d : dls)

handleFay :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => CommandHandler site
handleFay render command =
  case readFromFay command of
    Just (RunReport rid ans r) -> do
      return ()
      Just route <- getCurrentRoute
      renderURI  <- getUrlRender
      let uri = toWS $ renderURI route
      Report{..} <- runDB $ get404 (ReportKey $ toEnum rid)
      withModule render r reportSpec $ \test ->
        withModule render r ans $ \rslt -> 
          executeReport test rslt >>= \case
            Right () -> render r (Success uri)
            Left err -> render r $ Failure $ showError err
    Nothing -> invalidArgs ["Invalid arguments"]

showError :: (IsSequence c, Element c ~ Char) => InterpreterError -> [c]
showError (UnknownError err) = [pack err]
showError (WontCompile err)  = map (pack . errMsg) err
showError (NotAllowed err)   = [pack err]
showError (GhcException err) = [pack err]

toWS :: (EqSequence m, IsString m) => m -> m
toWS = replacePrefix "https://" "wss://" . replacePrefix "http://" "ws://"

replacePrefix :: EqSequence m => m -> m -> m -> m
replacePrefix from to str = maybe str (to++) $ stripPrefix from str

withModule :: (t1 -> Result a -> t) -> t1 -> Text -> (Module -> t) -> t
withModule render r src f =
  case parseModule $ unpack src of
    ParseFailed loc err -> render r $ Failure ["Parse error: " ++ tshow loc, pack err]
    ParseOk m -> f m

executeReport :: (MonadHandler m, MonadBaseControl IO m, Applicative m, MonadMask m, MonadIO m, Functor m)
              => Module -> Module -> m (Either InterpreterError ())
executeReport test ans
  =  withSystemTempFile "Main.hs" $ \fp h -> do
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
                                      IVar NoNamespace (name "stdArgs")]))] $ mergeModules "Check" ltes lans
  hPutStrLn h src
  liftIO $ hClose h
  eith <- runInterpreter $ do
    mapM_ (unsafeSetGhcOption . ("-trust " ++)) trusted
    mapM_ (unsafeSetGhcOption . ("-distrust " ++)) distrusted
    loadModules [fp]
    setTopLevelModules ["Check"]
    (,) <$> interpret "()" (as :: ()) <*> runGhc Ghc.getSession
  case eith of
    Right ((), sess0) -> do
      let loop _ [] = sendEvent Finished
          loop sess (prop:ps) = do
            let cmd = prettyPrint [hs|QC.quickCheckWithResult QC.stdArgs{chatty=False} $(var prop)|]
            r <- withSession sess $ join $ liftIO <$> interpret cmd (as :: IO QC.Result)
            case r of
              Right (a, sess') -> sendEvent (fromQCResult (dropProp prop) a) >> loop sess' ps
              Left err -> sendEvent $ Exception $ showError err
      webSockets $ loop sess0 propNs
      return $ Right ()
    Left l -> return $ Left l

dropProp :: Name -> String
dropProp = fromJust . stripPrefix "prop_" . prettyPrint

fromQCResult :: String -> QC.Result -> FayEvent
fromQCResult n QC.Success {..} =
  CheckResult (pack n) True (pack output)
fromQCResult n QC.GaveUp { .. } =
  CheckResult (pack n) False ("Timedout: " ++ pack output)
fromQCResult n QC.Failure {..} =
  CheckResult (pack n) False (pack $ unlines ["Failed: " ++ output, "reason: " ++ reason, "seed: " ++ show usedSeed])
fromQCResult n QC.NoExpectedFailure {..} =
  CheckResult (pack n) False (pack $ "Unexpected error!: " ++ output)

sendEvent :: (MonadIO m) => FayEvent -> WebSocketsT m ()
sendEvent = sendTextData . encode . fromJust . showToFay

withSession :: (MonadMask m, MonadIO m, Applicative m)
            => HscEnv -> InterpreterT m a -> m (Either InterpreterError (a, HscEnv))
withSession sess0 act = runInterpreter $ do
  runGhc $ Ghc.setSession sess0
  mapM_ (unsafeSetGhcOption . ("-trust " ++)) trusted
  mapM_ (unsafeSetGhcOption . ("-distrust " ++)) distrusted
  setTopLevelModules ["Check"]
  (,) <$> act <*> runGhc Ghc.getSession
  

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
  Module a b (nub $ ps' ++ ps) c d e f

addImports :: [ImportDecl] -> Module -> Module
addImports ims (Module a b c d e ims0 ds) =
  Module a b c d e (nub $ ims ++ ims0) ds
