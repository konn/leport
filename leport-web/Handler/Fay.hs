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
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Interpreter (runInterpreter)
import Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import Language.Haskell.Interpreter (interpret)
import Language.Haskell.Interpreter (as)
import qualified Test.QuickCheck as QC
import Language.Haskell.Exts (listE)

deriving instance Typeable QC.Result


addDecl :: Decl -> Module -> Module
addDecl d (Module sl mn mps mws mes idls dls) = Module sl mn mps mws mes idls (d : dls)

handleFay :: CommandHandler app
handleFay render command = do
    case readFromFay command of
      {-
      Just (RegisterReport rname note src r) ->
        registerReport rname note src
      -}
      Just (RunReport spec ans r) -> 
        withModule render r spec $ \test ->
        withModule render r ans $ \rslt ->
          executeReport test rslt >>= \case
            Right result -> render r (Success $ tshow result)
            Left  (UnknownError err) -> render r $ Failure [pack err]
            Left  (WontCompile err)  -> render r $ Failure  (map (pack . errMsg) err)
            Left  (NotAllowed err)   -> render r $ Failure $ [pack err]
            Left  (GhcException err) -> render r $ Failure $ [pack err]
      Nothing -> invalidArgs ["Invalid arguments"]

withModule :: (t1 -> Result a -> t) -> t1 -> Text -> (Module -> t) -> t
withModule render r src f =
  case parseModule $ unpack src of
    ParseFailed loc err -> render r $ Failure ["Parse error: " ++ tshow loc, pack err]
    ParseOk m -> f m

executeReport :: (MonadMask m, MonadIO m, Functor m)
              => Module -> Module -> m (Either InterpreterError [(String, QC.Result)])
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
  runInterpreter $ do
    mapM_ (unsafeSetGhcOption . ("-trust " ++)) trusted
    mapM_ (unsafeSetGhcOption . ("-distrust " ++)) distrusted
    loadModules [fp]
    setTopLevelModules ["Check"]
    join $ liftIO <$> interpret "main" (as :: IO [(String, QC.Result)])

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
