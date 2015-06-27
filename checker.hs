{-# LANGUAGE NamedFieldPuns, OverloadedStrings, QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
module Main where
import Merger (extractFunNames, mergeModules)

import Control.Applicative                 ((<$>), (<*>))
import Control.Monad                       (join)
import Data.List                           (isPrefixOf, nub, (\\))
import Data.Monoid                         ((<>))
import Language.Haskell.Exts               (Decl, ImportDecl (..))
import Language.Haskell.Exts               (ImportSpec (IVar), Module (..))
import Language.Haskell.Exts               (ModuleName (..), ModulePragma (..))
import Language.Haskell.Exts               (Name (Ident))
import Language.Haskell.Exts               (Namespace (NoNamespace))
import Language.Haskell.Exts               (ParseResult (..), Tool (GHC), app)
import Language.Haskell.Exts               (doE, name, nameBind, parseFile)
import Language.Haskell.Exts               (prettyPrint, qualStmt, strE, tuple)
import Language.Haskell.Exts               (var)
import Language.Haskell.Exts               (qvar)
import Language.Haskell.Exts.SrcLoc        (noLoc)
import Language.Haskell.Interpreter        (InterpreterError (..), as)
import Language.Haskell.Interpreter        (interpret, liftIO, loadModules)
import Language.Haskell.Interpreter        (runInterpreter, setTopLevelModules)
import Language.Haskell.Interpreter        (errMsg)
import Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import Options.Applicative                 (ParserInfo, argument, execParser)
import Options.Applicative                 (fullDesc, header, helper, info)
import Options.Applicative                 (long, metavar, progDesc, short, str)
import Options.Applicative                 (strOption)
import System.Exit                         (exitFailure)
import System.IO                           (hClose, hPutStrLn, stderr)
import System.IO.Temp                      (withSystemTempFile)

data Option = Option { testPath   :: FilePath
                     , answerPath :: FilePath
                     } deriving (Show, Eq, Ord)


defOpts :: ParserInfo Option
defOpts = info (helper <*> opts)
          (fullDesc <> progDesc "Helper for report rating" <> header "checker")
  where
    opts = Option <$> (strOption (long "test" <> short 't' <> metavar "PATH"))
                  <*> (argument str (metavar "PATH"))

addDecl :: Decl -> Module -> Module
addDecl d (Module sl mn mps mws mes idls dls) = Module sl mn mps mws mes idls (d : dls)

main :: IO ()
main = do
  Option{..} <- execParser defOpts
  ParseOk test@(Module _ _ _ _ _ _ decs) <- parseFile testPath
  ParseOk ans@(Module _ _ _ _ _ _ ads)  <- parseFile answerPath
  let propNs = nub $ filter (("prop_" `isPrefixOf`) . prettyPrint) $ concatMap extractFunNames decs
      props = concat [ map qualStmt
                       [ app (var $ name "putStrLn") $ strE $ "* checking: " ++ drop 5 (prettyPrint n)
                       , app (qvar (ModuleName "QC") $ name "quickCheck") (var n)
                       , app (var $ name "putStrLn") $ strE ""
                       , app (var $ name "return") $ tuple []
                       ]
                     | n <- propNs ]
      ma = nameBind noLoc (name "main") $ doE props
      missing = map (Ident . drop 5.prettyPrint) propNs \\ concatMap extractFunNames (ads ++ decs)
      missDec = map (\p -> nameBind noLoc p (app (var $ name "error") $ strE "Not Implemented")) missing
      ltes = addDecl ma test
      lans = foldr addDecl ans missDec
      src = prettyPrint $ addPragmas [LanguagePragma noLoc [name "Safe"]
                                     ,OptionsPragma noLoc  (Just GHC) "-fpackage-trust"] $
            addImports [ImportDecl noLoc (ModuleName "Test.QuickCheck") True False False Nothing
                        (Just (ModuleName "QC"))
                       (Just (False, [IVar NoNamespace (name "quickCheck")]))] $ mergeModules "Check" ltes lans
  withSystemTempFile "Check.hs" $ \fp h -> do
    hPutStrLn h src
    hClose h
    r <- runInterpreter $ do
      mapM_ (unsafeSetGhcOption . ("-trust " ++)) trusted
      mapM_ (unsafeSetGhcOption . ("-distrust " ++)) distrusted
      loadModules [fp]
      setTopLevelModules ["Check"]
      join $ liftIO <$> interpret "main" (as :: IO ())
    case r of
      Left err -> do
        hPutStrLn stderr $ printError err
        exitFailure
      Right () -> putStrLn "done."

printError :: InterpreterError -> String
printError (UnknownError ue) = "*** Unknown Error: " ++ ue
printError (WontCompile ue) =
  unlines $ "*** Compilation Error: " : map errMsg ue
printError (NotAllowed ue) = "*** Not Allowed: " ++ ue
printError (GhcException ue) = "*** GHC Error: " ++ ue

addImports :: [ImportDecl] -> Module -> Module
addImports ims (Module a b c d e ims0 ds) =
  Module a b c d e (nub $ ims ++ ims0) ds

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
