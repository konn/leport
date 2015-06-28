{-# LANGUAGE RankNTypes #-}
module Handler.Fay where
import ClassyPrelude.Yesod
import Language.Haskell.Interpreter (Extension(NoImplicitPrelude), OptionVal((:=)), eval, loadModules)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import qualified Language.Haskell.Interpreter as Ghc
import Fay.Convert (readFromFay)
import qualified Data.List as L
import System.IO.Temp
import Language.Haskell.Interpreter (setTopLevelModules)
import Import.NoFoundation
import Language.Haskell.Interpreter (InterpreterError(..))
import Language.Haskell.Interpreter (errMsg)
import PureIO

handleFay :: CommandHandler app
handleFay render command = do
    $logDebug $ "incoming comand: " ++ pack (show (readFromFay command :: Maybe RunReport))
    case readFromFay command of
      Just (RunReport src r) -> withSystemTempFile "Main.hs" $ \fp h -> do
        liftIO $ do
          hPut h ("module Main where\nimport Prelude\nmain :: IO ()\nmain = putStrLn \"a\"" :: String)
          hClose h
        eith <- unsafeRunInterpreterWithArgs
          ["-XSafe"] $ do
            exts <- Ghc.get Ghc.languageExtensions
            Ghc.set [Ghc.languageExtensions := (L.delete NoImplicitPrelude exts)]
            loadModules [fp]
            setTopLevelModules ["Main"]
            eval (unpack src)
        case eith of
          Right ans -> do
            render r (Success $ pack ans)
          Left  (UnknownError err) -> render r $ Failure [pack err]
          Left  (WontCompile err)  -> render r $ Failure  (map (pack . errMsg) err)
          Left  (NotAllowed err)   -> render r $ Failure $ [pack err]
          Left  (GhcException err) -> render r $ Failure $ [pack err]
      Nothing -> invalidArgs ["Invalid arguments"]
