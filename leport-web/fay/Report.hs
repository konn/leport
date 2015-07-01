{-# LANGUAGE ExtendedDefaultRules, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax, RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Home where
import qualified Fay.Text as T
import Fay.Text
import Fay.Yesod
import Prelude
import SharedTypes
#ifdef FAY
import FFI
import Fay.JQuery
#else
import Control.Monad ((<=<))
import Control.Monad (void)
import Fay.Compiler.Prelude (unless)
import JQuery hiding (append)
import Fay.FFI
#endif

default (Text)

put :: Text -> Fay ()
put = ffi "console.log(%1)"

offsetBottom :: JQuery -> Fay Double
offsetBottom = ffi "%1.offset().top"

scrollTo :: Double -> Text -> JQuery -> Fay JQuery
scrollTo = ffi "%3.animate({scrollTop: %1}, %2)"

type Key = Int

#ifndef FAY
keyCode :: Event -> Key
keyCode = ffi "%1.keyCode"
#endif

disable :: JQuery -> Fay JQuery
disable = ffi "%1['prop']('disabled', true)"

enable :: JQuery -> Fay JQuery
enable = ffi "%1['prop']('disabled', false)"

eventFiles :: Event -> [Text]
eventFiles = ffi "%1.originalEvent.dataTransfer.files"

data CodeMirror

saveCM :: CodeMirror -> Fay ()
saveCM = ffi "%1.save()"


data CMConf_

data CMConf = CMConf { cmValue :: Text
                     , cmMode  :: Text
                     , cmTheme :: Text
                     , cmIndentUnit :: Int
                     , cmSmartIndent :: Bool
                     , cmTabSize :: Int
                     , cmIndentWithTabs :: Bool
                     , cmAutoCloseBrackets :: Bool
                     , cmMatchBrackets :: Bool
                     , cmLineNumbers:: Bool} deriving (Eq)

defCMConf :: CMConf
defCMConf = CMConf { cmValue = ""
                   , cmMode  = "haskell"
                   , cmTheme = "default"
                   , cmIndentUnit = 2
                   , cmSmartIndent = True
                   , cmTabSize = 4
                   , cmIndentWithTabs = False
                   , cmAutoCloseBrackets = True
                   , cmMatchBrackets = True
                   , cmLineNumbers = True}

mkCMConf :: CMConf -> CMConf_
mkCMConf CMConf{..} =
  sub cmValue cmMode cmTheme
      cmIndentUnit cmSmartIndent
      cmTabSize cmIndentWithTabs cmAutoCloseBrackets cmMatchBrackets cmLineNumbers
  where
    sub :: Text ->  Text ->  Text -> Int -> Bool -> Int -> Bool -> Bool -> Bool -> Bool -> CMConf_
    sub = ffi "{value: %1, mode: %2, theme: %3,indentUnit: %4, smartIndent: %5,tabSize: %6, indentWithTabs: %7, autoCloseBrackets: %9, matchBrackets: %8, lineNumbers: %9}"

fromTextArea :: CMConf -> JQuery -> Fay CodeMirror
fromTextArea c j = fromTextArea_ (mkCMConf c) =<< toElement j

fromTextArea_ :: CMConf_        -- ^ Configuration
              -> Element        -- ^ Target
              -> Fay CodeMirror -- ^ Resulting Editor
fromTextArea_ = ffi "CodeMirror.fromTextArea(%2, %1)"

toElement :: JQuery -> Fay Element
toElement = ffi "%1[0]"

message :: JQuery -> Text -> Fay ()
message prompt txt = do
  void $ appendToJQuery prompt =<< setText txt =<< select "<span>"
  newl prompt

newl :: JQuery -> Fay ()
newl prompt = void $ do
  void $ appendToJQuery prompt =<< select "<br>"
  bot <- offsetBottom  prompt
  scrollTo bot "fast" =<< select "body"

performClick :: JQuery -> Fay ()
performClick = ffi "%1['click']()"

main :: Fay ()
main = do
  spec   <- select "#spec"
  answer <- select "#answer"
  prompt <- select "#log"
  specF  <- select "#specFile"
  ansF   <- select "#ansFile"
  specC  <- select "#specChoose"
  ansC  <- select "#ansChoose"
  specCM <- fromTextArea defCMConf { cmMode = "haskell"
                                   , cmTabSize = 2
                                   , cmMatchBrackets = True}
            spec
  ansCM <- fromTextArea defCMConf { cmMode = "haskell"
                                  , cmTabSize = 2
                                  , cmMatchBrackets = True}
           answer
  setupFile specCM specF specC
  setupFile ansCM  ansF ansC
  chk <- select "#check"
  void $ click (runChecker prompt chk spec answer specCM ansCM) chk

setupFile :: CodeMirror -> JQuery -> JQuery -> Fay ()
setupFile ed file btn = do
  let write f = do
        reader <- newFileReader
        onLoad (flip setCode ed <=< result <=< target) reader
        readAsText f reader
  void $ click (const $ performClick file) btn
  bind "change" (\e -> do
                    put "heyheyhey!!!"
                    write =<< targetFile =<< target e) file

setCode :: Text -> CodeMirror -> Fay ()
setCode = ffi "%2.setValue(%1)"

targetFile :: Element -> Fay File
targetFile = ffi "%1.files[0]"

data FileReader

readAsText :: File -> FileReader -> Fay ()
readAsText = ffi "%2.readAsText(%1)"

onLoad :: (Event -> Fay ()) -> FileReader -> Fay ()
onLoad = ffi "%2.onload = %1"

result :: Element -> Fay a
result = ffi "%1.result"

newFileReader :: Fay FileReader
newFileReader = ffi "new window['FileReader']()"

debug :: a -> Fay ()
debug = ffi "console.log(JSON.stringify(%1))"

data File

fileFile :: JQuery -> Fay File
fileFile = ffi "%1.files[0]"

runChecker :: JQuery -> JQuery -> JQuery -> JQuery -> CodeMirror -> CodeMirror -> Event -> Fay ()
runChecker prompt chk specF ansF scm acm _ = do
  mapM_ saveCM [scm,acm]
  put "Gokigen Uruwashu"
  spec <- getVal specF
  ans  <- getVal ansF
  unless (T.null spec && T.null ans) $ do
    void $ disable chk
    message prompt "Checking..."
    call (RunReport spec ans) $ \case
      Success rsl -> do
        void $ appendToJQuery prompt
          =<< setAttr "style" "color: black"
          =<< setText rsl
          =<< select "<span .success>"
        newl prompt
        mapM_ enable [chk,specF,ansF]
      Failure ws -> do
        void $ appendToJQuery prompt
          =<< setAttr "style" "color: red"
          =<< setText (T.unlines ws)
          =<< select "<span .failure>"
        newl prompt
        mapM_ enable [chk,specF,ansF]
