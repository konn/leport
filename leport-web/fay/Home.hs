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
  return ()
