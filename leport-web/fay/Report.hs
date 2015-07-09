{-# LANGUAGE ExtendedDefaultRules, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax, RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Home where
import qualified Fay.Text as T
import Fay.Text
import Prelude
import SharedTypes
import JQuery hiding (append)
#ifdef FAY
import FFI
import Data.MutMap
import WebSockets
import FileAPI
#else
import FayCompat
#endif

default (Text)

put :: Text -> Fay ()
put = ffi "console.log(%1)"

offsetBottom :: JQuery -> Fay Double
offsetBottom = ffi "%1.offset().top"

scrollTo :: Double -> Text -> JQuery -> Fay JQuery
scrollTo = ffi "%3.animate({scrollTop: %1}, %2)"

disable :: JQuery -> Fay JQuery
disable = ffi "%1['prop']('disabled', true)"

enable :: JQuery -> Fay JQuery
enable = ffi "%1['prop']('disabled', false)"

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

newl :: JQuery -> Fay ()
newl prompt = void $ do
  void $ appendToJQuery prompt =<< select "<br>"
  bot <- offsetBottom  prompt
  scrollTo bot "fast" =<< select "body"

performClick :: JQuery -> Fay ()
performClick = ffi "%1['click']()"

main :: Fay ()
main = do
  editors <- mutEmpty
  mapM_ (uncurry $ setupFile editors) [(True, "spec"), (True, "answer"), (False, "multi")]
  chk <- select "#check"
  void $ click (runSingleChecker editors) chk
  mul <- select "#multiFile"
  bind "change" runMultipleChecker mul

infixr 5 <>

(<>) :: Text -> Text -> Text
(<>) = append

setupFile :: MutMap CodeMirror -> Bool -> Text -> Fay ()
setupFile refs enableCM ident = do
  file  <- select $ "#" <> ident <> "File"
  btn   <- select $ "#" <> ident <> "Choose"
  void $ click (const $ performClick file) btn
  when enableCM $ do
    field <- select $ "#" <> ident
    ed <- fromTextArea defCMConf { cmMode = "haskell"
                                   , cmTabSize = 2
                                   , cmMatchBrackets = True}
          field
    mutInsert ident ed refs
    let write f = do
          reader <- newFileReader
          onLoad (flip setCode ed <=< result <=< target) reader
          readAsText f reader
    bind "change" (\e -> do
                      write =<< targetFile =<< target e) file

setCode :: Text -> CodeMirror -> Fay ()
setCode = ffi "%2.setValue(%1)"

result :: Element -> Fay a
result = ffi "%1.result"

newFileReader :: Fay FileReader
newFileReader = ffi "new window['FileReader']()"

debug :: a -> Fay ()
debug = ffi "console.log(JSON.stringify(%1))"

runMultipleChecker :: Event -> Fay ()
runMultipleChecker evFile = do
  f <- targetFile =<< target evFile
  void $ setText (fileName f) =<< select "#multiLabel"
  put "starting"
  reader <- newFileReader
  onLoad body reader
  readAsArrayBuffer f reader
  where
    body readEvt = do
      prompt <- select "#log"
      let normal  = putLog "normal"
          success = putLog "success"
          warn    = putLog "warn"  
          fatal   = putLog "fatal"
          putLog cls msg = do
            void $ appendToJQuery prompt
              =<< setText msg
              =<< select ("<span class=\"" `append` cls `append` "\">")
            newl prompt
      url <- getWSAddress
      normal "Checking..."
      sock <- newWebSocket url
      onClose sock $ \_e ->
        normal "Connection closed."
      onMessage sock $ \e -> do
        let dat = eventData e
        case dat of
          CheckResult fun passed msg -> do
            (if passed then success else warn) $
              "Case " `append` fun `append` ": " `append` msg
          Finished -> success "Rating Finished." >> close sock
          Information ts -> do
            mapM_ (normal . ("message: " `append`)) ts
          Exception ts -> do
            fatal $ T.unlines $ "*** Fatal Error: " : ts
            close sock
      onOpen sock $ \_ -> do
        send sock Multiple
        sendBinary sock =<< result =<< target readEvt
      return ()

clear :: JQuery -> Fay ()
clear = void . setHtml ""

runSingleChecker :: MutMap CodeMirror -> Event -> Fay ()
runSingleChecker editors _ = do
  prompt <- select "#log"
  chk <- select "#check"
  specF <- select "#spec"
  ansF  <- select "#answer"
  [Just scm, Just acm] <- mapM (flip mutLookup editors) ["spec", "answer"]
  let normal  = putLog "normal"
      success = putLog "success"
      warn    = putLog "warn"  
      fatal   = putLog "fatal"
      putLog cls msg = do
        void $ appendToJQuery prompt
          =<< setText msg
          =<< select ("<span class=\"" `append` cls `append` "\">")
        newl prompt
  clear prompt
  mapM_ saveCM [scm,acm]
  spec <- getVal specF
  ans  <- getVal ansF
  unless (T.null spec && T.null ans) $ do
    url <- getWSAddress
    normal "Checking..."
    sock <- newWebSocket url
    onClose sock $ \_e -> do
      normal "Connection closed."
      mapM_ enable [chk,specF,ansF]
    onMessage sock $ \e -> do
      let dat = eventData e
      case dat of
        CheckResult fun passed msg -> do
          (if passed then success else warn) $ "Case " `append` fun `append` ": " `append` msg
        Finished -> success "Rating Finished." >> close sock
        Information ts -> do
          mapM_ (normal . ("message: " `append`)) ts
        Exception ts -> do
          fatal $ T.unlines $ "*** Unexpected Error: " : ts
          close sock
    onOpen sock $ \_ -> do
      send sock $ Single ans

getWSAddress :: Fay Text
getWSAddress = ffi "window['wsaddr']"

getReportID :: Fay Int
getReportID = ffi "window['report_id']"
