{-# LANGUAGE ExtendedDefaultRules, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax, RecordWildCards, EmptyDataDecls   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module FileAPI where
import qualified Fay.Text as T
import Fay.Text
import Fay.Yesod
import Prelude
import SharedTypes
import JQuery hiding (append)
#ifdef FAY
import FFI
import Data.MutMap
import WebSockets
#else
import Fay
import Fay.FFI
#endif

default (Text)

data File

fileFile :: JQuery -> Fay File
fileFile = ffi "%1.files[0]"

fileName :: File -> Text
fileName = ffi "%1.name"

targetFile :: Element -> Fay File
targetFile = ffi "%1.files[0]"

data FileReader

readAsText :: File -> FileReader -> Fay ()
readAsText = ffi "%2.readAsText(%1)"

data ArrayBuffer

readAsArrayBuffer :: File -> FileReader -> Fay ()
readAsArrayBuffer = ffi "%2.readAsArrayBuffer(%1)"

onLoad :: (Event -> Fay ()) -> FileReader -> Fay ()
onLoad = ffi "%2.onload = %1"
