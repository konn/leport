{-# LANGUAGE ExtendedDefaultRules, ImplicitPrelude, LambdaCase    #-}
{-# LANGUAGE OverloadedStrings, RebindableSyntax, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fwarn-unused-binds #-}
module FayCompat (module Fay, module Fay.Text, module Fay.FFI,
                  module Control.Monad, module FayCompat) where
import Control.Monad ((<=<))
import Control.Monad (unless, void, when)
import Fay
import Fay.FFI
import Fay.Text
import JQuery
import Prelude

data MutMap a

mutEmpty :: Fay (MutMap a)
mutEmpty = undefined

mutFromList :: [(Text, a)] -> Fay (MutMap a)
mutFromList = undefined

mutLookup :: Text -> MutMap a -> Fay (Maybe a)
mutLookup = undefined

mutElems :: MutMap a -> Fay [a]
mutElems = undefined

mutKeys :: MutMap a -> Fay [Text]
mutKeys = undefined

mutAssocs :: MutMap a -> Fay [(Text, a)]
mutAssocs = undefined

mutClone :: MutMap a -> Fay (MutMap a)
mutClone = undefined

mutMapM :: (a -> Fay b) -> MutMap a -> MutMap b
mutMapM = undefined

mutMapM_ :: (a -> Fay ()) -> MutMap a -> Fay ()
mutMapM_ = undefined

mutMapMaybeM :: (a -> Fay (Maybe b)) -> MutMap a -> MutMap b
mutMapMaybeM = undefined

mutInsert :: Text -> a -> MutMap a -> Fay ()
mutInsert = undefined

mutDelete :: Text -> MutMap a -> Fay ()
mutDelete = undefined
mutClear :: MutMap a -> Fay ()
mutClear = undefined

data WebSocket

newWebSocket :: Text -> Fay WebSocket
newWebSocket = ffi "new WebSocket(%1)"

onOpen :: WebSocket -> (Event -> Fay ()) -> Fay ()
onOpen = ffi "%1['onopen'] = %2"

onClose :: WebSocket -> (Event -> Fay ()) -> Fay ()
onClose = ffi "%1['onclose'] = %2"

eventData :: Event -> a
eventData = ffi "JSON.parse(%1.data)"

onMessage :: WebSocket -> (Event -> Fay ()) -> Fay ()
onMessage = ffi "%1['onmessage'] = %2"

close :: WebSocket -> Fay ()
close = ffi "%1.close()"

send :: WebSocket -> a -> Fay ()
send = ffi "%1.send(JSON.stringify(%2))"

ifThenElse :: Bool -> t -> t -> t
ifThenElse True  p _ = p
ifThenElse False _ q = q


data File

fileFile :: JQuery -> Fay File
fileFile = ffi "%1.files[0]"

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

onLoadDone :: (Event -> Fay ()) -> FileReader -> Fay ()
onLoadDone = ffi "%2.onload = %1"

type Key = Int

keyCode :: Event -> Key
keyCode = ffi "%1.keyCode"

data ByteArray

sendBinary :: WebSocket -> ByteArray -> Fay ()
sendBinary = ffi "%1.send(%2)"

fileName :: File -> Text
fileName = undefined
