{-# LANGUAGE EmptyDataDecls, ExtendedDefaultRules, LambdaCase     #-}
{-# LANGUAGE OverloadedStrings, RebindableSyntax, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module WebSockets where
import Fay.Text
import Fay.Yesod
import FFI
import FileAPI
import JQuery
import Prelude

default (Text)

data WebSocket

newWebSocket :: Text -> Fay WebSocket
newWebSocket = ffi "new window['WebSocket'](%1)"

onOpen :: WebSocket -> (Event -> Fay ()) -> Fay ()
onOpen = ffi "%1['onopen'] = %2"

onClose :: WebSocket -> (Event -> Fay ()) -> Fay ()
onClose = ffi "%1['onclose'] = %2"

onLoadDone :: WebSocket -> (Event -> Fay ()) -> Fay ()
onLoadDone = ffi "%1['onloadend'] = function(evt){console.log('onloadend');if (evt.target.readyState == FileReader.DONE) {%2(evt)} }"

eventData :: Event -> a
eventData = ffi "JSON.parse(%1.data)"

onMessage :: WebSocket -> (Event -> Fay ()) -> Fay ()
onMessage = ffi "%1['onmessage'] = %2"

close :: WebSocket -> Fay ()
close = ffi "%1.close()"

send :: WebSocket -> a -> Fay ()
send = ffi "%1.send(JSON.stringify(%2))"

{-
sendBinary :: WebSocket -> ByteArray -> Fay ()
sendBinary ws ba = setArrayBuffer ws >> sendBinary_ ws ba
-}

setArrayBuffer :: WebSocket -> Fay ()
setArrayBuffer = ffi "%1.binaryType = 'arraybuffer'"

sendBinary :: WebSocket -> ByteArray -> Fay ()
sendBinary = ffi "%1.send(%2.buffer)"
