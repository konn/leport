{-# LANGUAGE EmptyDataDecls, OverloadedStrings, RebindableSyntax #-}
module Fay.JQuery (module Fay.JQuery) where
import Fay.Text
#ifdef FAY
import FFI
#else
import Fay.FFI
#endif
import Prelude  (Int,Bool)

class Selectable a
data Element
data JQuery

instance Selectable Text
instance Selectable Element
instance Selectable JQuery

type EventType = Text
data Event
type Selector = Text
type KeyCode = Int

select :: Selectable a => a -> Fay JQuery
select = ffi "jQuery(Fay$$_(%1))"

create :: Text -> Fay JQuery
create = ffi "jQuery(Fay$$_(%1))"

keypress :: (Event -> Fay ()) -> JQuery -> Fay ()
keypress = ffi "%2.keypress(%1)"

keyCode :: Event -> KeyCode
keyCode = ffi "%1.keyCode"

appendToJQuery :: JQuery -> JQuery -> Fay JQuery
appendToJQuery = ffi "%2['appendTo'](%1)"

setText :: Text -> JQuery -> Fay JQuery
setText = ffi "%2['text'](%1)"

getText :: JQuery -> Fay Text
getText = ffi "%1['text']()"

setVal :: Text -> JQuery -> Fay JQuery
setVal = ffi "%2['val'](%1)"

getVal :: JQuery -> Fay Text
getVal = ffi "%1['val']()"

setAttr :: Text -> Text -> JQuery -> Fay JQuery
setAttr = ffi "%3['attr'](%1, %2)"

getAttr :: Text -> JQuery -> Fay (Defined Text)
getAttr = ffi "%2['attr'](%1)"

click :: (Event -> Fay ()) -> JQuery -> Fay JQuery
click = ffi "%2['click'](%1)"

getProp :: Text -> JQuery -> Fay Text
getProp = ffi "%2['prop'](%1)"

setProp :: Text -> Text -> JQuery -> Fay JQuery
setProp = ffi "%3['prop'](%1, %2)"

bind :: EventType -> (Event -> Fay ()) -> JQuery -> Fay ()
bind = ffi "%3['bind'](%1, %2)"

preventDefault :: Event -> Fay ()
preventDefault = ffi "%1['preventDefault']()"

target :: Event -> Fay Element
target = ffi "%1['target']"

