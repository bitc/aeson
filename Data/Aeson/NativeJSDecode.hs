{-# LANGUAGE CPP #-}

module Data.Aeson.NativeJSDecode where

import Control.Monad (forM_)
import Unsafe.Coerce
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM ( unsafeNew, unsafeWrite )

import Data.Aeson
import Data.Aeson.Types.NativeJSValue
import Data.Aeson.Types.JString

import Data.JSString (JSString)

nativeDecodeVal :: NativeJSValue -> Value
nativeDecodeVal v
    | js_isArray v = Array (decodeArray v)
    | js_isString v = String (JString (js_toString v))
    | js_isNumber v = Number (js_toNumber v)
    | js_isBool v = Bool (js_toBool v)
    | js_isNull v = Null
    | otherwise = Object (decodeObject v)

decodeObject :: NativeJSValue -> H.HashMap JString Value
decodeObject v = addKeys H.empty (js_arrayLength keys - 1)
    where
    keys = js_objectKeys v

    addKeys :: H.HashMap JString Value -> Int -> H.HashMap JString Value
    addKeys h (-1) = h
    addKeys h index =
        let key = js_arrayIndex keys index
            val = js_objectValue v key
        in addKeys (H.insert (JString key) (nativeDecodeVal val) h) (index - 1)

decodeArray :: NativeJSValue -> V.Vector Value
decodeArray (NativeJSValue ref) = V.create $ do
    mv <- VM.unsafeNew (js_arrayLength ref)
    forM_ [0..(js_arrayLength ref - 1)] $ \index -> do
        let val = js_arrayIndex ref index
        VM.unsafeWrite mv index (nativeDecodeVal (unsafeCoerce val))
    return mv

jsonFromString :: JSString -> Maybe NativeJSValue
jsonFromString str =
    let result = js_fromString str
    in if js_isUndefined result
        then Nothing
        else Just result

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe
    "\
    $r = (function(str) { \
      try { \
        return JSON.parse(str); \
      } catch(e) { \
        return undefined; \
      } \
    })($1);"
    js_fromString :: JSString -> NativeJSValue

foreign import javascript unsafe
    "$r = $1 === undefined"
    js_isUndefined :: NativeJSValue -> Bool

#else

js_fromString :: JSString -> NativeJSValue
js_fromString = error "only available in JavaScript"

js_isUndefined :: NativeJSValue -> Bool
js_isUndefined = error "only available in JavaScript"

#endif
