{-# LANGUAGE CPP #-}

module Data.Aeson.NativeJSEncode
    ( nativeEncodeVal
    , toString
    ) where

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Data.Aeson
import Data.Aeson.Types.NativeJSValue
import Unsafe.Coerce

import Data.Aeson.Types.JString (JString(..))

import JavaScript.Array
import GHCJS.Types

nativeEncodeVal :: Value -> NativeJSValue
nativeEncodeVal (Object o) =
    let (keys, vals) = unzip $ map (\(key, val) -> (unJString key, nativeEncodeVal val)) (H.toList o)
        keysArray = fromList (map stringToJSVal keys)
        valsArray = fromList (map valToJSVal vals)
    in js_objectVal keysArray valsArray

nativeEncodeVal (Array arr) =
    let vals = map nativeEncodeVal (V.toList arr)
        valsArray = fromList (map valToJSVal vals)
    in js_arrayVal valsArray

nativeEncodeVal (String str) = js_stringVal (unJString str)
nativeEncodeVal (Number num) = js_numberVal num
nativeEncodeVal (Bool b) = js_boolVal b
nativeEncodeVal Null = js_nullVal

stringToJSVal :: JSString -> JSVal
stringToJSVal = unsafeCoerce

valToJSVal :: NativeJSValue -> JSVal
valToJSVal = unsafeCoerce

toString :: NativeJSValue -> JSString
toString = js_nativeToString

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe
    "$r = JSON.stringify($1)"
    js_nativeToString :: NativeJSValue -> JSString

#else

js_nativeToString :: NativeJSValue -> JSString
js_nativeToString = error "only available in JavaScript"

#endif
