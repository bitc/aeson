{-# LANGUAGE CPP #-}

module Data.Aeson.Types.NativeJSValue where

import GHCJS.Types
import JavaScript.Array

newtype NativeJSValue = NativeJSValue (JSRef ())

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe
    "$r = Array.isArray($1)"
    js_isArray :: NativeJSValue -> Bool

foreign import javascript unsafe
    "$r = typeof $1 === 'string'"
    js_isString :: NativeJSValue -> Bool

foreign import javascript unsafe
    "$r = typeof $1 === 'number'"
    js_isNumber :: NativeJSValue -> Bool

foreign import javascript unsafe
    "$r = typeof $1 === 'boolean'"
    js_isBool :: NativeJSValue -> Bool

foreign import javascript unsafe
    "$r = $1 === null"
    js_isNull :: NativeJSValue -> Bool

foreign import javascript unsafe
    "\
    $r = (function(obj) { \
      var result = []; \
      for (k in obj) { \
        result.push(k); \
      } \
      return result; \
    })($1);"
    js_objectKeys :: NativeJSValue -> JSRef a

foreign import javascript unsafe
    "$r = $1.length;"
    js_arrayLength :: JSRef a -> Int

foreign import javascript unsafe
    "$r = $1[$2];"
    js_arrayIndex :: JSRef a -> Int -> JSString

foreign import javascript unsafe
    "$r = $1[$2];"
    js_objectValue :: NativeJSValue -> JSString -> NativeJSValue

foreign import javascript unsafe
    "$r = $1"
    js_toString :: NativeJSValue -> JSString

foreign import javascript unsafe
    "$r = $1"
    js_toNumber :: NativeJSValue -> Double

foreign import javascript unsafe
    "$r = $1"
    js_toBool :: NativeJSValue -> Bool

foreign import javascript unsafe
    "$r = $1"
    js_stringVal :: JSString -> NativeJSValue

foreign import javascript unsafe
    "$r = $1"
    js_numberVal :: Double -> NativeJSValue

foreign import javascript unsafe
    "$r = $1"
    js_boolVal :: Bool -> NativeJSValue

foreign import javascript unsafe
    "$r = null"
    js_nullVal :: NativeJSValue

foreign import javascript unsafe
    " \
    $r = (function(keys, vals) { \
      var obj = {}; \
      var length = keys.length; \
      for (var i = 0; i < length; ++i) { \
        obj[keys[i]] = vals[i]; \
      } \
      return obj; \
    })($1, $2)"
    js_objectVal :: JSArray -> JSArray -> NativeJSValue

foreign import javascript unsafe
    "$r = $1"
    js_arrayVal :: JSArray -> NativeJSValue

#else

js_isArray :: NativeJSValue -> Bool
js_isArray = error "only available in JavaScript"

js_isString :: NativeJSValue -> Bool
js_isString = error "only available in JavaScript"

js_isNumber :: NativeJSValue -> Bool
js_isNumber = error "only available in JavaScript"

js_isBool :: NativeJSValue -> Bool
js_isBool = error "only available in JavaScript"

js_isNull :: NativeJSValue -> Bool
js_isNull = error "only available in JavaScript"

js_objectKeys :: NativeJSValue -> JSRef a
js_objectKeys = error "only available in JavaScript"

js_arrayLength :: JSRef a -> Int
js_arrayLength = error "only available in JavaScript"

js_arrayIndex :: JSRef a -> Int -> JSString
js_arrayIndex = error "only available in JavaScript"

js_objectValue :: NativeJSValue -> JSString -> NativeJSValue
js_objectValue = error "only available in JavaScript"

js_toString :: NativeJSValue -> JSString
js_toString = error "only available in JavaScript"

js_toNumber :: NativeJSValue -> Double
js_toNumber = error "only available in JavaScript"

js_toBool :: NativeJSValue -> Bool
js_toBool = error "only available in JavaScript"

js_stringVal :: JSString -> NativeJSValue
js_stringVal = error "only available in JavaScript"

js_numberVal :: Double -> NativeJSValue
js_numberVal = error "only available in JavaScript"

js_boolVal :: Bool -> NativeJSValue
js_boolVal = error "only available in JavaScript"

js_nullVal :: NativeJSValue
js_nullVal = error "only available in JavaScript"

js_objectVal :: JSArray -> JSArray -> NativeJSValue
js_objectVal = error "only available in JavaScript"

js_arrayVal :: JSArray -> NativeJSValue
js_arrayVal = error "only available in JavaScript"

#endif
