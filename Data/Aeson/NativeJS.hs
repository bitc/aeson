module Data.Aeson.NativeJS where

import Data.Aeson
import Data.Aeson.NativeJSEncode
import Data.Aeson.NativeJSDecode

import Data.JSString (JSString)

json :: JSString -> Maybe Value
json str =
    case jsonFromString str of
        Nothing -> Nothing
        Just n -> Just (nativeDecodeVal n)

jsonToString :: Value -> JSString
jsonToString = toString . nativeEncodeVal

decode :: (FromJSON a) => JSString -> Maybe a
decode s = case json s of
    Nothing -> Nothing
    Just v -> case fromJSON v of
        Success r -> Just r
        Error _ -> Nothing

encode :: (ToJSON a) => a -> JSString
encode = jsonToString . toJSON
