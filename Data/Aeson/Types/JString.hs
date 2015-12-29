{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aeson.Types.JString where

import Control.DeepSeq (NFData(..))
import Data.Data (Data)
import Data.Hashable (Hashable(..))

import Data.JSString (JSString)

newtype JString = JString { unJString :: JSString }
    deriving (Eq, Read, Show, Data, NFData)

instance Hashable JString where
    hashWithSalt = error "TODO"
