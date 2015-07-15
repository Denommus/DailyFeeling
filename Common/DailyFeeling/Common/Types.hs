{-# LANGUAGE DeriveGeneric #-}
module DailyFeeling.Common.Types (Mood(..), Entry(..)) where

import GHC.Generics
import Data.Aeson

data Mood = Happy
          | Normal
          | Sad deriving (Generic)

instance ToJSON Mood

data Entry = Entry { entryId     :: Integer
                   , mood        :: Mood
                   , name        :: String
                   , description :: String
                   } deriving (Generic)

instance ToJSON Entry
