{-# LANGUAGE DeriveGeneric #-}
module DailyFeeling.Common.Types (Mood(..), Entry(..)) where

import GHC.Generics
import Data.Aeson

data Mood = Happy
          | Normal
          | Sad deriving (Generic, Show)

instance ToJSON Mood
instance FromJSON Mood

data Entry = Entry { mood        :: Mood
                   , name        :: String
                   , description :: String
                   } deriving (Generic, Show)

instance ToJSON Entry
instance FromJSON Entry
