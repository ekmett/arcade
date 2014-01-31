{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Rogue.Description where

import Control.Monad
import Control.Lens hiding ((.=))
import Control.Applicative
import Data.Text (Text)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as JS

import Rogue.ASCII
import Rogue.Stat
import Rogue.Bucket

data Description =
    Description { 
        _desc :: Text
      , _descriptionStats :: Stats
      , _descriptionBuckets :: Buckets
      , _descriptionAscii :: Char
      }
  deriving (Read,Show)

makeLenses ''Description

instance HasStats Description where
  stats = descriptionStats

instance HasBuckets Description where
  buckets = descriptionBuckets

instance ASCII Description where
  ascii d = d ^. descriptionAscii

instance JS.ToJSON Description where
  toJSON (Description d s b a) =
    JS.object ["desc" .= d, "stats" .= s, "buckets" .= b, "ascii" .= a]

instance JS.FromJSON Description where
  parseJSON (JS.Object v) =
    Description
    <$> v .: "desc"
    <*> v .: "stats"
    <*> v .: "buckets"
    <*> v .: "ascii"
  parseJSON _ = mzero

-- Eventually this might take data about the viewer's senses?
class HasDescription a where
  description :: a -> Description
