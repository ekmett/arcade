{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Rogue.Mob
  ( MobId
  , Mob(..)
  , HasMob(..)
  , mobbed
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Default
import Data.Foldable
import Data.Function (on)
import Data.Table
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Rogue.Body
import Rogue.Location
import Rogue.Stat

type MobId = UUID

data Mob a = Mob
  { _mobId :: {-# UNPACK #-} !MobId
  , _mobBody :: !Body
  , _mind :: a
  } deriving (Show,Read)

mobbed :: APrism s t a b -> Prism (Mob s) (Mob t) (Mob a) (Mob b)
mobbed p = prism (fmap (clonePrism p #)) $ \(Mob i b s) -> case clonePrism p Left s of
  Right t -> Left (Mob i b t)
  Left a -> Right (Mob i b a)

makeClassy ''Mob

instance Eq (Mob a) where
  (==) = (==) `on` _mobId

instance Ord (Mob a) where
  compare = compare `on` _mobId

instance Comonad Mob where
  extract (Mob _ _ a) = a
  extend f w@(Mob i b _) = Mob i b (f w)

instance Functor Mob where
  fmap f (Mob i b a) = Mob i b (f a)

instance Foldable Mob where
  foldMap f (Mob _ _ a) = f a

instance Traversable Mob where
  traverse f (Mob i b a) = Mob i b <$> f a

instance HasLocation (Mob a) where
  location = body.location

instance HasStats (Mob a) where
  stats = body.stats

instance FromJSON a => FromJSON (Mob a) where
  parseJSON (Object v) = Mob <$>
    (v .: "id" >>= withText "id" (maybe mzero return . UUID.fromString . T.unpack)) <*>
    v .: "body" <*>
    v .: "mind"
  parseJSON _ = mzero

instance ToJSON a => ToJSON (Mob a) where
  toJSON (Mob i b m) = object ["id" .= (T.pack $ UUID.toString i), "body" .= b, "mind" .= m]

instance HasBody (Mob a) where
  body = mobBody

instance Default a => Default (Mob a) where
  def = Mob UUID.nil def def

instance Tabular (Mob a) where
  type PKT (Mob a) = MobId

  data Key k (Mob a) b where
    MobId :: Key Primary (Mob a) MobId
    MobLocation :: Key Supplemental (Mob a) Location

  data Tab (Mob a) i = MobTab (i Primary MobId) (i Supplemental Location)

  fetch MobId = _mobId
  fetch MobLocation = _bodyLocation._mobBody

  primary = MobId
  primarily MobId r = r

  mkTab f = MobTab <$> f MobId <*> f MobLocation
  forTab (MobTab x y) f = MobTab <$> f MobId x <*> f MobLocation y
  ixTab (MobTab x _) MobId = x
  ixTab (MobTab _ x) MobLocation = x
