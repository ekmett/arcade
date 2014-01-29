{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
module Rogue.Mob
  where

import Control.Lens
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics
import Data.Aeson

import Rogue.Stat
import Rogue.Expr
import Rogue.Bucket
import Rogue.Hat

type Slot = Text
type ClothingSlot = Text

type Item = ()
type ItemStack = (Item, [Hat])

type Fact = ()
type Verb = ()

data Character =
  Ch {
      -- What slots you have is fairly fixed. We shouldn't use them by name if we can help it.
      _stackSlots    :: Map Slot ItemStack
    , _worn          :: Map ClothingSlot Item
    , _buckets       :: Stats Bucket
    , _facts         :: Set Fact
      -- Buckets are updated in this order, with any unamed buckets being updated in undefined order afterwards.
    , _practice      :: Int64
      -- The mob's inherant verbs
    , _charVerbs     :: Set Verb
    }
  deriving (Read,Show,Generic)

instance FromJSON Character
instance ToJSON Character

makeClassy ''Character

instance HasStats Character Bucket where
  stats = buckets

data Mob =
    Player { _char :: Character }
  deriving (Show,Read,Generic)

instance FromJSON Mob
instance ToJSON Mob

makeLenses ''Mob

instance HasCharacter Mob where
  character = char

instance HasStats Mob Bucket where
  stats = char.stats

startsFull :: Env -> Stat -> Expr -> Expr -> Bucket
startsFull e s l d = Bucket l d (eval e (Capacity s))

rollPlayer :: Mob
rollPlayer =
    leak (stat Endurance) (stat Stun) (10*Sqrt (Current Health)) p 
  where
    p = Player (Ch Map.empty Map.empty b Set.empty 0 Set.empty)
    -- Healthier players recover faster
    hB = startsFull e Health (Given 10) (1+Sqrt (Current Health))
    eB = startsFull e Endurance (20 * Current Health) (Sqrt (Current Health))
    sB = startsFull e Stun (Current Health) (Given 0)
    b = Stats hB eB sB Map.empty
    e = context p
