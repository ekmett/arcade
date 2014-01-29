{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Rogue.Mob
  where

import Control.Lens
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

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

makeClassy ''Character

instance HasStats Character Bucket where
  stats = buckets

data Mob =
  Player { _char :: Character }

makeLenses ''Mob

instance HasCharacter Mob where
  character = char

instance HasStats Mob Bucket where
  stats = char.stats

mobEnv :: Mob -> Env
mobEnv m =
    e
  where
    e = Env (\s -> m ^. buckets.stat s.current) (\s -> eval e (m ^. buckets.stat s.capacity))

startsFull :: Env -> Stat -> Expr -> Expr -> Bucket
startsFull e s l d = Bucket l d (eval e (Capacity s))

rollPlayer :: Mob
rollPlayer =
    p
  where
    p = Player (Ch Map.empty Map.empty b Set.empty 0 Set.empty)
    hB = startsFull e Health (Given 10) (Sqrt (Given 10))
    eB = startsFull e Endurance (20 * Current Health) (Sqrt (Current Health))
    sB = startsFull e Stun (Current Health) (Given 0)
    b = Stats hB eB sB Map.empty
    e = mobEnv p
