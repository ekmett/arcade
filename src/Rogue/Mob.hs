module Rogue.Mob (
  ) where

import Data.Word


type Slot = Text
type ClothingSlot = Text

type StatName = Text
type Stats = Map StatName Int32

-- Well known stats
sHPMax, sStaminaMax, sAPMax, sStyle :: StatName

sHPMax = "max hp"
sStaminaMax = "endurance"
sAPMax = "max ap"
sStyle = "style"

data Mob a =
  Mob {
      -- What slots you have is fairly fixed. We shouldn't use them by name if we can help it.
      _stackSlots    :: Map Slot ItemStack
    , _worn          :: Map ClothingSlot Item
    , _stats         :: Stats
    , _facts         :: Set Fact
    , _buckets       :: Map Bucket Word32
    , _practice      :: Int64
      -- The mob's inherant verbs
    , _mobVerbs      :: Set Verb
      -- Custom stuff related to this mob in particular.
      -- For example, a nerdsniper would have a Set of Snipes they could use.
    , _extra         :: a
    }
