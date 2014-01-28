module Rogue.Mob (
  ) where

import Data.Word


type Slot = Text
type ClothingSlot = Text

type StatName = Text
type Stats = Map StatName Int64

-- Well known stats
sHPMax, sStaminaMax, sAPMax, sStyle :: StatName

sHPMax = "max hp"
sStaminaMax = "endurance"
sAPMax = "max ap"
sStyle = "style"

data StatusModifier =
  

data Mob =
  Mob {
      -- What slots you have is fairly fixed. We shouldn't use them by name if we can help it.
      _stackSlots    :: Map Slot ItemStack
    , _worn          :: Map ClothingSlot Item
    , _stats         :: Stats
    , _facts         :: Set Fact
    , _buckets       :: Map BucketName Bucket
      -- Buckets are updated in this order, with any unamed buckets being updated in undefined order afterwards.
    , _bucketOrder   :: [BucketName]
    , _practice      :: Int64
      -- The mob's inherant verbs
    , _mobVerbs      :: Set Verb
    }
