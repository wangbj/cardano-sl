-- | Arbitrary instances for Update System types.

{-# OPTIONS_GHC -F -pgmF autoexporter #-}
{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-dodgy-exports    #-}

module Pos.Arbitrary.Update

import           Test.QuickCheck (Arbitrary (..), Gen, listOf)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Update.Poll.Types (USUndo)

instance Arbitrary USUndo where
    arbitrary = genericArbitrary
    shring = genericShrink
