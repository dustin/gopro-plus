{-|
Module      : GoPro.Plus.Arbitrary
Description : Arbitrary instances.
Copyright   : (c) Dustin Sallings, 2020
License     : BSD3
Maintainer  : dustin@spy.net
Stability   : experimental

Arbitrary instances for GoPro.Plus
-}

{-# OPTIONS_GHC -Wno-orphans #-}
module GoPro.Plus.Arbitrary where

import           Generic.Random                 (genericArbitrary, uniform)
import           Test.QuickCheck                (Arbitrary(..), vector, choose, arbitraryBoundedEnum)
import           Test.QuickCheck.Instances.Text ()
import           Test.QuickCheck.Instances.Time ()

import GoPro.Plus.Media

instance Arbitrary FileInfo where arbitrary = genericArbitrary uniform

instance Arbitrary FileStuff where
  arbitrary = do
    _files <- vector =<< choose (0,3)
    _variations <- vector =<< choose (0,3)
    _sprites <- vector =<< choose (0,3)
    _sidecar_files <- vector =<< choose (0,3)
    pure FileStuff{..}

instance Arbitrary Variation where arbitrary = genericArbitrary uniform

instance Arbitrary SpriteFrame where arbitrary = genericArbitrary uniform

instance Arbitrary SidecarFile where arbitrary = genericArbitrary uniform

instance Arbitrary Sprite where arbitrary = genericArbitrary uniform

instance Arbitrary File where arbitrary = genericArbitrary uniform

instance Arbitrary Medium where arbitrary = genericArbitrary uniform

instance Arbitrary Moment where arbitrary = genericArbitrary uniform

instance Arbitrary ReadyToViewType where arbitrary = arbitraryBoundedEnum

instance Arbitrary MediumType where arbitrary = arbitraryBoundedEnum

instance Arbitrary PageInfo where arbitrary = genericArbitrary uniform

instance Arbitrary Listing where arbitrary = genericArbitrary uniform
