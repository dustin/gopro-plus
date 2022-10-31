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

import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Generic.Random                 (genericArbitrary, uniform)
import           Test.QuickCheck                (Arbitrary (..), Gen, NonNegative (..), arbitraryBoundedEnum, choose,
                                                 elements, listOf, oneof, vector, vectorOf)
import           Test.QuickCheck.Instances.Time ()

import           GoPro.Plus.Auth
import           GoPro.Plus.Media

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

instance Arbitrary AuthInfo where arbitrary = AuthInfo <$> aText <*> (getNonNegative <$> arbitrary) <*> aText <*> aText

aString :: Gen String
aString = listOf (elements (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "?<>/.\\!@#$%^&*()_-'\";:{}[]"))

aText :: Gen Text
aText = T.pack <$> aString

gMaybe :: Gen a -> Gen (Maybe a)
gMaybe a = oneof [pure Nothing, Just <$> a]

aCamera :: Gen String
aCamera = elements [
  "GoPro Max",
  "HERO11 Black",
  "HERO3+Silver Edition",
  "HERO4 Black",
  "HERO5 Black",
  "HERO5 Session",
  "HERO8 Black",
  "HERO9 Black"]

genID :: Gen MediumID
genID = T.pack <$> vectorOf 13 (elements (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']))

instance Arbitrary Medium where
  arbitrary = Medium
    <$> genID
    <*> gMaybe aCamera
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> gMaybe aString
    <*> arbitrary
    <*> aString
    <*> arbitrary
    <*> arbitrary
    <*> gMaybe aString

instance Arbitrary Moment where arbitrary = Moment <$> aText <*> arbitrary

instance Arbitrary ReadyToViewType where arbitrary = arbitraryBoundedEnum

instance Arbitrary MediumType where arbitrary = arbitraryBoundedEnum

instance Arbitrary PageInfo where arbitrary = genericArbitrary uniform

instance Arbitrary Listing where arbitrary = genericArbitrary uniform
