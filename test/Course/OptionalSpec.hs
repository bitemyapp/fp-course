{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.OptionalSpec where

import           Test.Hspec               (Spec, describe, it, shouldBe)

import           Course.Core
import           Course.Optional  (Optional (..), bindOptional, mapOptional,
                                   (<+>), (??))
spec :: Spec
spec = describe "OptionalSpec" $ do
  describe "mapOptional" $ do
    it "Empty" $
      mapOptional (+1) Empty `shouldBe` Empty
    it "Full" $
      mapOptional (+1) (Full 8) `shouldBe` Full 9

  let
    evenDecOddInc n =
      if even n
      then Full (n - 1)
      else Full (n + 1)

  describe "bindOptional" $ do
    it "Empty" $
      bindOptional Full Empty `shouldBe` (Empty :: Optional Integer)
    it "even dec, odd inc, even input" $
      bindOptional evenDecOddInc (Full 8) `shouldBe` Full 7
    it "even dec, odd inc, odd input" $
      bindOptional evenDecOddInc (Full 9) `shouldBe` Full 10

  describe "??" $ do
    it "Full" $
      Full 8 ?? 99 `shouldBe` 8
    it "Empty" $
      Empty ?? 99 `shouldBe` 99

  describe "<+>" $ do
    it "first Full" $
      Full 8 <+> Empty `shouldBe` Full 8
    it "both Full" $
      Full 8 <+> Full 9 `shouldBe` Full 8
    it "first Empty" $
      Empty <+> Full 9 `shouldBe` Full 9
    it "both empty" $
      Empty <+> Empty `shouldBe` (Empty :: Optional Integer)
