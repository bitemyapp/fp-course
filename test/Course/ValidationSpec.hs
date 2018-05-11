{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ValidationSpec where

import qualified Prelude               as P (either, fmap)
import           Test.Hspec               (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck    (prop)
import Test.QuickCheck (Arbitrary(..), (===), (.||.))

import           Course.Core
import           Course.Validation

instance Arbitrary a => Arbitrary (Validation a) where
  arbitrary = P.fmap (P.either Error Value) arbitrary

spec :: Spec
spec = describe "ValidationSpec" $ do
  describe "isError" $ do
    it "true for errors" $
      isError (Error "Message") `shouldBe` True
    it "false for values" $
      isError (Value "7") `shouldBe` False
    prop "not the same as isValue" $
      \(x :: Validation Int) -> isError x /= isValue x

  describe "isValue" $ do
    it "false for errors" $
      isValue (Error "Message") `shouldBe` False
    it "false for values" $
      isValue (Value "7") `shouldBe` True
    prop "not the same as isValue" $
      \(x :: Validation Int) -> isValue x /= isError x

  describe "mapValidation" $ do
    it "errors unchanged" $
      mapValidation (+ 10) (Error "message") `shouldBe` Error "message"
    it "values changed" $
      mapValidation (+ 10) (Value 7) `shouldBe` Value 17
    prop "map with id causes no change" $
      \(x :: Validation Int) -> mapValidation id x === x

  let
    f n =
      if even n
      then Value (n + 10)
      else Error "odd"
  describe "bindValidation" $ do
    it "error unchanged" $
      bindValidation f (Error "message") `shouldBe` Error "message"
    it "odd value" $
      bindValidation f (Value 7) `shouldBe` Error "odd"
    it "even value" $
      bindValidation f (Value 8) `shouldBe` Value 18
    prop "bind with Value causes no change" $
      \(x :: Validation Int) -> bindValidation Value x === x

  describe "valueOr" $ do
    it "falls through for errors" $
      valueOr (Error "message") "foo" `shouldBe` "foo"
    it "unwraps values" $
      valueOr (Value "foo") "bar" `shouldBe` "foo"
    prop "isValue or valueOr falls through" $
      \(x :: Validation Int) n -> isValue x .||. valueOr x n === n

  describe "errorOr" $ do
    it "unwraps errors" $
      errorOr (Error "message") "q" `shouldBe` "message"
    it "falls through for values" $
      errorOr (Value (7 :: Integer)) "q" `shouldBe` "q"
    prop "isError or errorOr falls through" $
      \(x :: Validation Int) n -> isError x .||. errorOr x n === n
