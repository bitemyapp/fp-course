{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FunctorSpec where

import           Test.Hspec               (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck.Function (Fun (..))

import           Course.Core
import           Course.ExactlyOne        (ExactlyOne (..))
import           Course.Functor           (void, (<$), (<$>))
import           Course.List              (List (..))
import           Course.Optional          (Optional (..))

spec :: Spec
spec = do
  it "ExactlyOne" $ (+1) <$> ExactlyOne 2 `shouldBe` ExactlyOne 3

  describe "List" $ do
    it "empty list" $
      (+1) <$> Nil `shouldBe` Nil
    it "increment" $
      (+1) <$> (1 :. 2 :. 3 :. Nil) `shouldBe` (2 :. 3 :. 4 :. Nil)

  describe "Optional" $ do
    it "Empty" $ (+1) <$> Empty `shouldBe` Empty
    it "Full"  $ (+1) <$> Full 2 `shouldBe` Full 3

  describe "Function" $ do
    it "(->)" $ ((+1) <$> (*2)) 8 `shouldBe` 17

  describe "(<$)" $ do
    it "Map 7" $ 7 <$ (1 :. 2 :. 3 :. Nil) `shouldBe` (7 :. 7 :. 7 :. Nil)
    prop "Always maps a constant value over List" $
      \x a b c -> (x :: Integer) <$ ((a :. b :. c :. Nil) :: List Integer) == (x :. x :. x :. Nil)
    prop "Always maps a constant value over Full (Optional)" $
      \(x :: Integer) (q :: Integer) -> x <$ Full q == Full x

  describe "void" $ do
    it "List"  $ void (1 :. 2 :. 3 :. Nil) `shouldBe` () :. () :. () :. Nil
    it "Full"  $ void (Full 7) `shouldBe` Full ()
    it "Empty" $ void Empty `shouldBe` Empty
    it "(->)"  $ void (+10) 5 `shouldBe` ()
