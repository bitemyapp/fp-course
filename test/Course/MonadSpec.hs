{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.MonadSpec where

import           Test.Hspec        (Spec, describe, it, shouldBe)

import           Course.Core
import           Course.ExactlyOne (ExactlyOne (..))
import           Course.List       (List (..))
import           Course.Monad      (join, (<**>), (=<<), (>>=), (<=<))
import           Course.Optional   (Optional (..))

spec :: Spec
spec = describe "MonadSpec" $ do
  describe "Bind tests" $ do
    it "(=<<) for ExactlyOne" $
      ((\x -> ExactlyOne(x+1)) =<< ExactlyOne 2) `shouldBe` ExactlyOne 3

    it "(=<<) for List" $
      ((\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)) `shouldBe` (1:.1:.2:.2:.3:.3:.Nil)

    it "(=<<) for Optional" $
      ((\n -> Full (n + n)) =<< Full 7) `shouldBe` Full 14

    it "(=<<) for (->)" $
      ((*) =<< (+10)) 7 `shouldBe` 119

  describe "<**>" $ do
    it "ExactlyOne" $
      ExactlyOne (+10) <**> ExactlyOne 8 `shouldBe` ExactlyOne 18
    it "List" $
      (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil `shouldBe` (2:.3:.4:.2:.4:.6:.Nil)
    it "Optional" $
      Full (+8) <**> Full 7 `shouldBe` Full 15
    it "Optional - empty function" $
      Empty <**> Full 7 `shouldBe` (Empty :: Optional Integer)
    it "Optional - empty value" $
      Full (+8) <**> Empty `shouldBe` Empty
    it "(->) 1" $
      ((+) <**> (+10)) 3 `shouldBe` 16
    it "(->) 2" $
      ((+) <**> (+5)) 3 `shouldBe` 11
    it "(->) 3" $
      ((+) <**> (+5)) 1 `shouldBe` 7
    it "(->) 4" $
      ((*) <**> (+10)) 3 `shouldBe` 39
    it "(->) 5" $
      ((*) <**> (+2)) 3 `shouldBe` 15

  describe "join" $ do
    it "List" $
      join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil) `shouldBe` (1:.2:.3:.1:.2:.Nil)
    it "Optional with Empty" $
      join (Full Empty) `shouldBe` (Empty :: Optional Integer)
    it "Optional all Full" $
      join (Full (Full 7)) `shouldBe` Full 7
    it "(->)" $
      join (+) 7 `shouldBe` 14

  describe "bindFlipped" $ do
    it "(>>=)" $
      ((+10) >>= (*)) 7 `shouldBe` 119

  describe "Kleisli Composition" $ do
    it "kleisliComposition" $
      ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
        `shouldBe`
          (2:.2:.3:.3:.Nil)
