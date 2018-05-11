{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ExtendSpec where


import           Test.Hspec        (Spec, describe, it, shouldBe)

import           Course.Core
import           Course.ExactlyOne (ExactlyOne (ExactlyOne))
import           Course.Functor    ((<$>))
import           Course.List       (List (..), length, listh, reverse)
import           Course.Optional   (Optional (..))

import           Course.Extend     (cojoin, (<<=))

spec :: Spec
spec = do
  it "ExactlyOne instance" $
    (id <<= ExactlyOne 7) `shouldBe` ExactlyOne (ExactlyOne 7)

  describe "List" $ do
    it "length" $
      (length <<= ('a' :. 'b' :. 'c' :. Nil)) `shouldBe` (3 :. 2 :. 1 :. Nil)
    it "id" $
      (id <<= (1 :. 2 :. 3 :. 4 :. Nil)) `shouldBe` nestedListh2 [[1,2,3,4],[2,3,4],[3,4],[4]]
    it "reverse" $
      (reverse <<= ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. Nil)) `shouldBe`
        nestedListh3 [[[4,5,6],[1,2,3]],[[4,5,6]]]

  describe "Optional" $ do
    it "id Full" $
      (id <<= (Full 7)) `shouldBe` Full (Full 7)
    it "id Empty" $
      (id <<= Empty) `shouldBe` (Empty :: Optional (Optional Integer))

  describe "cojoin" $ do
    it "ExactlyOne" $
      cojoin (ExactlyOne 7) `shouldBe` ExactlyOne (ExactlyOne 7)
    it "List" $
      cojoin (1 :. 2 :. 3 :. 4 :. Nil) `shouldBe` nestedListh2 [[1,2,3,4],[2,3,4],[3,4],[4]]
    it "Full" $
      cojoin (Full 7) `shouldBe` Full (Full 7)
    it "Empty" $
      cojoin Empty `shouldBe` (Empty :: Optional (Optional Integer))


nestedListh2 :: [[a]] -> List (List a)
nestedListh2 = (listh <$>) . listh

nestedListh3 :: [[[a]]] -> List (List (List a))
nestedListh3 = ((listh <$>) <$>) . nestedListh2
