{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Course.ApplicativeSpec where

import           Control.Monad (Monad)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Course.Applicative    (filtering, lift1, lift2, lift3, lift4,
                                        pure, replicateA, sequence, (*>), (<*),
                                        (<*>))
import           Course.Core
import           Course.ExactlyOne     (ExactlyOne (..))
import           Course.Functor        ((<$>))
import           Course.List           (List (..), filter, length, listh,
                                        product, sum)
import           Course.Optional       (Optional (..))

tests :: IO Bool
tests = checkSequential $$(discover)

genInt :: Monad m => PropertyT m Int
genInt = forAll $ Gen.int (Range.linear 0 100)

prop_pureExactlyOne :: Property
prop_pureExactlyOne =
  property $ do
    x <- genInt
    pure x === ExactlyOne x

prop_applyExactlyOne :: Property
prop_applyExactlyOne =
  property $ do
    x <- genInt
    y <- genInt
    (ExactlyOne (+x) <*> ExactlyOne y) === ExactlyOne (x + y)

prop_pureList :: Property
prop_pureList =
  property $ do
    x <- genInt
    pure x === (x :. Nil)

prop_applyList :: Property
prop_applyList =
  property $
    ((+1) :. (*2) :. Nil <*> listh [1,2,3]) === listh [2,3,4,2,4,6]

prop_lift1ExactlyOne :: Property
prop_lift1ExactlyOne =
  property $ do
    x <- genInt
    y <- genInt
    lift1 (+x) (ExactlyOne y) === ExactlyOne (x + y)

prop_lift1ListNil :: Property
prop_lift1ListNil =
  property $ do
    x <- genInt
    lift1 (+x) Nil === Nil

prop_lift1List :: Property
prop_lift1List =
  property $ do
    lift1 (+1) (listh [1, 2, 3]) === listh [2, 3, 4]

prop_pureOptional :: Property
prop_pureOptional =
  property $ do
    x <- genInt
    pure x === Full x

prop_applyTwoOptionals :: Property
prop_applyTwoOptionals =
  property $ do
    x <- genInt
    y <- genInt
    (Full (+x) <*> Full y) === Full (x + y)

prop_applyLeftEmpty :: Property
prop_applyLeftEmpty =
  property $ do
    x <- genInt
    (Empty <*> Full x) === (Empty :: Optional Int)

prop_applyRightEmpty :: Property
prop_applyRightEmpty =
  property $ do
    x <- genInt
    (Full (+x) <*> Empty) === (Empty :: Optional Int)

prop_addition :: Property
prop_addition =
  property $
    ((+) <*> (+10)) 3 === 16

prop_moreAddition :: Property
prop_moreAddition =
  property $
    ((+) <*> (+5)) 3 === 11

prop_evenMoreAddition :: Property
prop_evenMoreAddition =
  property $
    ((+) <*> (+5)) 1 === 7

prop_additionAndMultiplication :: Property
prop_additionAndMultiplication =
  property $
    ((*) <*> (+10)) 3 === 39

prop_moreAdditionAndMultiplication :: Property
prop_moreAdditionAndMultiplication =
  property $
    ((*) <*> (+2)) 3 === 15

prop_pureFunction :: Property
prop_pureFunction =
  property $ do
    x <- genInt
    y <- genInt
    pure x y === x

prop_plusOverExactlyOne :: Property
prop_plusOverExactlyOne =
  property $ do
    x <- genInt
    y <- genInt
    lift2 (+) (ExactlyOne x) (ExactlyOne y) === ExactlyOne (x + y)

prop_plusOverList :: Property
prop_plusOverList =
  property $
    lift2 (+) (listh [1,2,3]) (listh [4,5]) === listh [5,6,6,7,7,8]

prop_plusOverTwoOptionals :: Property
prop_plusOverTwoOptionals =
  property $ do
    x <- genInt
    y <- genInt
    lift2 (+) (Full x) (Full y) === Full (x + y)

prop_plusOverLeftEmpty :: Property
prop_plusOverLeftEmpty =
  property $ do
    x <- genInt
    lift2 (+) Empty (Full x) === Empty

prop_plusOverRightEmpty :: Property
prop_plusOverRightEmpty =
  property $ do
    x <- genInt
    lift2 (+) (Full x) Empty === Empty

prop_plusOverFunctions :: Property
prop_plusOverFunctions =
  property $
    lift2 (+) length sum (listh [4, 5, 6]) === 18

prop_lift3PlusOverExactlyOne :: Property
prop_lift3PlusOverExactlyOne =
  property $ do
    x <- genInt
    y <- genInt
    z <- genInt
    lift3 (\a b c -> a + b + c) (ExactlyOne x) (ExactlyOne y) (ExactlyOne z)
      === ExactlyOne (x + y + z)

prop_lift3PlusOverList :: Property
prop_lift3PlusOverList =
  property $
    lift3 (\a b c -> a + b + c) (listh [1,2,3]) (listh [4,5]) (listh [6,7,8])
      === listh [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]

prop_lift3PlusOverThreeOptionals :: Property
prop_lift3PlusOverThreeOptionals =
  property $ do
    x <- genInt
    y <- genInt
    z <- genInt
    lift3 (\a b c -> a + b + c) (Full x) (Full y) (Full z)
      === Full 24

prop_lift3PlusOverFirstEmpty :: Property
prop_lift3PlusOverFirstEmpty =
  property $ do
    x <- genInt
    y <- genInt
    lift3 (\a b c -> a + b + c) (Full x) (Full y) Empty
      === Empty

prop_lift3PlusOverLastEmpty :: Property
prop_lift3PlusOverLastEmpty =
  property $ do
    x <- genInt
    y <- genInt
    lift3 (\a b c -> a + b + c) Empty (Full x) (Full y)
      === Empty

prop_lift3PlusOverTwoEmpty :: Property
prop_lift3PlusOverTwoEmpty =
  property $ do
    x <- genInt
    lift3 (\a b c -> a + b + c) Empty Empty (Full x)
      === Empty

prop_lift3PlusOverFunctions :: Property
prop_lift3PlusOverFunctions =
  property $
    lift3 (\a b c -> a + b + c) length sum product (listh [4, 5, 6])
      === 138

{--
spec :: Spec
spec = do
  describe "lift4" $ do
    it "+ over ExactlyOne" $
      lift4 (\a b c d -> a + b + c + d) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9) (ExactlyOne 10)
        `shouldBe`
          ExactlyOne 34
    it "+ over List" $
      lift4 (\a b c d -> a + b + c + d) (listh [1, 2, 3]) (listh [4, 5]) (listh [6, 7, 8]) (listh [9, 10])
        `shouldBe`

        (listh [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26])
    it "+ over Optional" $
      lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
        `shouldBe`
          Full 34
    it "+ over Optional - third Empty" $
      lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
        `shouldBe`
          Empty
    it "+ over Optional - first Empty" $
      lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
        `shouldBe`
          Empty
    it "+ over Optional - first and second Empty" $
      lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
        `shouldBe`
          Empty
    it "+ over functions" $
      lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) (listh [4,5,6])
        `shouldBe`
          148

  describe "rightApply" $ do
    it "*> over List" $
      listh [1,  2,  3] *> listh [4,  5,  6]
        `shouldBe`
          listh [4,5,6,4,5,6,4,5,6]
    it "*> over List" $
      listh [1,  2] *> listh [4,  5,  6]
        `shouldBe`
          listh [4,5,6,4,5,6]
    it "another *> over List" $
      listh [1,  2,  3] *> listh [4,  5]
        `shouldBe`
          listh [4,5,4,5,4,5]
    it "*> over Optional" $
      Full 7 *> Full 8
        `shouldBe`
          Full 8
    prop "*> over List property" $
      \a b c x y z ->
        let l1 = (listh [a,  b,  c] :: List Integer)
            l2 = (listh [x,  y,  z] :: List Integer)
         in l1 *> l2 == listh [x,  y,  z,  x,  y,  z,  x,  y,  z]
    prop "*> over Optional property" $
      \x y -> (Full x :: Optional Integer) *> (Full y :: Optional Integer) == Full y

  describe "leftApply" $ do
    it "<* over List" $
      (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. 6 :. Nil)
        `shouldBe`
          listh [1,1,1,2,2,2,3,3,3]
    it "another <* over List" $
      (1 :. 2 :. Nil) <* (4 :. 5 :. 6 :. Nil)
        `shouldBe`
          listh [1,1,1,2,2,2]
    it "Yet another <* over List" $
      (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. Nil)
        `shouldBe`
          listh [1,1,2,2,3,3]
    it "<* over Optional" $
      Full 7 <* Full 8
        `shouldBe`
          Full 7
    prop "<* over List property" $
      \x y z a b c ->
        let l1 = (x :. y :. z :. Nil) :: List Integer
            l2 = (a :. b :. c :. Nil) :: List Integer
         in l1 <* l2 == listh [x,  x,  x,  y,  y,  y,  z,  z,  z]
    prop "<* over Optional property" $
      \x y -> Full (x :: Integer) <* Full (y :: Integer) == Full x

  describe "sequence" $ do
    it "ExactlyOne" $
      sequence (listh [ExactlyOne 7, ExactlyOne 8, ExactlyOne 9])
        `shouldBe`
          ExactlyOne (listh [7,8,9])
    it "List" $
      sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
        `shouldBe`
          (listh <$> (listh [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]))
    it "Optional with an empty" $
      sequence (Full 7 :. Empty :. Nil)
        `shouldBe`
          Empty
    it "Optional" $
      sequence (Full 7 :. Full 8 :. Nil)
        `shouldBe`
          Full (listh [7,8])
    it "(->)" $
      sequence ((*10) :. (+2) :. Nil) 6
        `shouldBe`
          (listh [60,8])

  describe "replicateA" $ do
    it "ExactlyOne" $
      replicateA 4 (ExactlyOne "hi")
        `shouldBe`
          ExactlyOne (listh ["hi","hi","hi","hi"])
    it "Optional - Full" $
      replicateA 4 (Full "hi")
        `shouldBe`
          Full (listh ["hi","hi","hi","hi"])
    it "Optional - Empty" $
      replicateA 4 Empty
        `shouldBe`
          (Empty :: Optional (List Integer))
    it "(->)" $
      replicateA 4 (*2) 5
        `shouldBe`
          (listh [10,10,10,10])
    it "List" $ do
      let
        expected =
          listh <$> listh
            [ "aaa","aab","aac","aba","abb","abc","aca","acb","acc"
            , "baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc"
            , "caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"
            ]
      replicateA 3 ('a' :. 'b' :. 'c' :. Nil)
        `shouldBe`
          expected

  describe "filtering" $ do
    it "ExactlyOne" $
      filtering (ExactlyOne . even) (4 :. 5 :. 6 :. Nil)
        `shouldBe`
          ExactlyOne (listh [4,6])
    it "Optional - all true" $ do
      let
        predicate a =
          if a > 13
          then Empty
          else Full (a <= 7)
      filtering predicate (4 :. 5 :. 6 :. Nil)
        `shouldBe`
          Full (listh [4,5,6])
    it "Optional - some false" $ do
      let
        predicate a =
          if a > 13
          then Empty
          else Full (a <= 7)
      filtering predicate (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
        `shouldBe`
          Full (listh [4,5,6,7])
    it "Optional - some empty" $ do
      let
        predicate a =
          if a > 13
          then Empty
          else Full (a <= 7)
      filtering predicate (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
        `shouldBe`
          Empty
    it "(->)" $ do
      filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
        `shouldBe`
          listh [9,10,11,12]
    it "List" $ do
      let
        expected =
          listh <$> listh
            [ [1,2,3], [1,2,3], [1,2,3]
            , [1,2,3], [1,2,3], [1,2,3]
            , [1,2,3], [1,2,3]
            ]
      filtering (const $ True :. True :.  Nil) (1 :. 2 :. 3 :. Nil)
        `shouldBe`
          expected
--}
