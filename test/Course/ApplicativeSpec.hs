{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Course.ApplicativeSpec where

import           Control.Monad (Monad)
import           Data.String   (String)
import           Prelude       (replicate)

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
tests = checkParallel $$(discover)

genInt :: Monad m => PropertyT m Int
genInt = forAll $ Gen.int (Range.constant 0 100)

genStr :: Monad m => PropertyT m String
genStr = forAll $ Gen.string (Range.constant 0 5) Gen.alphaNum

----------------------------------------------------------------------
-- Applicative instance tests
----------------------------------------------------------------------
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

----------------------------------------------------------------------
-- lift2 tests
----------------------------------------------------------------------
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

----------------------------------------------------------------------
-- lift3 tests
----------------------------------------------------------------------
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
      === Full (x + y + z)

prop_lift3PlusOverLastEmpty :: Property
prop_lift3PlusOverLastEmpty =
  property $ do
    x <- genInt
    y <- genInt
    lift3 (\a b c -> a + b + c) (Full x) (Full y) Empty
      === Empty

prop_lift3PlusOverFirstEmpty :: Property
prop_lift3PlusOverFirstEmpty =
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

----------------------------------------------------------------------
-- lift4 tests
----------------------------------------------------------------------
prop_lift4PlusOverExactlyOne :: Property
prop_lift4PlusOverExactlyOne =
  property $ do
    w <- genInt
    x <- genInt
    y <- genInt
    z <- genInt
    lift4 (\a b c d -> a + b + c + d)
          (ExactlyOne w)
          (ExactlyOne x)
          (ExactlyOne y)
          (ExactlyOne z)
      === ExactlyOne (w + x + y + z)

prop_lift4PlusOverList :: Property
prop_lift4PlusOverList =
  property $
    lift4 (\a b c d -> a + b + c + d)
          (listh [1, 2, 3])
          (listh [4, 5])
          (listh [6, 7, 8])
          (listh [9, 10])
      === listh [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]

prop_lift4PlusOverThreeOptionals :: Property
prop_lift4PlusOverThreeOptionals =
  property $ do
    w <- genInt
    x <- genInt
    y <- genInt
    z <- genInt
    lift4 (\a b c d -> a + b + c + d) (Full w) (Full x) (Full y) (Full z)
      === Full (w + x + y + z)

prop_lift4PlusOverThirdEmpty :: Property
prop_lift4PlusOverThirdEmpty =
  property $ do
    x <- genInt
    y <- genInt
    z <- genInt
    lift4 (\a b c d -> a + b + c + d) (Full x) (Full y) Empty (Full z)
      === Empty

prop_lift4PlusOverFirstEmpty :: Property
prop_lift4PlusOverFirstEmpty =
  property $ do
    x <- genInt
    y <- genInt
    z <- genInt
    lift4 (\a b c d -> a + b + c + d) Empty (Full x) (Full y) (Full z)
      === Empty

prop_lift4PlusOverTwoEmpty :: Property
prop_lift4PlusOverTwoEmpty =
  property $ do
    x <- genInt
    y <- genInt
    lift4 (\a b c d -> a + b + c + d) Empty Empty (Full x) (Full y)
      === Empty

prop_lift4PlusOverFunctions :: Property
prop_lift4PlusOverFunctions =
  property $
    lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) (listh [4, 5, 6])
      === 148

----------------------------------------------------------------------
-- (*>) rightApply tests
----------------------------------------------------------------------
prop_rightApplyOverList1 :: Property
prop_rightApplyOverList1 =
  property $
    (listh [1, 2, 3] *> listh [4, 5, 6]) === listh [4, 5, 6, 4, 5, 6, 4, 5, 6]

prop_rightApplyOverList2 :: Property
prop_rightApplyOverList2 =
  property $
    (listh [1, 2] *> listh [4, 5, 6]) === listh [4, 5, 6, 4, 5, 6]

prop_rightApplyOverList3 :: Property
prop_rightApplyOverList3 =
  property $
    (listh [1, 2, 3] *> listh [4, 5]) === listh [4, 5, 4, 5, 4, 5]

prop_rightApplyOverListGeneric :: Property
prop_rightApplyOverListGeneric =
  property $ do
    a <- genInt
    b <- genInt
    c <- genInt
    x <- genInt
    y <- genInt
    z <- genInt
    let l1 = listh [a,  b,  c]
        l2 = listh [x,  y,  z]
     in (l1 *> l2) === listh [x,  y,  z,  x,  y,  z,  x,  y,  z]

prop_rightApplyOverOptional :: Property
prop_rightApplyOverOptional =
  property $
    (Full 7 *> Full 8) === Full 8

prop_rightApplyOverOptionalGeneric :: Property
prop_rightApplyOverOptionalGeneric =
  property $ do
    x <- genInt
    y <- genInt
    (Full x *> Full y) === Full y


----------------------------------------------------------------------
-- (<*) leftApply tests
----------------------------------------------------------------------
prop_leftApplyOverList1 :: Property
prop_leftApplyOverList1 =
  property $
    (listh [1, 2, 3] <* listh [4, 5, 6]) === listh [1, 1, 1, 2, 2, 2, 3, 3, 3]

prop_leftApplyOverList2 :: Property
prop_leftApplyOverList2 =
  property $
    (listh [1, 2] <* listh [4, 5, 6]) === listh [1, 1, 1, 2, 2, 2]

prop_leftApplyOverList3 :: Property
prop_leftApplyOverList3 =
  property $
    (listh [1, 2, 3] <* listh [4, 5]) === listh [1, 1, 2, 2, 3, 3]

prop_leftApplyOverListGeneric :: Property
prop_leftApplyOverListGeneric =
  property $ do
    a <- genInt
    b <- genInt
    c <- genInt
    x <- genInt
    y <- genInt
    z <- genInt
    let l1 = listh [a,  b,  c]
        l2 = listh [x,  y,  z]
     in (l1 *> l2) === listh [a, a, a, b, b, b, c, c, c]

prop_leftApplyOverOptional :: Property
prop_leftApplyOverOptional =
  property $
    (Full 7 *> Full 8) === Full 7

prop_leftApplyOverOptionalGeneric :: Property
prop_leftApplyOverOptionalGeneric =
  property $ do
    x <- genInt
    y <- genInt
    (Full x *> Full y) === Full x


----------------------------------------------------------------------
-- sequence tests
----------------------------------------------------------------------
prop_sequenceExactlyOne :: Property
prop_sequenceExactlyOne =
  property $
    sequence (listh [ExactlyOne 7, ExactlyOne 8, ExactlyOne 9])
      === ExactlyOne (listh [7, 8, 9])

prop_sequenceList :: Property
prop_sequenceList =
  property $
    sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
      === (listh <$> listh [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]])

prop_sequenceOptionalWithEmpty :: Property
prop_sequenceOptionalWithEmpty =
  property $
    sequence (listh [Full 7, Empty]) === Empty

prop_sequenceOptional :: Property
prop_sequenceOptional =
  property $
    sequence (listh [Full 7, Full 8]) === Full (listh [7, 8])

prop_sequenceFunction :: Property
prop_sequenceFunction =
  property $
    sequence (listh [(*10), (+2)]) 6 === listh [60, 8]

----------------------------------------------------------------------
-- replicateA tests
----------------------------------------------------------------------

prop_replicatAOverExactlyOne :: Property
prop_replicatAOverExactlyOne =
  property $ do
    s <- genStr
    l <- genInt
    replicateA l (ExactlyOne s) === ExactlyOne (listh $ replicate l s)

prop_replicatAOverFull :: Property
prop_replicatAOverFull =
  property $ do
    s <- genStr
    l <- genInt
    replicateA l (Full s) === Full (listh $ replicate l s)

prop_replicatAOverEmpty :: Property
prop_replicatAOverEmpty =
  property $ do
    l <- genInt
    replicateA l Empty === (Empty :: Optional (List String))

prop_replicateAOverFunction :: Property
prop_replicateAOverFunction =
  property $ do
    l <- genInt
    replicateA l (*2) 5 === (listh $ replicate l (5 * 2))

prop_replicateAOverList :: Property
prop_replicateAOverList =
  let expected =
        listh <$> listh
          [ "aaa","aab","aac","aba","abb","abc","aca","acb","acc"
          , "baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc"
          , "caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"
          ]
  in property $ replicateA 3 ('a' :. 'b' :. 'c' :. Nil) === expected

prop_filteringExactlyOne :: Property
prop_filteringExactlyOne =
  property $
    filtering (ExactlyOne . even) (listh [4, 5, 6])
      === ExactlyOne (listh [4, 6])

prop_filteringOptional1 :: Property
prop_filteringOptional1 =
  let pred a = if a > 13
               then Empty
               else Full (a <= 7)
  in property $
    filtering pred (listh [4, 5, 6]) === Full (listh [4, 5, 6])

prop_filteringOptional2 :: Property
prop_filteringOptional2 =
  let pred a = if a > 13
               then Empty
               else Full (a <= 7)
  in property $
    filtering pred (listh [4, 5, 6, 7, 8, 9]) === Full (listh [4, 5, 6, 7])

prop_filteringOptional3 :: Property
prop_filteringOptional3 =
  let pred a = if a > 13
               then Empty
               else Full (a <= 7)
  in property $
    filtering pred (listh [4, 5, 6, 13, 14]) === Empty

prop_filteringFunction :: Property
prop_filteringFunction =
  property $
    filtering (>) (listh [4..12]) 8 === listh [9, 10, 11, 12]

prop_filteringList :: Property
prop_filteringList =
  let expected =
        listh <$> listh
          [ [1,2,3], [1,2,3], [1,2,3]
          , [1,2,3], [1,2,3], [1,2,3]
          , [1,2,3], [1,2,3]
          ]
  in property $
    filtering (const $ listh [True, True]) (listh [1, 2, 3])
      === expected
