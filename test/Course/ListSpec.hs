{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListSpec where

import qualified Prelude               as P (length)

import           Test.QuickCheck       (forAllShrink)
import           Test.Hspec.QuickCheck (prop)
import Test.Hspec (describe, it, shouldBe, Spec)

import           Course.Core
import           Course.Gens           (forAllLists, genIntegerAndList, genList,
                                        genListOfLists, genThreeLists,
                                        genTwoLists, shrinkIntegerAndList,
                                        shrinkList, shrinkListOfLists,
                                        shrinkThreeLists, shrinkTwoLists)
import           Course.List           (List (..), filter, find, flatMap,
                                        flatten, flattenAgain, foldLeft, headOr,
                                        hlist, infinity, largeList, length,
                                        lengthGT4, listh, map, produce, product,
                                        reverse, seqOptional, sum, take, (++))
import           Course.Optional       (Optional (..))

spec :: Spec
spec = do
  describe "headOr" $ do
    it "headOr on non-empty list" $ headOr 3 (1 :. 2 :. Nil) `shouldBe` 1
    it "headOr on empty list" $ headOr 3 Nil `shouldBe` 3
    prop "headOr on infinity always 0" $ \x -> x `headOr` infinity == 0
    prop "headOr on empty list always the default" $ \x -> x `headOr` Nil == (x :: Integer)

  describe "productTest" $ do
    it "product of empty list" $ product Nil `shouldBe` 1
    it "product of 1..3" $ product (1 :. 2 :. 3 :. Nil) `shouldBe` 6
    it "product of 1..4" $ product (1 :. 2 :. 3 :. 4 :. Nil) `shouldBe` 24

  describe "sum" $ do
    it "sum 1..3" $ sum (1 :. 2 :. 3 :. Nil) `shouldBe` 6
    it "sum 1..4" $ sum (1 :. 2 :. 3 :. 4 :. Nil) `shouldBe` 10
    prop "subtracting each element in a list from its sum is always 0" $
      forAllShrink genList shrinkList (\x -> foldLeft (-) (sum x) x == 0)

  describe "length" $ do
    it "length 1..3" $ length (1 :. 2 :. 3 :. Nil) `shouldBe` 3
    prop "summing a list of 1s is equal to its length" $
      forAllLists (\x -> P.length (hlist x) == length x)

  describe "map" $ do
    it "add 10 on list" $
      map (+10) (1 :. 2 :. 3 :. Nil) `shouldBe` (11 :. 12 :. 13 :. Nil)
    prop "headOr after map" $
      \x -> headOr (x :: Integer) (map (+1) infinity) == 1
    prop "map id is id" $
      forAllLists (\x -> map id x == x)

  describe "filter" $ do
    it "filter even" $
      filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldBe` (2 :. 4 :. Nil)
    prop "filter (const True) is identity (headOr)" $
      \x -> headOr x (filter (const True) infinity) == 0
    prop "filter (const True) is identity" $
      forAllLists (\x -> filter (const True) x == x)
    prop "filter (const False) is the empty list" $
      forAllLists (\x -> filter (const False) x == Nil)

  describe "(++)" $ do
    it "(1..6)" $
      (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil) `shouldBe` listh [1,2,3,4,5,6]
    prop "append empty to infinite" $
      \x -> headOr x (Nil ++ infinity) == 0
    prop "append anything to infinity" $
       forAllShrink genIntegerAndList shrinkIntegerAndList (\(x, y) -> headOr x (y ++ infinity) == headOr 0 y)
    prop "associativity" $
      forAllShrink genThreeLists shrinkThreeLists (\(x,y,z) -> (x ++ y) ++ z == x ++ (y ++ z))
    prop "append to empty list" $
      forAllLists (\x -> x ++ Nil == x)

  describe "flatten" $ do
    it "(1..9)" $
      flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil) `shouldBe` listh [1,2,3,4,5,6,7,8,9]
    prop "flatten (infinity :. y)" $
      forAllShrink genIntegerAndList shrinkIntegerAndList (\(x, y) -> headOr x (flatten (infinity :. y :. Nil)) == 0)
    prop "flatten (y :. infinity)" $
      forAllShrink genIntegerAndList shrinkIntegerAndList (\(x, y) -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y)
    prop "sum of lengths == length of flattened" $
      forAllShrink genListOfLists shrinkListOfLists (\x -> sum (map length x) == length (flatten x))

  describe "flatMap" $ do
    it "lists of Integer" $
      flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil) `shouldBe` listh [1,2,3,2,3,4,3,4,5]
    prop "flatMap id flattens a list of lists" $
      forAllShrink genIntegerAndList shrinkIntegerAndList (\(x, y) -> headOr x (flatMap id (infinity :. y :. Nil)) == 0)
    prop "flatMap id on a list of lists take 2" $
      forAllShrink genIntegerAndList shrinkIntegerAndList (\(x, y) -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y)
    prop "flatMap id == flatten" $
      forAllShrink genListOfLists shrinkListOfLists (\x -> flatMap id x == flatten x)

  describe "flattenAgain" $ do
    prop "lists of Integer" $
      forAllShrink genListOfLists shrinkListOfLists (\x -> flatten x == flattenAgain x)

  describe "seqOptional" $ do
    it "all Full" $
      seqOptional (Full 1 :. Full 10 :. Nil) `shouldBe` Full (1 :. 10 :. Nil)
    it "empty list" $
      let empty = Nil :: List (Optional Integer)
       in seqOptional empty `shouldBe` Full Nil
    it "contains Empty" $
      seqOptional (Full 1 :. Full 10 :. Empty :. Nil) `shouldBe` Empty
    it "Empty at head of infinity" $
      seqOptional (Empty :. map Full infinity) `shouldBe` Empty

  describe "find" $ do
    it "find no matches" $
      find even (1 :. 3 :. 5 :. Nil) `shouldBe` Empty
    it "empty list" $ find even Nil `shouldBe` Empty
    it "find only even" $
      find even (1 :. 2 :. 3 :. 5 :. Nil) `shouldBe` Full 2
    it "find first, not second even" $
      find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldBe` Full 2
    it "find on infinite list" $
      find (const True) infinity `shouldBe` Full 0

  describe "lengthGT4" $ do
    it "list of length 3" $
      lengthGT4 (1 :. 3 :. 5 :. Nil) `shouldBe` False
    it "empty list" $
      lengthGT4 Nil `shouldBe` False
    it "list of length 5" $
      lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldBe` True
    it "infinite list" $
      lengthGT4 infinity `shouldBe` True

  describe "reverse" $ do
    it "empty list" $
      reverse Nil `shouldBe` (Nil :: List Integer)
    it "reverse . reverse on largeList" $
      take 1 (reverse (reverse largeList)) `shouldBe` (1 :. Nil)
    prop "reverse then append is same as append then reverse" $
      forAllShrink genTwoLists shrinkTwoLists (\(x, y) -> reverse x ++ reverse y == reverse (y ++ x))
    prop "" $
      forAllLists (\x -> reverse (x :. Nil) == x :. Nil)

  describe "produce" $ do
    it "increment" $
      let (x:.y:.z:.w:._) = produce (+1) 0
       in (x:.y:.z:.w:.Nil) `shouldBe` (0:.1:.2:.3:.Nil)
    it "double" $
      let (x:.y:.z:.w:._) = produce (*2) 1
       in (x:.y:.z:.w:.Nil) `shouldBe` (1:.2:.4:.8:.Nil)
