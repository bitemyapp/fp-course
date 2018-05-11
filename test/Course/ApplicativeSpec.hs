{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ApplicativeSpec where

import           Test.Hspec            (describe, it, Spec, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       ((===))

import           Course.Applicative    (filtering, lift1, lift2, lift3, lift4,
                                        pure, replicateA, sequence, (*>), (<*),
                                        (<*>))
import           Course.Core
import           Course.ExactlyOne     (ExactlyOne (..))
import           Course.Functor        ((<$>))
import           Course.List           (List (..), filter, length, listh,
                                        product, sum)
import           Course.Optional       (Optional (..))

spec :: Spec
spec = do
  describe  "ExactlyOne instance" $ do
    prop "pure == ExactlyOne" $ \(x :: Integer) ->
      pure x === ExactlyOne x
    it "Applying within ExactlyOne" $
      ExactlyOne (+ 10) <*> ExactlyOne 8
        `shouldBe`
          ExactlyOne 18

  describe "List instance" $ do
    prop "pure" $
      \x -> pure x === (x :. Nil :: List Integer)
    it "<*>" $
      (+1) :. (*2) :. Nil <*> listh [1,2,3]
        `shouldBe`
          listh [2,3,4,2,4,6]

  describe "lift1" $ do
    it "ExactlyOne" $
      lift1 (+ 1) (ExactlyOne 2)
        `shouldBe`
          ExactlyOne (3 :: Integer)
    it "empty List" $
      lift1 (+ 1) Nil
        `shouldBe`
          Nil
    it "List" $
      lift1 (+ 1) (listh [1,2,3])
        `shouldBe`
          listh [2,3,4]

  describe "Optional instance" $ do
    prop "pure" $
      \(x :: Integer) -> pure x === Full x
    it "Full <*> Full" $
      Full (+8) <*> Full 7
        `shouldBe`
          Full 15
    it "Empty <*> Full" $
      Empty <*> Full "tilt"
        `shouldBe`
          (Empty :: Optional Integer)
    it "Full <*> Empty" $
      Full (+8) <*> Empty
        `shouldBe`
          Empty

  describe "Function instance" $ do
    it "addition" $
      ((+) <*> (+10)) 3
        `shouldBe`
          16
    it "more addition" $
      ((+) <*> (+5)) 3
        `shouldBe`
          11
    it "even more addition" $
      ((+) <*> (+5)) 1
        `shouldBe`
          7
    it "addition and multiplication" $
      ((*) <*> (+10)) 3
        `shouldBe`
          39
    it "more addition and multiplcation" $
      ((*) <*> (+2)) 3
        `shouldBe`
          15
    prop "pure" $
      \(x :: Integer) (y :: Integer) -> pure x y == x

  describe "lift2" $ do
    it "+ over ExactlyOne" $
      lift2 (+) (ExactlyOne 7) (ExactlyOne 8)
        `shouldBe`
          ExactlyOne 15
    it "+ over List" $
      lift2 (+) (listh [1,2,3]) (listh [4,5])
        `shouldBe`
          listh [5,6,6,7,7,8]
    it "+ over Optional - all full" $
      lift2 (+) (Full 7) (Full 8)
        `shouldBe`
          Full 15
    it "+ over Optional - first Empty" $
      lift2 (+) Empty (Full 8)
        `shouldBe`
          Empty
    it "+ over Optional - second Empty" $
      lift2 (+) (Full 7) Empty
        `shouldBe`
          Empty
    it "+ over functions" $
      lift2 (+) length sum (listh [4,5,6])
        `shouldBe`
          18

  describe "lift3" $ do
    it "+ over ExactlyOne" $
      lift3 (\a b c -> a + b + c) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9)
        `shouldBe`
          ExactlyOne 24
    it "+ over List" $
      lift3 (\a b c -> a + b + c) (listh [1,2,3]) (listh [4,5]) (listh [6,7,8])
        `shouldBe`

        listh [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
    it "+ over Optional" $
      lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
        `shouldBe`
          Full 24
    it "+ over Optional - third Empty" $
      lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
        `shouldBe`
          Empty
    it "+ over Optional - first Empty" $
      lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
        `shouldBe`
          Empty
    it "+ over Optional - first and second Empty" $
      lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
        `shouldBe`
          Empty
    it "+ over functions" $
      lift3 (\a b c -> a + b + c) length sum product (listh [4,5,6])
        `shouldBe`
          138

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
