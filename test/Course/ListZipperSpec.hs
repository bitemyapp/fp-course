{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListZipperSpec where


import qualified Prelude                  as P (fromIntegral, (<$>))
import           Test.Hspec               (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck          ((===))
import           Test.QuickCheck.Function (Fun (..))

import           Course.Applicative       (pure, (<*>))
import           Course.Comonad           (copure)
import           Course.Core
import           Course.Extend            ((<<=))
import           Course.Functor           ((<$>))
import           Course.List              (List (..), all, isEmpty, take)
import           Course.ListZipper        (ListZipper, MaybeListZipper (..),
                                           deletePullLeft, deletePullRight,
                                           dropLefts, dropRights, end, findLeft,
                                           findRight, fromList, hasLeft,
                                           hasRight, index, insertPushLeft,
                                           insertPushRight, lefts, moveLeft,
                                           moveLeftLoop, moveLeftN, moveLeftN',
                                           moveRight, moveRightLoop, moveRightN,
                                           moveRightN', nth, rights, setFocus,
                                           start, swapLeft, swapRight, toList,
                                           toListZ, toOptional, withFocus,
                                           zipper, (-<<))
import           Course.Optional          (Optional (Empty, Full))
import           Course.Traversable       (traverse)

import           Course.Gens              (forAllListZipper,
                                           forAllListZipperWithInt, forAllLists,
                                           forAllListsAndBool)
spec :: Spec
spec = do
  describe "Functor" $ do
    it "ListZipper (<$>)" $
      (+1) <$> zipper [3,2,1] 4 [5,6,7] `shouldBe` zipper [4,3,2] 5 [6,7,8]

  describe "Functor Maybe" $ do
    it "MaybeListZipper (<$>)" $
      (+1) <$> IsZ (zipper [3,2,1] 4 [5,6,7]) `shouldBe` IsZ (zipper [4,3,2] 5 [6,7,8])

  describe "toList" $ do
    it "Optional empty list" $
      toList <$> Empty `shouldBe` (Empty :: Optional (List Int))
    it "empty left" $
      toList (zipper [] 1 [2,3,4]) `shouldBe` (1:.2:.3:.4:.Nil)
    it "lefts and rights" $
      toList (zipper [3,2,1] 4 [5,6,7]) `shouldBe` (1:.2:.3:.4:.5:.6:.7:.Nil)

  describe "fromList" $ do
    it "non-empty" $ fromList (1 :. 2 :. 3 :. Nil) `shouldBe` IsZ (zipper [] 1 [2,3])
    it "empty" $ fromList Nil `shouldBe` (IsNotZ :: MaybeListZipper Integer)
    prop "round trip" $
      forAllLists (\xs -> toListZ (fromList xs) === xs)

  describe "toOptional" $ do
    prop "empty" $
      forAllLists $ \xs ->
        isEmpty xs === (toOptional (fromList xs) == Empty)

  describe "withFocus" $ do
    it "empty left" $
      withFocus (+1) (zipper [] 0 [1]) `shouldBe` zipper [] 1 [1]
    it "left and right" $
      withFocus (+1) (zipper [1,0] 2 [3,4]) `shouldBe` zipper [1,0] 3 [3,4]

  describe "setFocus" $ do
    it "empty left" $
      setFocus 1 (zipper [] 0 [1]) `shouldBe` zipper [] 1 [1]
    it "left and right" $
      setFocus 1 (zipper [1,0] 2 [3,4]) `shouldBe` zipper [1,0] 1 [3,4]

  describe "hasLeft" $ do
    it "left and right" $ hasLeft (zipper [1,0] 2 [3,4]) `shouldBe` True
    it "empty left" $ hasLeft (zipper [] 0 [1,2]) `shouldBe` False

  describe "hasRight" $ do
    it "left and right" $ hasRight (zipper [1,0] 2 [3,4]) `shouldBe` True
    it "empty right" $ hasRight (zipper [1,0] 2 []) `shouldBe` False

  describe "findLeft" $ do
    prop "missing element returns IsNotZ" $
      forAllListsAndBool (\(xs, p) -> findLeft (const p) -<< fromList xs === IsNotZ)
    it "found in left" $
      findLeft (== 1) (zipper [2,1] 3 [4,5]) `shouldBe` IsZ (zipper [] 1 [2,3,4,5])
    it "not found" $
      findLeft (== 6) (zipper [2,1] 3 [4,5]) `shouldBe` IsNotZ
    it "one match in left" $
      findLeft (== 1) (zipper [2,1] 1 [4,5]) `shouldBe` IsZ (zipper [] 1 [2,1,4,5])
    it "multiple matches in left" $
      findLeft (== 1) (zipper [1,2,1] 3 [4,5]) `shouldBe` IsZ (zipper [2,1] 1 [3,4,5])
    it "elements shifted to right correctly" $
      findLeft (== 1) (zipper [3,4,1,5] 9 [2,7]) `shouldBe` IsZ (zipper [5] 1 [4,3,9,2,7])

  describe "findRight" $ do
    prop "missing element returns IsNotZ" $
      forAllLists (\xs -> findRight (const False) -<< fromList xs === IsNotZ)
    it "found in right" $
      findRight (== 5) (zipper [2,1] 3 [4,5]) `shouldBe` IsZ (zipper [4,3,2,1] 5 [])
    it "not found" $
      findRight (== 6) (zipper [2,1] 3 [4,5]) `shouldBe` IsNotZ
    it "one match in right" $
      findRight (== 1) (zipper [2,3] 1 [4,5,1]) `shouldBe` IsZ (zipper [5,4,1,2,3] 1 [])
    it "multiple matches in right" $
      findRight (== 1) (zipper [2,3] 1 [1,4,5,1]) `shouldBe` IsZ (zipper [1,2,3] 1 [4,5,1])

  describe "moveLeftLoop" $ do
    it "with left" $
      moveLeftLoop (zipper [3,2,1] 4 [5,6,7]) `shouldBe` zipper [2,1] 3 [4,5,6,7]
    it "empty left" $
      moveLeftLoop (zipper [] 1 [2,3,4]) `shouldBe` zipper [3,2,1] 4 []

  describe "moveRightLoop" $ do
    it "with right" $
      moveRightLoop (zipper [3,2,1] 4 [5,6,7]) `shouldBe` zipper [4,3,2,1] 5 [6,7]
    it "empty right" $
      moveRightLoop (zipper [3,2,1] 4 []) `shouldBe` zipper [] 1 [2,3,4]

  describe "moveLeft" $ do
    it "with left" $
      moveLeft (zipper [3,2,1] 4 [5,6,7]) `shouldBe` IsZ (zipper [2,1] 3 [4,5,6,7])
    it "empty left" $
      moveLeft (zipper [] 1 [2,3,4]) `shouldBe` IsNotZ

  describe "moveRight" $ do
    it "with right" $
      moveRight (zipper [3,2,1] 4 [5,6,7]) `shouldBe` IsZ (zipper [4,3,2,1] 5 [6,7])
    it "empty right" $
      moveRight (zipper [3,2,1] 4 []) `shouldBe` IsNotZ

  describe "swapLeft" $ do
    it "with left" $
      swapLeft (zipper [3,2,1] 4 [5,6,7]) `shouldBe` IsZ (zipper [4,2,1] 3 [5,6,7])
    it "empty left" $
      swapLeft (zipper [] 1 [2,3,4]) `shouldBe` IsNotZ

  describe "swapRight" $ do
    it "with right" $
      swapRight (zipper [3,2,1] 4 [5,6,7]) `shouldBe` IsZ (zipper [3,2,1] 5 [4,6,7])
    it "empty right" $
      swapRight (zipper [3,2,1] 4 []) `shouldBe` IsNotZ

  describe "dropLeft" $ do
    it "with left" $
      dropLefts (zipper [3,2,1] 4 [5,6,7]) `shouldBe` zipper [] 4 [5,6,7]
    it "empty left" $
      dropLefts (zipper [] 1 [2,3,4]) `shouldBe` zipper [] 1 [2,3,4]
    prop "dropLefts empties left of zipper"
      (\l x r -> dropLefts (zipper l x r) === (zipper [] x r :: ListZipper Integer))

  describe "dropRights" $ do
    it "with right" $
      dropRights (zipper [3,2,1] 4 [5,6,7]) `shouldBe` zipper [3,2,1] 4 []
    it "empty right" $
      dropRights (zipper [3,2,1] 4 []) `shouldBe` zipper [3,2,1] 4 []
    prop "dropRights empties right of zipper"
      (\l x r -> dropRights (zipper l x r) === (zipper l x [] :: ListZipper Integer))

  describe "moveLeftN" $ do
    it "positive moves" $
      moveLeftN 2 (zipper [2,1,0] 3 [4,5,6]) `shouldBe` IsZ (zipper [0] 1 [2,3,4,5,6])
    it "negative moves" $
      moveLeftN (-1) (zipper [2,1,0] 3 [4,5,6]) `shouldBe` IsZ (zipper [3,2,1,0] 4 [5,6])

  describe "moveRightN" $ do
    it "positive moves" $
      moveRightN 1 (zipper [2,1,0] 3 [4,5,6]) `shouldBe` IsZ (zipper [3,2,1,0] 4 [5,6])
    it "negative moves" $
      moveRightN (-1) (zipper [2,1,0] 3 [4,5,6]) `shouldBe` IsZ (zipper [1,0] 2 [3,4,5,6])

  describe "moveLeftN'" $ do
    it "positive - out of bounds both sides" $
      moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7]) `shouldBe` Left 3
    it "positive in range" $
      moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7]) `shouldBe` Right (zipper [2,1] 3 [4,5,6,7])
    prop "moving zero is `Right . id`"
      (\l x r -> let lz = zipper l x r :: ListZipper Integer
                  in moveLeftN' 0 lz === (Right . id $ lz))
    it "negative in range" $
      moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7]) `shouldBe` Right (zipper [5,4,3,2,1] 6 [7])
    it "negative out of bounds" $
      moveLeftN' (-4 ) (zipper [3,2,1] 4 [5,6,7]) `shouldBe` Left 3
    it "positive - out of bounds on left only" $
      moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9]) `shouldBe` Left 3
    it "negative - out of bounds on right only" $
      moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9]) `shouldBe` Left 3

  describe "moveRightN'" $ do
    it "positive - out of bounds both sides" $
      moveRightN' 4 (zipper [3,2,1] 4 [5,6,7]) `shouldBe` Left 3
    it "positive in range" $
      moveRightN' 1 (zipper [3,2,1] 4 [5,6,7]) `shouldBe` Right (zipper [4,3,2,1] 5 [6,7])
    prop "moving zero is `Right . id`"
      (\l x r -> let lz = (zipper l x r :: ListZipper Integer) in moveRightN' 0 lz === (Right . id $ lz))
    it "negative in range" $
      moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7]) `shouldBe` Right (zipper [1] 2 [3,4,5,6,7])
    it "negative - out of bounds both sides" $
      moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7]) `shouldBe` Left 3

  describe "nth" $ do
    it "have 1"    $ nth 1 (zipper [3,2,1] 4 [5,6,7]) `shouldBe` IsZ (zipper [1] 2 [3,4,5,6,7])
    it "have 5"    $ nth 5 (zipper [3,2,1] 4 [5,6,7]) `shouldBe` IsZ (zipper [5,4,3,2,1] 6 [7])
    it "missing 8" $ nth 8 (zipper [3,2,1] 4 [5,6,7]) `shouldBe` IsNotZ

  describe "index" $ do
    it "index works" $ index (zipper [3,2,1] 4 [5,6,7]) `shouldBe` 3
    prop "Always returns the index on a valid zipper" $
      forAllListZipperWithInt $ \(z,i) ->
        optional True (\z' -> index z' == i) (toOptional (nth i z))

  describe "end" $ do
    it "end" $ end (zipper [3,2,1] 4 [5,6,7]) `shouldBe` zipper [6,5,4,3,2,1] 7 []
    prop "end never changes the zipper's contents" $
      forAllListZipper (\z -> toList z === toList (end z))
    prop "never have rights after calling end" $
      forAllListZipper (\z -> rights (end z) === Nil)

  describe "start" $ do
    it "start" $ start (zipper [3,2,1] 4 [5,6,7]) `shouldBe` zipper [] 1 [2,3,4,5,6,7]
    prop "start never changes the zipper's contents" $
      forAllListZipper (\z -> toList z === toList (start z))
    prop "never have lefts after calling start" $
      forAllListZipper (\z -> lefts (start z) === Nil)

  describe "deletePullLeft" $ do
    it "non-empty lefts" $ deletePullLeft (zipper [3,2,1] 4 [5,6,7]) `shouldBe` IsZ (zipper [2,1] 3 [5,6,7])
    it "empty lefts" $ deletePullLeft (zipper [] 1 [2,3,4]) `shouldBe` IsNotZ

  describe "deletePullRight" $ do
    it "non-empty rights" $ deletePullRight (zipper [3,2,1] 4 [5,6,7]) `shouldBe` IsZ (zipper [3,2,1] 5 [6,7])
    it "empty rights" $ deletePullRight (zipper [3,2,1] 4 []) `shouldBe` IsNotZ

  describe "insertPushLeft" $ do
    it "non-empty lefts" $
      insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7]) `shouldBe` zipper [4,3,2,1] 15 [5,6,7]
    it "empty lefts" $
      insertPushLeft 15 (zipper [] 1 [2,3,4]) `shouldBe` zipper [1] 15 [2,3,4]
    prop "deletePullLeft . insertPushLeft === id" $
      forAllListZipperWithInt $ \(z,i) ->
        optional
          False
          (== z)
          (toOptional (deletePullLeft (insertPushLeft (P.fromIntegral i) z)))

  describe "insertPushRight" $ do
    it "non-empty rights" $
      insertPushRight 15 (zipper [3,2,1] 4 [5,6,7]) `shouldBe` zipper [3,2,1] 15 [4,5,6,7]
    it "empty rights" $
      insertPushRight 15 (zipper [3,2,1] 4 []) `shouldBe` zipper [3,2,1] 15 [4]
    prop "deletePullRight . insertPushRight === id" $
      forAllListZipperWithInt $ \(z,i) ->
        optional
          False
          (== z)
          (toOptional (deletePullRight (insertPushRight (P.fromIntegral i) z)))

  describe "Applicative" $ do
    prop "pure produces infinite lefts"
      (\a n -> (all . (==) <*> take (n :: Int) . lefts . pure) (a :: Integer))
    prop "pure produces infinite rights"
      (\a n -> (all . (==) <*> take (n :: Int) . rights . pure) (a :: Integer))
    it "<*> applies functions to corresponding elements in zipper" $
      zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7] `shouldBe` zipper [5,12] 8 [15,24,12]

  let
    is (IsZ z) = z
    is _       = error "MaybeListZipper's Applicative instances is busted"
    notZ       = IsNotZ :: MaybeListZipper Integer

  describe "Applicative (MaybeListZipper)" $ do
    prop "pure produces infinite lefts"
      (\a n -> (all . (==) <*> take (n :: Int) . lefts . is . pure) (a :: Integer))
    prop "pure produces infinite rights"
      (\a n -> (all . (==) <*> take (n :: Int) . rights . is . pure) (a :: Integer))
    it "IsZ <*> IsZ" $
      let z = IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsZ (zipper [3,2,1] 4 [5,6,7])
       in z `shouldBe` IsZ (zipper [5,12] 8 [15,24,12])
    prop "IsNotZ <*> IsZ" $
      let fs = (IsNotZ :: MaybeListZipper (Integer -> Integer))
       in forAllListZipper (\z -> (fs <*> IsZ z) === IsNotZ)
    prop "IsZ <*> IsNotZ"
      (\(Fun _ f) -> (IsZ (pure f) <*> notZ) === notZ)
    it "IsNotZ <*> IsNotZ" $
      IsNotZ <*> notZ `shouldBe` notZ

  describe "Extend" $ do
    it "zipper o' zippers" $ do
      let
        z = zipper [2,1] 3 [4,5]
        l = [zipper [1] 2 [3,4,5], zipper [] 1 [2,3,4,5]]
        r = [zipper [3,2,1] 4 [5], zipper [4,3,2,1] 5 []]
      (id <<= z) `shouldBe` zipper l z r

  describe "Extend (MaybeListZipper)" $ do
    it "IsNotZ" $
      (id <<= IsNotZ) `shouldBe` (IsNotZ :: MaybeListZipper (MaybeListZipper Integer))
    it "IsZ" $ do
      let
        z = IsZ (zipper [2,1] 3 [4,5])
        l = IsZ P.<$> [zipper [1] 2 [3,4,5], zipper [] 1 [2,3,4,5]]
        r = IsZ P.<$> [zipper [3,2,1] 4 [5], zipper [4,3,2,1] 5 []]
      (id <<= z) `shouldBe` IsZ (zipper l z r)

  describe "Comonad" $ do
    it "copure" $ copure (zipper [2,1] 3 [4,5]) `shouldBe` 3

  describe "Traversable" $ do
    prop "All Full" $
      forAllListZipper (\z -> traverse id (Full <$> z) === Full z)
    it "One Empty" $
      traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7])
        `shouldBe`
          Empty

  describe "Traversable (MaybeListZipper)" $ do
    it "IsNotZ" $
      traverse id IsNotZ `shouldBe` (Full IsNotZ :: Optional (MaybeListZipper Integer))
    prop "IsZ Full" $
      forAllListZipper (\z -> traverse id (Full <$> IsZ z) === Full (IsZ z))

optional :: b -> (a -> b) -> Optional a -> b
optional e _ Empty    = e
optional _ f (Full a) = f a
