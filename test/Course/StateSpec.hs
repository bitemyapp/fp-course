{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.StateSpec where

import           Data.List                (nub)
import qualified Prelude                  as P ((++))

import           Test.Hspec               (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck.Function (Fun (..))

import           Course.Applicative       (pure, (<*>))
import           Course.Core
import           Course.Functor           ((<$>))
import           Course.Gens              (forAllLists)
import           Course.List              (List (..), filter, flatMap, hlist,
                                           length, listh, span, (++))
import           Course.Monad
import           Course.Optional          (Optional (..))
import           Course.State             (State (..), distinct, eval, exec,
                                           findM, firstRepeat, get, isHappy,
                                           put, runState)

spec :: Spec
spec = do
  describe "State methods" $ do
    prop "exec" $
      \(Fun _ f :: Fun Integer (Integer, Integer)) s -> exec (State f) s == snd (runState (State f) s)

    prop "eval" $
      \(Fun _ f :: Fun Integer (Integer, Integer)) s -> eval (State f) s == fst (runState (State f) s)

    it "get" $ runState get 0 `shouldBe` (0,0)

    it "put" $ runState (put 1) 0 `shouldBe` ((),1)

    it "(<$>)" $
      runState ((+1) <$> State (\s -> (9, s * 2))) 3 `shouldBe` (10,6)

  describe "Applicative" $ do
    it "pure" $ runState (pure 2) 0 `shouldBe` (2,0)
    it "<*>" $ runState (pure (+1) <*> pure 0) 0 `shouldBe` (1,0)
    it "complicated <*>" $
      let state = State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))
       in runState state [] `shouldBe` (10,["apple","banana"])

  describe "Monad" $ do
    it "(=<<)" $
      runState ((const $ put 2) =<< put 1) 0 `shouldBe` ((),2)
    it "" $
      runState ((\a -> State (\s -> (a + s, 10 + s))) =<< State (\s -> (s * 2, 4 + s))) 2 `shouldBe` (10, 16)
    it "(>>=)" $
      let modify f = State (\s -> ((), f s))
       in runState (modify (+1) >>= \() -> modify (*2)) 7  `shouldBe` ((),16)

  describe "findM" $ do
    it "find 'c' in 'a'..'h'" $
      let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get
       in runState (findM p $ listh ['a'..'h']) 0 `shouldBe` (Full 'c',3)
    it "find 'i' in 'a'..'h'" $
      let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get
       in runState (findM p $ listh ['a'..'h']) 0 `shouldBe` (Empty,8)

  describe "firstRepeat" $ do
    prop "finds repeats" $ forAllLists $ \xs ->
      case firstRepeat xs of
        Empty ->
          let xs' = hlist xs
           in nub xs' == xs'
        Full x -> length (filter (== x) xs) > 1
    prop "" $ forAllLists $ \xs ->
      case firstRepeat xs of
        Empty -> True
        Full x ->
          let
            (l, (rx :. rs)) = span (/= x) xs
            (l2, _) = span (/= x) rs
            l3 = hlist (l ++ (rx :. Nil) ++ l2)
          in
            nub l3 == l3

  describe "distinct" $ do
    prop "No repeats after distinct" $
      forAllLists (\xs -> firstRepeat (distinct xs) == Empty)
    prop "" $
      forAllLists (\xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs))

  describe "isHappy" $ do
    it "4" $ isHappy 4 `shouldBe` False
    it "7" $ isHappy 7 `shouldBe` True
    it "42" $ isHappy 42 `shouldBe`  False
    it "44" $ isHappy 44 `shouldBe`  True
