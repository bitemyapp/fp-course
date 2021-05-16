{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.StateTSpec where

import qualified Prelude               as P (String, (++))

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       ((===))

import           Course.Applicative    (pure, (<*>))
import           Course.Core
import           Course.ExactlyOne     (ExactlyOne (..))
import           Course.Functor        ((<$>))
import           Course.Gens           (forAllLists)
import           Course.List           (List (..), flatMap, listh)
import           Course.Monad          ((=<<), (>>=))
import           Course.Optional       (Optional (..))
import           Course.State          (put, runState)
import           Course.StateT         (Logger (..), OptionalT (..),
                                        StateT (..), distinct', distinctF,
                                        distinctG, getT, log1, putT,
                                        runOptionalT, runState', state')

spec :: Spec
spec = do

  describe "Functor" $ do
    it "<$>" $ do
      let
        st =
          StateT (\s -> ((2, s) :. Nil))
      runStateT ((+1) <$> st) 0 `shouldBe` ((3,0) :. Nil)

  describe "Applicative" $ do
    it "List (pure)" $ runStateT ((pure 2) :: StateT Int List Int) 0 `shouldBe` ((2,0) :. Nil)
    it "List (<*>)" $ runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0 `shouldBe` ((4,0) :. Nil)
    it "Optional" $ do
      let
        st =
          StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))
      runStateT st [0] `shouldBe` Full (4,[0,1,2])
    it "List" $ do
      let
        st =
          StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil)
            <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))
      runStateT st [0] `shouldBe` ((4,[0,1,2]) :. (5,[0,1,2]) :. Nil)

  describe "Monad" $ do
    it "bind const" $ do
      let
        s n =
          StateT $ const (((), n) :. Nil)
      runStateT (const (s 2) =<< s 1) 0 `shouldBe` (((), 2) :. Nil)
    it "modify" $ do
      let
        modify f =
          StateT (\s -> pure ((), f s))
      runStateT (modify (+1) >>= \() -> modify (*2)) 7
        `shouldBe`
          (((), 16) :. Nil)

  describe "state'" $ do
    it "lifts stately functions" $
      runStateT (state' $ runState $ put 1) 0 `shouldBe` ExactlyOne ((), 1)

  describe "runState'" $ do
    it "runs identity states" $
      runState' (state' $ runState $ put 1) 0 `shouldBe` ((),1)

  describe "getTTest" $ do
    it "returns it's input" $
      runStateT (getT :: StateT Int List Int) 3 `shouldBe` ((3,3) :. Nil)

  describe "putTTest" $ do
    it "puts the state" $
      runStateT (putT 2 :: StateT Int List ()) 0 `shouldBe` (((),2) :. Nil)

  describe "distinct'" $ do
    prop "removes adjacent duplicates" $
      forAllLists $ \xs ->
        distinct' xs === distinct' (flatMap (\x -> x :. x :. Nil) xs)

  describe "distinctF" $ do
    it "Full case" $
      distinctF (listh [1,2,3,2,1]) `shouldBe` Full (listh [1,2,3])
    it "Empty case" $
      distinctF (listh [1,2,3,2,1,101]) `shouldBe` Empty

  describe "OptionalT" $ do
    it "(<$>) for OptionalT" $
      runOptionalT ((+1) <$> OptionalT (Full 1 :. Empty :. Nil))
        `shouldBe`
          (Full 2 :. Empty :. Nil)

    describe "(<*>) for OptionalT" $ do
      it "one" $ do
        let
          ot =
            OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
        runOptionalT ot `shouldBe` (Nil :: List (Optional Int))
      it "two" $ do
        let
          ot =
            OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
        runOptionalT ot `shouldBe` (Nil :: List (Optional Int))
      it "three" $ do
        let
          ot =
            OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
        runOptionalT ot `shouldBe` (Empty :. Nil :: List (Optional Int))
      it "four" $ do
        let
          ot =
            OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
        runOptionalT ot `shouldBe` (Empty :. Empty :. Nil :: List (Optional Int))
      it "five" $ do
        let
          ot =
            OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
        runOptionalT ot `shouldBe` (Empty :. Nil :: List (Optional Int))
      it "six" $ do
        let
          ot =
            OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
        runOptionalT ot `shouldBe` (Full 2 :. Full 3 :. Empty :. Nil)
      it "seven" $ do
        let
          ot =
            OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
        runOptionalT ot `shouldBe` (Full 2 :. Empty :. Full 3 :. Empty :. Nil)

    describe "OptionalT Monad" $ do
      it "(=<<) for OptionalT" $ do
        let
          func a =
            OptionalT (Full (a+1) :. Full (a+2) :. Nil)
          ot =
            func =<< OptionalT (Full 1 :. Empty :. Nil)
        runOptionalT ot `shouldBe` (Full 2:.Full 3:.Empty:.Nil)

  describe "Logger" $ do
    it "(<$>) for Logger" $
      (+3) <$> Logger (1 :. 2 :. Nil) 3 `shouldBe` Logger (1 :. 2 :. Nil) 6

    describe "Applicative" $ do
      it "pure" $
        (pure "table" :: Logger Int P.String) `shouldBe` Logger Nil "table"
      it "<*>" $
        Logger (1:.2:.Nil) (+7) <*> Logger (3:.4:.Nil) 3
          `shouldBe`
            Logger (1:.2:.3:.4:.Nil) 10

    describe "Functor" $ do
      it "(=<<) for Logger" $ do
        let
          func a =
            Logger (4:.5:.Nil) (a+3)
        (func =<< Logger (1:.2:.Nil) 3)
          `shouldBe`
            Logger (1:.2:.4:.5:.Nil) 6

  it "log1" $
    log1 1 2 `shouldBe` Logger (1:.Nil) 2

  describe "distinctG" $ do
    it "Full case" $ do
      let
        expected =
          Logger
            (listh <$> ("even number: 2":."even number: 2":."even number: 6":.Nil))
            (Full (1:.2:.3:.6:.Nil))
      distinctG (1:.2:.3:.2:.6:.Nil) `shouldBe` expected
    it "Empty case" $ do
      let
        expected =
          Logger
            (listh <$> ("even number: 2":."even number: 2":."even number: 6":."aborting > 100: 106":.Nil))
            Empty
      distinctG (listh [1,2,3,2,6,106]) `shouldBe` expected
