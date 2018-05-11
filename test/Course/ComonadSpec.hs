{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ComonadSpec where


import           Test.Hspec               (Spec, it, shouldBe)

import           Course.Comonad    (copure, (<$$>))
import           Course.Core
import           Course.ExactlyOne (ExactlyOne (..))

spec :: Spec
spec = do
  it "ExactlyOne" $ copure (ExactlyOne 7) `shouldBe` 7

  it "<$$>" $
    ((+10) <$$> ExactlyOne 7) `shouldBe` ExactlyOne 17
