module Test.Main where

import Prelude

import Data.Date.Lunar (leapDays, solar2lunar)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
    describe "测试内部接口" do
      it "leapDays" do
        leapDays 1900 `shouldEqual` Just 29
        leapDays 1925 `shouldEqual` Just 30
        leapDays 2021 `shouldEqual` Just 0

    describe "测试接口使用" do
      it "测试solar2lunar" do
        solar2lunar 2021 10 26 `shouldEqual` Just { year : 2021, month: 9, day: 21 }