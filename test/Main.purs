module Test.Main where

import Prelude
import Effect (Effect)
import Mezgeb as Mezgeb
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

testLacks :: forall r. Mezgeb.Lacks ( hello :: Unit, world :: Unit ) r => { | r } -> Unit
testLacks = const unit

testCons :: forall r. Mezgeb.Cons ( hello :: Int, world :: Number ) r => { | r } -> Unit
testCons = const unit

a = testLacks { a: 1 } :: Unit

--fails correctly
--b = testLacks {hello:1} :: Unit
a' = testCons { hello: 1, world: 2.0 } :: Unit

--fails correctly
--b = testLacks {hello:1} :: Unit
main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "መዝገብ" do
          it "inserts" do
            Mezgeb.insert { a: 1, b: 2 } { c: 3, d: 4 } `shouldEqual` { a: 1, b: 2, c: 3, d: 4 }
          it "gets" do
            Mezgeb.get { a: unit } { a: 3, d: 4 } `shouldEqual` { a: 3 }
          it "deletes" do
            Mezgeb.delete
              { b: 0, c: 0, d: 0 }
              { a: 1, b: 2, c: true, d: false, e: unit, f: [ 1 ], g: 5 }
              `shouldEqual`
                { a: 1, e: unit, f: [ 1 ], g: 5 }
          it "sets" do
            Mezgeb.set { a: 3, b: 4 } { a: 1, b: 2, c: true } `shouldEqual` { a: 3, b: 4, c: true }
          it "modifies" do
            Mezgeb.modify { a: (+) 1, b: (+) 5 } { a: 1, b: 2, c: true } `shouldEqual` { a: 2, b: 7, c: true }
