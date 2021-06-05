module Test.Main where

import Prelude
import Effect (Effect)
import Megzeb as Megzeb
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "መዝገብ" do
          it "inserts" do
            Megzeb.insert { a: 1, b: 2 } { c: 3, d: 4 } `shouldEqual` { a: 1, b: 2, c: 3, d: 4 }
          it "gets" do
            Megzeb.get { a: unit } { a: 3, d: 4 } `shouldEqual` { a: 3 }
          it "deletes" do
            Megzeb.delete
              { b: 0, c: 0, d: 0 }
              { a: 1, b: 2, c: true, d: false, e: unit, f: [ 1 ], g: 5 }
              `shouldEqual`
                { a: 1, e: unit, f: [ 1 ], g: 5 }
          it "sets" do
            Megzeb.set { a: 3, b: 4 } { a: 1, b: 2, c: true } `shouldEqual` { a: 3, b: 4, c: true }
          it "modifies" do
            Megzeb.modify { a: (+) 1, b: (+) 5 } { a: 1, b: 2, c: true } `shouldEqual` { a: 2, b: 7, c: true }
