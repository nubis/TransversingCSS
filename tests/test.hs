module Main where

import qualified Data.List as DL
import Control.Monad
import Text.XML.HXT.TransversingCSS
import Test.HUnit
import Test.Hspec.HUnit
import Test.Hspec.Monadic

import System.IO
 
main :: IO ()
main = do
  html <- readFile "tests/test.html"
  hspecX $ describe "Test ALL the selectors" $ do
    it "^^^ Just that one thing" $ tests html

tests html = do
  finds 5 "li"
  finds 4 "div"
  finds 1 "#content" 
  finds 3 ".big"
  finds 1 ".big.c"
  finds 1 "p .big"
  finds 9 "li, div"
  finds 3 "li[extra-attr], div.b"
  finds 3 "[extra-attr]"
  finds 2 "li[extra-attr]"
  finds 1 "div[extra-attr=nothing]"
  finds 1 "li[extra-attr$=ata]"
  finds 1 "li[extra-attr^=some]"
  finds 1 "li[extra-attr*=o]"
  finds 2 "[extra-attr*=o]"
  finds 2 ".baz"
  finds 1 "p > .baz"
 where
  finds count query = case findBySelector html query of
    Left res -> assertFailure $ query ++ " did not parse."
    Right res -> do
      let were = DL.length res
          msg = query ++ " should match " ++ (show count) ++ " elements, it matched " ++ (show were)

      assertBool msg (count == were)
    
