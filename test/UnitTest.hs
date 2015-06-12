{-# LANGUAGE Rank2Types #-}

module Main where

import Test.Tasty
import Test.Tasty.Golden.Advanced
import System.Directory
import qualified System.FilePath as FP
import System.IO hiding (utf8)
import Text.Html.Consolidate
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Text.XML.HXT.Arrow.WriteDocument
import Control.Arrow
import Control.Monad
import Data.Char
import Control.Monad.IO.Class
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput

main = do allCases <- getDirectoryContents casesDir
          allExpects <- getDirectoryContents expectsDir
          let validCases = getValid allCases
          let validExpects = getValid allExpects
          defaultMain $ testGroup "Tests" $
            map genTest $ filter (`elem` validExpects) validCases
            where getValid = filter $ \x -> FP.takeExtension x == ".html"

casesDir = "test-data/cases"
expectsDir = "test-data/expects"

genTest :: FilePath -> TestTree
genTest testFileName =
  let caseFileName = casesDir `FP.combine` testFileName
      expectFileName = expectsDir `FP.combine` testFileName
      ns = initialConsState False Nothing []
      state  = initialState ns
      normalizedA :: String -> TArr XmlTree XmlTree
      normalizedA s = single $ parseHTML s Nothing >>> consolidateArr
      runX :: forall r s. (String -> TArr XmlTree XmlTree) -> FilePath -> IO String
      runX a f = do
          s <- readFile f
          let arr :: TArr XmlTree String
              arr = (a s) >>> writeDocumentToString [withOutputHTML, withOutputEncoding utf8]
          liftM head $ runXIOState state $ arr
      expectedValueAction :: IO String
      expectedValueAction = readFile expectFileName
      testValueAction     = runX normalizedA caseFileName
  in goldenTest testFileName expectedValueAction testValueAction verifyOutput (const $ return ())

verifyOutput :: String -> String -> IO (Maybe String)
verifyOutput expected actual = return $
  let fs = filter (not . isSpace)
      msg = "Failed to match expected output to normalized input:\n\
            \Diff:\n" ++ ppDiff (getGroupedDiff (lines expected) (lines actual))
  in  if (fs expected == fs actual) then Nothing
                                    else Just msg
