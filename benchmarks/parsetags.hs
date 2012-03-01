module Main
    where
import Criterion.Main
import Audio.Metadata

import Control.Applicative
import Control.Monad
import System.Directory

import qualified Data.IntMap as IM

parseDir :: IO [IM.IntMap FieldContent]
parseDir = do 
    files <- filterM doesFileExist =<< getDirectoryContents "."
    mapM fileTags files


main = defaultMain [
            bench "parseCurrent" $ parseDir
        ]
