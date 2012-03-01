module Audio.Metadata
    (
        fileTags
    ,   fileType
    ,   info
    ,   module Audio.Metadata.Types
    )
  where
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Attoparsec
import Data.Attoparsec.Combinator

import qualified System.IO as IO
import qualified Data.ByteString as B
import qualified Data.IntMap as IM 

import Audio.Metadata.Types
import qualified Audio.Metadata.ID3 as ID3

data Container = ID3 
    deriving (Show, Eq)


fileTags :: String -> IO (IM.IntMap FieldContent)
fileTags file = do
    result <- IO.withFile file IO.ReadMode $ \handle ->
        parseWith (B.hGet handle 131072) ID3.tags B.empty
    return . fromMaybe IM.empty . maybeResult  $ result

fileType :: String -> IO (Maybe Container)
fileType file = do
    let parser = choice 
            [
                const ID3 <$> ID3.test 
            ]
    result <- IO.withFile file IO.ReadMode $ \handle ->
        parseWith (B.hGet handle 5012) parser B.empty
    return . maybeResult $ result

info :: String -> IO ()
info file = do
    result <- IO.withFile file IO.ReadMode $ \handle ->
        parseWith (B.hGet handle 131072) ID3.info B.empty
    print . maybeResult $ result
