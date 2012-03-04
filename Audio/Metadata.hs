module Audio.Metadata
    (
        hParse
    ,   metadata
    ,   container
    ,   parseFile
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
import qualified Audio.Metadata.VorbisComment as VC

data Container = ID3 | VorbisComment
    deriving (Enum, Show)
data Metadata = ID3Data ID3.Metadata
              | VorbisCommentData VC.Metadata
    deriving Show

instance AudioData Metadata where
    title metadata = case metadata of
                ID3Data meta -> title meta
                VorbisCommentData meta -> title meta
    album metadata = case metadata of
                ID3Data meta -> album meta
                VorbisCommentData meta -> album meta
    track metadata = case metadata of
                ID3Data meta -> track meta
                VorbisCommentData meta -> track meta
    artist metadata = case metadata of
                ID3Data meta -> artist meta
                VorbisCommentData meta -> artist meta
    

hParse :: Parser a -> IO.Handle -> IO (Maybe a)
hParse parser handle = maybeResult <$>
    parseWith (B.hGet handle 131072) parser B.empty

parseFile :: Parser a -> String -> IO (Maybe a)
parseFile parser file = IO.withFile file IO.ReadMode $ hParse parser

metadata :: Parser Metadata
metadata = choice [ID3Data <$> ID3.metadata, VorbisCommentData <$> VC.metadata]

container = choice [const ID3 <$> ID3.test, const VorbisComment <$> VC.test]

