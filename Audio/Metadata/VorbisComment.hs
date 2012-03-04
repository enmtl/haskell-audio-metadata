module Audio.Metadata.VorbisComment
  where

import Prelude hiding (take)
import Data.Word
import Data.Bits 

import Control.Applicative
import Data.Attoparsec
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as Text
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Text.Encoding as Encoding

import Data.Maybe
import Data.Function
import Data.List (sortBy)

import Audio.Metadata.Types

data Field = Title
           | Version
           | Album
           | Track
           | Artist
    deriving (Show, Enum)

newtype Metadata = Metadata (IM.IntMap [Text.Text])
    deriving Show

metadataLookup :: Field -> Metadata -> Maybe [Text.Text]
metadataLookup field (Metadata map) = IM.lookup (fromEnum field) map

instance AudioData Metadata where
    title = metadataLookup Title
    album = metadataLookup Album
    track = metadataLookup Track
    artist = metadataLookup Artist

fields :: M.Map B.ByteString Field
fields = let (+>) x y = (BC.pack x, y) in M.fromList
                [ "TITLE" +> Title
                , "VERSION" +> Version
                , "ALBUM" +> Album
                , "TRACKNUMBER" +> Track
                , "ARTIST" +> Artist
                ]

oggPageSize :: Parser Int
oggPageSize = do
    string $ BC.pack "OggS\NUL"
    _ <- take $
            1 -- header type flag
        +   8 -- absolute granule position
        +   4 -- stream serial number
        +   4 -- page sequence number
        +   4 -- page checksum
    segments <- anyWord8
    let add :: Int -> Word8 -> Int
        add x y = x + (fromIntegral y)
    B.foldl' add 0 <$> take (fromIntegral segments)

uint32le :: Parser Word32
uint32le = B.foldr' add 0 <$> take 4
    where add :: Word8 -> Word32 -> Word32
          add x y = (fromIntegral x) .|.  (y `shiftL` 8)

int :: Int -> Parser Int
int n = B.foldl' add 0 <$> take n
    where add :: Int -> Word8 -> Int
          add x y = (x `shiftL` 8) .|. (fromIntegral y) 

commentVector :: Parser (B.ByteString, Text.Text)
commentVector = uint32le >>= fmap (decode . break ) . take . fromIntegral
  where 
    break = B.breakByte . fromIntegral . fromEnum $ '='
    decode (a,s) = (a,Encoding.decodeUtf8 $ B.tail s)

comments :: Parser (Text.Text, Metadata)
comments = do
    vendor <- Encoding.decodeUtf8 <$> (uint32le >>= take . fromIntegral)
    len <- uint32le
    commentVectors <- count (fromIntegral len)  commentVector
    return (vendor, toMetadata commentVectors)



oggPreComments :: Parser ()
oggPreComments = () <$ do
    _ <- oggPageSize >>= take
    _ <- oggPageSize
    word8 3
    string $ BC.pack "vorbis"


toMetadata :: [(B.ByteString, Text.Text)] -> Metadata
toMetadata = Metadata . IM.fromAscList . group . sortBy (compare `on` fst) . tags
  where
    fromRaw (field, cnt) = flip (,) cnt . fromEnum <$> M.lookup field fields
    tags = catMaybes . map fromRaw
    group [] = []
    group ((key,val):xs) = let (vals, rest) = span ((==) key . fst) xs
                            in (key, val : map snd vals) : group rest

ogg :: Parser Metadata 
ogg = oggPreComments >> snd <$> comments

flacPreComments :: Parser ()
flacPreComments = () <$ do
    string $ BC.pack "fLaC"
    let nonCommentNorLastBlock = satisfy (\w -> not (testBit w 7 || w == 4)) 
                                    >> int 3 >>= take
    skipMany1 nonCommentNorLastBlock
    satisfyWith ((.&.) 0x7f) ((==) 4)
    int 3

    
flac :: Parser Metadata
flac = flacPreComments >> snd <$> comments
    
test :: Parser ()
test = () <$ (string (BC.pack "OggS") <|> string (BC.pack "fLaC"))

metadata = flac <|> ogg
   



