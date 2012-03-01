{-# LANGUAGE TupleSections #-}

module Audio.Metadata.ID3 
  where

import Prelude hiding (take)
import Data.Bits
import Data.Word
import Control.Applicative
import Control.Monad
import Data.Attoparsec
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe
import Data.List (foldl')

import Audio.Metadata.Types

id3 = BC.pack "ID3"

newtype EncodingErrors = EncodingErrors Int
noEncodingErrors = EncodingErrors 0
unsynchedFrameSize (EncodingErrors eerrs) = testBit eerrs (fromEnum UnsynchedFrameSize)
shortIdentifier (EncodingErrors eerrs) = testBit eerrs (fromEnum ShortIdentifier)

data EncodingError = 
        UnsynchedFrameSize -- Frame Size
    |   ShortIdentifier
    deriving Enum

errors :: [EncodingError] -> EncodingErrors
errors = EncodingErrors . foldl' setBit 0 . map fromEnum

hasError :: EncodingError -> EncodingErrors -> Bool
hasError e (EncodingErrors errs) = testBit errs (fromEnum e)


data FrameID = Title 
             | Album
             | Track
             | Artist
    deriving (Show, Enum)

data Flags = Flags
            { unsynchronization :: Bool
            , extended :: Bool
            , experimental :: Bool
            , footer :: Bool
            }
    deriving Show

type Version = (Word8, Word8)
data Header = Header 
                { version :: Version
                , flags :: Flags
                , headerSize :: Int
                }
    deriving Show
data ExtendedHeader = ExtendedHeader 
                { update :: Bool
                , crc :: Maybe Int
                , restriction :: Maybe Word8
                }
    deriving Show

data FrameInfo = FrameInfo
            { frameId :: B.ByteString
            , size :: Int
            , statusFlags :: Word8
            , formatFlags :: Word8
            }
    deriving Show


frameIds_v2_4_0 :: M.Map B.ByteString FrameID
frameIds_v2_4_0 = frameIds_v2_3_0

frameIds_v2_3_0 :: M.Map B.ByteString FrameID
frameIds_v2_3_0 = let (+>) x y = (BC.pack x, y) in M.fromList 
                [ "TIT2" +> Title
                , "TALB" +> Album
                , "TRCK" +> Track
                , "TPE1" +> Artist
                ]

frameIds_v2_2_0 :: M.Map B.ByteString FrameID
frameIds_v2_2_0 = let (+>) x y = (BC.pack x, y) in M.fromList 
                [ "TT2" +> Title
                , "TAL" +> Album
                , "TRK" +> Track
                , "TP1" +> Artist
                ]

frameIds :: Version -> M.Map B.ByteString FrameID
frameIds (2,_) = frameIds_v2_2_0
frameIds (3,_) = frameIds_v2_3_0
frameIds (4,_) = frameIds_v2_4_0



readInt :: Int -> B.ByteString -> Int
readInt n = B.foldl' (add . (`shiftL` n)) 0
  where add :: Int -> Word8 -> Int
        add x y = x + (fromIntegral y)

synchsafeInt :: B.ByteString -> Int
synchsafeInt = readInt 7

int :: B.ByteString -> Int
int = readInt 8

id3Flags :: Parser Flags
id3Flags = do
    flags <- satisfyWith ((.&.) 0x7) (== 0)
    return Flags 
        { unsynchronization= testBit flags 7
        , extended= testBit flags 6
        , experimental= testBit flags 5
        , footer= testBit flags 4
        }
    

id3Header :: B.ByteString -> Parser Header
id3Header identifier = do
    string identifier
    version <- (,) <$> anyWord8 <*> anyWord8 
    flags <- id3Flags
    size <- synchsafeInt <$> take 4
    return $ Header {version=version, flags=flags, headerSize=size}

id3ExtendedHeader :: Parser ExtendedHeader
id3ExtendedHeader = do
    size <- synchsafeInt <$> take 4
    flagBytes <- word8 1
    flags <- satisfyWith (.&. 0x8f) (== 0)
    update <- if testBit flags 6
                then anyWord8 >> return True
                else return False
    crc <- if testBit flags 5
                then Just . synchsafeInt <$> take 5
                else return Nothing
    restriction <- if testBit flags 4
                    then Just <$> anyWord8
                    else return Nothing
    return $ ExtendedHeader {update=update, crc=crc, restriction=restriction}

identifier :: Version -> Parser (Maybe EncodingError, B.ByteString)
identifier (major, minor) 
        | major == 2 = do
                id' <- take 3
                when (not $ valid id') $ fail "id3 frame identifier"
                return (Nothing, id')
        | otherwise = do
                id' <- take 4 
                let shortId = valid (B.init id') && B.last id' == 0
                when ((not $ valid id') && not shortId) $ 
                    fail "id3 frame identifier"
                return (if shortId then Just ShortIdentifier else Nothing, id')
    where valid = B.all $ inClass "A-Z0-9"

frameSize :: Version -> EncodingErrors -> Parser Int
frameSize (major, minor) ee | major == 2 = int <$> take 3
                            | major == 3 = int <$> take 4 -- hasError UnsynchedFrameSize ee = int <$> take 4
                            | otherwise = synchsafeInt <$> take 4



textUTF8 :: Int -> Parser [T.Text]
textUTF8 n = map Encoding.decodeUtf8 . B.split 0 <$> take n

textUTF16BE :: Int -> Parser [T.Text]
textUTF16BE n = T.splitOn (T.pack "\NUL\NUL") . Encoding.decodeUtf16BE <$> take n

textUTF16 :: Int -> Parser [T.Text]
textUTF16 n = do
    bom <- satisfy (>= 0xFE)
    if bom == 0xFE 
        then word8 0xFF 
        else word8 0xFE
    let decode = if bom == 0xFE
                    then Encoding.decodeUtf16BE 
                    else Encoding.decodeUtf16LE
    T.splitOn (T.pack "\NUL\NUL") . decode <$> take (n - 2)

frame :: Version -> EncodingErrors -> Parser (EncodingErrors, FrameInfo, FieldContent)
frame version eerrors = do
    (maybeIdErr, frameId) <- identifier version

    size <- frameSize version eerrors
    let getFlag = if version >= (3,0) then anyWord8 else return 0
    statusFlags <- getFlag
    formatFlags <- getFlag
    content <- case BC.head frameId of
                    'T' -> do
                            encoding <- satisfy (<=3)
                            Text <$> case encoding of
                                        0 -> textUTF8 (pred size)
                                        1 -> textUTF16 (pred size)
                                        2 -> textUTF16BE (pred size)
                                        3 -> textUTF8 (pred size)
                    otherwise -> Bytes <$> take size

    let info = FrameInfo
            { frameId=  frameId
            , size=     size
            , statusFlags= statusFlags
            , formatFlags= formatFlags
            }
        frameEE = errors $ catMaybes [maybeIdErr]
    return (frameEE, info, content)


parseFrames :: Version -> Parser [(EncodingErrors, FrameInfo, FieldContent)]
parseFrames ver = def
  where 
    def = many $ frame ver noEncodingErrors
    check = do
        frames <- def
        case frames of
            [x] -> fail "resolving encoding errors"
            otherwise -> return frames
    tryUnsynch = do
        frames <- many $ frame ver $ errors [UnsynchedFrameSize]
        let garbage = (FrameInfo (BC.pack $ "garbage " ++ show (length frames)) 0 0 0
                        , Bytes (BC.pack $ "!"))
        when (null frames || null (tail frames)) $ fail "no encoding error"
        return $ frames
    
tags :: Parser (IM.IntMap FieldContent)
tags = do
    header <- id3Header id3
    when (extended $ flags header) $
        () <$ id3ExtendedHeader 
    frames <- parseFrames (version header)
        
    let lookup id' ee | shortIdentifier ee = M.lookup (B.init id') (frameIds (2,0))
                      | otherwise = M.lookup id' $ frameIds (version header)
        get (ee, info, val) = (,val) . fromEnum <$> lookup (frameId info) ee
    return . IM.fromList . catMaybes . map get $ frames

test :: Parser ()
test = () <$ string id3 

info :: Parser (Header, [FrameInfo])
info = do 
    header <- id3Header id3
    when (extended $ flags header) $
        () <$ id3ExtendedHeader 
    frames <- parseFrames (version header)
    return (header, map (\(_,info,_) -> info) frames)


    
  
    
