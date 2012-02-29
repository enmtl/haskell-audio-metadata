module Audio.Metadata.Types
  where

import qualified Data.Text as Text
import qualified Data.ByteString as B

data FieldContent = Text [Text.Text]
                  | Bytes B.ByteString
    deriving Show
