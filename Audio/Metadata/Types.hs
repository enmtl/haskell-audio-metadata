module Audio.Metadata.Types
  where

import Data.Text as Text


class AudioData a where
    title :: a -> Maybe [Text]
    album :: a -> Maybe [Text]
    track :: a -> Maybe [Text]
    artist :: a -> Maybe [Text]
