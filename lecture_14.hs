module HelloServer where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

phrase :: T.Text
phrase = T.pack "brevity is the soul of wit"

twophrases :: T.Text
twophrases = phrase <> phrase

countLetter :: Char -> T.Text -> Int
countLetter c t =
  case T.uncons t of
    Nothing -> 0
    Just (x, rest) -> (if x == c then 1 else 0) + countLetter c rest

lazyPhrase :: TL.Text
lazyPhrase = TL.fromStrict phrase

binary :: B.ByteString
binary = B.pack [99, 111, 102]

lazyBinary :: BL.ByteString
lazyBinary = BL.fromStrict binary

phraseLength :: Int
phraseLength = T.length phrase

port :: Int
port = 3421
