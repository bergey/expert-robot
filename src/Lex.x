{
module Main where

import           Data.Text (Text)

-- import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T

}

%wrapper "basic"

-- $startId = [a-zA-Z\+\-\*\/=]
-- $idChars = [0-9$startId]
$unicodeIds = $printable # [$white \;\'\(\)]
-- $unicodeStartId = $unicodeIds # [0-9]

tokens :-

    $white+            ;
    ";".*              ;
    "'"                {\s -> Quote}
    "("                {\s -> LParen}
    ")"                {\s -> RParen}
    [0-9]+             {Number . read}
    [0-9]*\.[0-9]+      {\s -> Number (read ('0':s))}
    [0-9]+\.[0-9]*      {\s -> Number (read (s ++ "0"))}
    $unicodeIds+        {Word . T.pack}

{

data Token = Quote | LParen | RParen | Number Double | Word Text
    deriving (Eq, Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
