module HsFp2.ParsecTest where

import Text.Parsec
import Text.Parsec.Char

getList :: Parsec String u [String]
getList = ((sepBy1 (many1 digit) (char ';')) <* eof)


test1 = parse getList "" "1;234;56"  -- succ
test2 = parse getList "" "1;234;56;" -- err
test3 = parse getList "" "1;;234;56" -- err