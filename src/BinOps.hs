module BinOps where
import Data.Char
import Data.Tuple
import Data.List

data Bit = Zero | One deriving (Eq, Show)
data Sign = Minus | Plus deriving (Eq, Show)
data Z = Z Sign [Bit] deriving (Eq, Show)

add :: Z -> Z -> Z
add (Z Plus a) (Z Plus b)   = Z Plus    $ sumz a b Zero
add (Z Minus a) (Z Minus b) = Z Minus   $ sumz a b Zero
add (Z Plus a) (Z Minus b) = case cmpz a b EQ of
    EQ -> Z Plus []
    GT -> Z Plus $ diffz a b Zero
    LT -> Z Minus $ diffz b a Zero
add a b = add b a

cmpz :: [Bit] -> [Bit] -> Ordering -> Ordering
cmpz []     [] c = c
cmpz (a:as) [] _ = GT
cmpz [] (b:bs) _ = LT
cmpz (a:as) (b:bs) po = case (a,b) of
    (Zero, Zero)    -> cmpz as bs po
    (One, One)      -> cmpz as bs po
    (Zero, One)     -> cmpz as bs LT
    (One, Zero)     -> cmpz as bs GT
    
-- carry bit
sumz :: [Bit] -> [Bit] -> Bit -> [Bit]
sumz [] [] One = [One]
sumz [] [] Zero = []
sumz a [] Zero = a
sumz [] b Zero = b
sumz a [] One = sumz a [One] Zero
sumz [] b One = sumz [One] b One
sumz (a:as) (b:bs) Zero = if a /= b then One : sumz as bs Zero else case (a,b) of
    (Zero, Zero)    -> Zero : sumz as bs Zero
    (One, One)      -> Zero : sumz as bs One
sumz (a:as) (b:bs) One = if a /= b then Zero : sumz as bs One else case (a,b) of
    (Zero, Zero)    -> One : sumz as bs Zero
    (One, One)      -> One : sumz as bs One

-- borrow bit
diffz :: [Bit] -> [Bit] -> Bit -> [Bit]
diffz [One] [Zero] One = []
diffz [One] [One] Zero = []
diffz a [] Zero = a
diffz a [] One = diffz a [One] Zero 
diffz (a:as) (b:bs) Zero = if a == b then Zero : diffz as bs Zero else case (a,b) of
    (One, Zero) -> One : diffz as bs Zero
    (Zero, One) -> One : diffz as bs One
diffz (a:as) (b:bs) One = if a /= b then Zero : diffz as bs Zero else One : diffz as bs One

mul :: Z -> Z -> Z
mul (Z Plus []) _           = Z Plus []
mul _ (Z Plus [])           = Z Plus []
mul (Z Plus a) (Z Plus b)   = Z Plus  $ mulz a b
mul (Z Minus a) (Z Minus b) = Z Plus  $ mulz a b
mul (Z Minus a) (Z Plus b)  = Z Minus $ mulz a b
mul (Z Plus a) (Z Minus b)  = Z Minus $ mulz a b

mulz :: [Bit] -> [Bit] -> [Bit]
mulz a b = let 
    indexed = zip a [0..] 
    shiftb n = (take n (repeat Zero)) ++ b
    in foldr (\(d, i) x -> if d == One then sumz x (shiftb i) Zero else x) [] indexed



test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []
test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]
test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]
test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]
test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]
test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]
test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]
test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]
test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]
test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]
test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []
test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]
test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]
test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]
test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]
test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]
test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]
test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]

emptyZ = (Z Plus [])

test101 = (mul (Z Plus []) (Z Plus [])) == emptyZ
test102 = (mul (Z Plus []) (Z Plus [One])) == emptyZ
test103 = (mul (Z Plus []) (Z Minus [One])) == emptyZ
test104 = (mul (Z Plus [One]) (Z Plus [])) == emptyZ
test105 = (mul (Z Minus [One]) (Z Plus [])) == emptyZ

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]

testAdd = test001 && test002 && test003 && test011 && test012 && test013 && test021 && test022 && test023 && test031 && test032 && test033 && test041 && test042 && test043 && test051 && test052 && test053 && test054 && test055 && test056 && test057 && test058
testMul = test101 && test102 && test103 && test104 && test105 && test111 && test112 && test113 && test114 && test121 && test122 && test131

testAll = testAdd && testMul


toInt :: Z -> Int
toInt (Z sign []) = 0 
toInt (Z sign as) = let 
    loop [] _ = 0
    loop (a:as) n = let r = loop as (n+1) in if a == One then 2^n + r else r
    signToInt Plus = 1
    signToInt Minus = -1 in (signToInt sign) * (loop as 0)
    

fromInt :: Int -> Z
fromInt a   | a > 0     = Z Plus  $ fromInt' a
            | otherwise = Z Minus $ fromInt' a
    
fromInt' :: Int -> [Bit]
fromInt' a = unfoldr (\x -> if x == 0 then Nothing else let
    r = mod x 2
    digit = if r == 1 then One else Zero
    d = quot x 2 in Just (digit, d)) a

-- add :: Z -> Z -> Z
-- add a b = fromInt $ toInt a + toInt b

-- mul :: Z -> Z -> Z
-- mul a b = fromInt $ toInt a * toInt b

-- foo :: Bool -> Int
-- foo ~True = 1
-- foo False = 0

data Tmp = Tmp{kek::Int}