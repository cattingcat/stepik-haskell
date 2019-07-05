module Records where

data LogLevel = Error | Warning | Info

data Person = Person {
    firstName :: String, 
    lastName :: String, 
    age :: Int 
} deriving (Show)

p1 = Person "1" "11" 5
p2 = p1{age = 66}

john = Person{firstName="John", lastName="Doe", age=35}

pfoo :: Person -> String
pfoo Person{firstName = fn, lastName = ln} = fn ++ " " ++ ln

pfoo2 :: Person -> String
pfoo2 (Person a b c) = a ++ " " ++ b


data Shape = Circle Double | Rectangle Double Double

isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False


abbrFirstName :: Person -> Person
abbrFirstName p@(Person fn _ _) = if length fn < 2 then p else p{firstName = (take 1 fn) ++ "."}




data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x2-x1) + abs (y2-y1)


getCenter :: Double -> Coord Int -> Coord Double
getCenter w (Coord x y) = let c a = w * (fromIntegral a + 0.5) in Coord (c x) (c y)

getCell :: Double -> Coord Double -> Coord Int
getCell w (Coord x y) = let n a = floor $ a / w in Coord (n x) (n y)
