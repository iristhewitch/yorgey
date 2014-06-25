-- yorgey/homework1.hs

-- 1
toDigits :: Integer -> [Integer]
toDigits n
	| n <= 0 = []
	| otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev n = reverse (toDigits n)

-- 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x*2]
doubleEveryOther (x:y:xs) = (x*2) : y : doubleEveryOther xs

-- 3
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map sum (map toDigits xs))

-- 4
validate :: Integer -> Bool
validate n
	| (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0 = True
	| otherwise = False
	
-- 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
	| n == 0 = []
	| otherwise = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a