--Summation over the nested lists
sums :: Num b => [[b]] -> [b]
sums l = map sum l

--Summation
sums' [(x:xs)] = sum x
sums' (x:xs) = (sum x) : (sums' xs)

--Determines whether a given number is perfect
--Creates a list of sums of the reciprocal divisors
--The sum of this list is equal to sum of all divisors
--For perfect numbers, this sum should be equal to doubled x
isPerfect :: Int -> Bool
isPerfect n = (2 * n) == sum [m + (div n m) | m <- [1 .. floor (sqrt (fromIntegral n))], mod n m == 0]

--Checks if m is a divisor of n
isDivisor :: Int -> Int -> Bool
isDivisor n m = 0 == mod n m

--Alternative way to check if n is perfect
isPerfect' :: Int -> Bool
isPerfect' n = n == sum (filter (isDivisor n) [ m | m <- [1..(n - 1)]])


--Computes the sum 1^3 + ... + n^3
sumOfCubes :: Int -> Int
sumOfCubes n = sum [ m^3 | m <- [1..n]]

sumOfCubes' n = sum (map (^3) [m | m <- [1..n]])


--Behaviour is identical to 'replicate b x'
--Cheater version
duplicate :: Int -> a -> [a]
duplicate n x = replicate n x

--Custom version of the previous function
--Non-cheater version
duplicate' :: Int -> a -> [a]
duplicate' n x = [ x | y <- [1..n]]

--Computes the mean value of the sequance of numbers
average :: (RealFrac a) => [a] -> a
average l = (sum l) / (fromIntegral (length l))

--Compute the number of elements lying inside the range (low, high)
--countingRange :: Int -> Int -> [a] -> Int
countingRange low high l = sum [ 1 | x <- l, x >= low, x <= high]

--Splits the string at the spaces
--splitSpaces :: String -> [String]
splitSpaces s = words s