--Summation over the nested lists
sums :: Num b => [[b]] -> [b]
sums l = map sum l

--Determines whether a given number is perfect
--Creates a list of sums of the reciprocal divisors
--The sum of this list is equal to sum of all divisors
--For perfect numbers, this sum should be equal to doubled x
isPerfect :: Int -> Bool
isPerfect n = (2 * n) == sum [m + (div n m) | m <- [1 .. floor (sqrt (fromIntegral n))], mod n m == 0]

--Computes the sum 1^3 + ... + n^3
sumOfCubes :: Int -> Int
sumOfCubes n = sum [ m^3 | m <- [1..n]]


--Behaviour is identical to 'replicate b x'
--Cheater version
duplicate :: Int -> a -> [a]
duplicate n x = replicate n x

--Custom version of the previous function
--Non-cheater version
duplicate' :: Int -> a -> [a]
duplicate' n x = [ x | y <- [1..n]]

--Coputes the mean value of the sequance of numbers
average :: (RealFrac a) => [a] -> a
average l = (sum l) / (fromIntegral (length l))

countingRange low high l = sum [ 1 | x <- l, x >= low, x <= high]

