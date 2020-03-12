{- PART A -}
inc1 x = x + 1
sub1 x = x - 1

zoom :: Int -> [Int]
zoom x = x:zoom (x + 1)

partA :: [Int]
partA = filter even (zoom 0)

{- PART B -}
partB :: [Int] -> [Int]
partB [] = []
partB (x:xs) = filter (>0) (map doubleIfPositive (x:xs))

doubleIfPositive :: Int -> Int
doubleIfPositive x
    | x > 0 = (2 * x)
    | otherwise = x

{- PART C-}
partC :: Double -> Double -> [Double] 
partC num guess = ((guess + num / guess) / 2) : partC num ((guess + num / guess) / 2)

{- PART D -}
partD :: [Double] -> Double -> Double
partD [] x = -1
partD (a:ab) x
    | abs (a - (nextD ab)) <= x = nextD ab
    | otherwise = partD ab x

nextD :: [Double] -> Double
nextD [] = -1
nextD (x:xs) = x

{- PART E -}
partE :: Double -> Double -> Double
partE x t = partD (partC x 1) t

{- PART F -}
partF :: [(b-> a)] -> [b] -> [a]
partF [] [] = []
partF (f:fs) (x:xs) = f x: partF fs xs

{- PART G -}
partG :: b -> [(b -> a)] -> [a]
partG x [] = []
partG x (f:fs) = f x: partG x fs

{- PART H -}
partH :: Eq a => a -> [a] -> [a]
partH elem [] = []
partH elem (x:xs)
    | elem == x = partH elem xs
    | otherwise = x : partH elem xs

{- PART I -}
partI :: Eq a => [a] -> [a] -> [a]
partI [] [] = []
partI [] (x:xs) = (x:xs)
partI (f:fs) [] = []
partI (f:fs) (x:xs) = map (partH f) (x:xs) : partI fs (x:xs)