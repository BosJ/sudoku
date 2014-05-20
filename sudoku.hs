
{-|
Module      : Main
Copyright   : (c) Jaco Bos, 2014
License     : see libraries/base/LICENSE
Maintainer  : University of Twente
Stability   : experimental
Portability : non-portable (experimental)
               
Simple Sudoku solver without guessing
-}

import Data.List

-- | Try to solve Sudoku until no more progress is made
solve :: (Enum a, Num a, Ord a, Show a) => [[a]] -> IO ()
solve sudoku = showSudoku $ f (solutionSet sudoku)
    where f xs = (if xs == g xs then xs else f $ g xs) 
            where g xs = reduceSubBlock $ reduceRowColumn xs

-- | Generate initial solution possibilities and set "a priori" values 
solutionSet :: (Enum a, Eq a, Num a) => [[a]] -> [[[a]]]
solutionSet xs = map f xs
    where f xs = map (\x -> if x /= [0] then x else [1..9]) 
                 (map (\x -> [x]) xs)

-- | Reduce solution set by removing impossibilities in rows and columns 
reduceRowColumn :: Ord a => [[[a]]] -> [[[a]]]
reduceRowColumn xs = transpose $ f $ transpose $ f xs
    where f = map (removeNakedPair . setLastpossible . removeSingles)

-- | Reduce solution set by removing impossibilities in sub-blocks
reduceSubBlock :: (Eq a, Ord a) => [[[a]]] -> [[[a]]]
reduceSubBlock xs = f s ++ f ((drop 3) s) ++ f ((drop 6) s)
    where f xs = g xs ++ g (map (drop 3) xs) ++ g (map (drop 6) xs)
          g xs = [ concat $ map (take 3) (take 3 xs) ]
          s = map (\x -> h x) (map (getSubBlock xs) [0..8])
          h = removeNakedPair . setLastpossible . removeSingles

-- | Get the Sudoku sub-block indicated by n (0..8)
getSubBlock :: [[a]] -> Int -> [a]
getSubBlock xs n = concat $ map (take 3) (map (drop ((n - d)*3)) r)
    where r = take 3 (drop d xs)
          d = (n `div` 3) * 3

-- | Elimenate singleton list-values from all other sub-lists 
removeSingles :: Eq a => [[a]] -> [[a]]
removeSingles xs = map (\x -> if length x > 1 then g x else x) xs
    where g = ( \\ (concat [ x | x <- xs, length x == 1 ]))

-- | Fixate last possible value in a sub-list
setLastpossible :: (Eq a, Ord a) => [[a]] -> [[a]]
setLastpossible xs = f xs k
    where k = [ x | x <- group $ sort $ concat xs, length x == 1 ] 
          f [] _ = []
          f (x:xs) s = [ if v == [] then x else v ] ++ f xs s
            where v = filter (\i -> i `elem` concat s) x

-- | Remove naked pairs from all other sub-lists
removeNakedPair :: Eq a => [[a]] -> [[a]]
removeNakedPair xs = g xs (nub (f xs))
    where f [] = []
          f (x:xs) = if length x == 2 && x `elem` xs then 
            [x] ++ f xs else f xs
          g [] _ = []
          g (x:xs) s
            | not (x `elem` s) = [(x \\ concat s)] ++ g xs s
            | otherwise = [x] ++ g xs s

-- | Print sudoku
showSudoku :: Show a => [[[a]]] -> IO ()
showSudoku xs = putStr ("\n" ++ sudokuString xs ++ 
    " +-----------------------+\n\n")
sudokuString [] = []
sudokuString xs = ( " +-----------------------+\n" ++
    (f $ concat $ take 3 xs) ++ sudokuString (drop 3 xs) )
    where f [] = []
          f xs = " |" ++ g (take 9 xs) ++ "\n" ++ f (drop 9 xs)
            where g [] = []
                  g xs = h (take 3 xs) ++ " |" ++ g (drop 3 xs)
                    where h [] = []
                          h (x:xs) = if length x > 1 
                            then " ." ++ h xs 
                            else " " ++ show (head x) ++ h xs

s1 = -- Easy Sudoku
  [ [ 0,0,0,2,6,0,7,0,1 ],
    [ 6,8,0,0,7,0,0,9,0 ],
    [ 1,9,0,0,0,4,5,0,0 ],
    [ 8,2,0,1,0,0,0,4,0 ],
    [ 0,0,4,6,0,2,9,0,0 ],
    [ 0,5,0,0,0,3,0,2,8 ],
    [ 0,0,9,3,0,0,0,7,4 ],
    [ 0,4,0,0,5,0,0,3,6 ],
    [ 7,0,3,0,1,8,0,0,0 ] ]

s2 = -- Assignment Sudoku
  [ [ 0,0,0,0,0,0,4,8,0 ],
    [ 0,0,0,0,0,1,3,0,0 ],
    [ 0,0,7,5,0,0,0,0,0 ],
    [ 0,0,0,0,8,0,9,0,6 ],
    [ 6,4,0,0,0,0,0,0,0 ],
    [ 0,1,0,0,0,5,0,0,7 ],
    [ 0,0,1,0,6,0,0,0,0 ],
    [ 8,9,0,3,0,7,0,0,2 ],
    [ 0,0,6,0,4,0,7,3,0 ] ]

s3 = -- Hard Sudoku
  [ [ 0,0,6,0,0,0,0,0,4 ],
    [ 0,0,0,8,6,0,7,3,0 ],
    [ 0,4,0,3,5,0,0,0,2 ],
    [ 1,7,0,4,0,0,6,0,0 ],
    [ 0,9,0,0,0,0,0,8,0 ],
    [ 0,0,8,0,0,6,0,1,7 ],
    [ 2,0,0,0,8,1,0,4,0 ],
    [ 0,6,7,0,4,3,0,0,0 ],
    [ 8,0,0,0,0,0,3,0,0 ] ]
