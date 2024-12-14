import System.IO
import Data.List

factorize :: Int -> [Int]
factorize 1 = []
factorize n = factorize' 2 n

factorize' :: Int -> Int -> [Int]
factorize' x n | n `mod` x == 0 = x : factorize (n `div` x)
               | otherwise = factorize' (x+1) n

main = do
    n <- getLine
    putStr (n ++ " = ")
    (putStrLn . intercalate "*" . map show . factorize . read) n
