module MyLib (coefficients, domainLeft, domainRight, prodMatrixB, prodMatrixL, evalFuncSplit, evalFuncDerSplit, prodFuncB, gaussIntegrate) where

import Math.GaussianQuadratureIntegration (nIntegrate256)
import Data.Maybe (fromJust)
import Numeric.LinearAlgebra (flatten, fromLists, linearSolve, luSolve, toList, toLists, (><))

domainRight :: Double
domainRight = 2

domainLeft :: Double
domainLeft = 0

approxU :: Int -> Double -> Double 
approxU n x = 
    sum . zipWith(\i c_i -> c_i * basisF i x) [0..] . coefficients $ n
    where basisF = evalFuncSplit n

coefficients :: Int -> [Double]
coefficients n = toList . flatten $ fromJust (linearSolve b_matrix l_vector)
    where 
        b_matrix = fromLists $ prodMatrixB n
        l_vector = (n >< 1) $ prodMatrixL n

prodMatrixL :: Int -> [Double]
prodMatrixL n = [l_j j | j <- [1..n]]
    where 
        l_j = prodFuncL evalFunc
        evalFunc = evalFuncSplit n

prodMatrixB :: Int -> [[Double]]
prodMatrixB n = applyDirichlet [[b_ij i j| j <- [1..n]] | i <- [1..n]]
    where b_ij = prodFuncB n evalFunc evalFuncDer 
            where 
            evalFunc    = evalFuncSplit    n
            evalFuncDer = evalFuncDerSplit n

applyDirichlet :: [[Double]] -> [[Double]]
applyDirichlet matrix = map zeroLast rows ++ [prodLastRow  (n+1)]
    where 
        (rows, _) = splitAt n matrix
        n = length matrix - 1


zeroLast :: [Double] -> [Double]
zeroLast list  = xs ++ [0]
    where 
        (xs, _) = splitAt n list
        n = length list - 1

prodLastRow :: Int -> [Double]
prodLastRow n = [el n i | i <- [1..n]] where 
    el n i 
        | i == n     = 1
        | otherwise  = 0

gaussIntegrate :: Fractional a => a -> a-> Bool -> (a -> a) ->  a
gaussIntegrate lowBound upBound areNear func
    | areNear = nIntegrate256 func lowBound upBound
    | otherwise = 0

evalFuncSplit :: Int -> Int -> Double -> Double
evalFuncSplit n i x 
    | x < left || x > right = 0
    | x <= center           = (x - left)  * slope
    | otherwise             = (right - x) * slope
    where 
        width = (domainRight - domainLeft) / fromIntegral (n - 1)
        center = width * fromIntegral (i - 1)
        left = center - width
        right = center + width
        slope =  1.0 / width

evalFuncDerSplit :: Int -> Int -> Double -> Double
evalFuncDerSplit n i x 
    | x < left || x > right = 0
    | x <= center           =  slope
    | otherwise             = -slope
    where 
        width = (domainRight - domainLeft) / fromIntegral (n - 1) 
        center = domainLeft + width * fromIntegral (i - 1)
        left = center - width
        right = center + width
        slope =  1.0 / width

prodFuncL:: (Int -> Double -> Double) -> Int -> Double
prodFuncL e_j j = -10 * e_j j 0

prodFuncB :: Int -> (Int -> Double -> Double) -> (Int -> Double -> Double) -> Int -> Int -> Double
prodFuncB n e_k e'_k i j = gaussIntegrate lowBound upBound (areNear i j) func - rest
    where 
        areNear i j = abs (i - j) <= 1
        rest = e_k i 0 * e_k j 0
        func x = e'_k i x * e'_k j x
        lesser  = min (fromIntegral i) (fromIntegral j)
        greater = max (fromIntegral i) (fromIntegral j)
        width = (domainRight - domainLeft) / fromIntegral (n - 1)
        lowBound = domainLeft +  width * max 0 (greater - 2)
        upBound  = domainLeft + width * min (fromIntegral n - 1) (lesser + 0)


--someFunc :: IO ()
--someFunc = putStrLn "someFunc"
