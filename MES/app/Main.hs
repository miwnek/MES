module Main where

import MyLib (coefficients, domainLeft, domainRight, prodMatrixB, prodMatrixL, evalFuncSplit, evalFuncDerSplit, prodFuncB, gaussIntegrate)
import Data.Maybe (fromMaybe)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy(Default (def), blue, layout_title, line, opaque, plot, points, red, setColors, (.=))
import System.Environment (getArgs)
import System.Exit ()
import Text.Read (readMaybe)
import Data.Char (digitToInt)
import Text.Printf
import Data.List (transpose, nub)
--import MyLib (domainRight, domainLeft)

approxPlot :: Int -> [(Double, Double)]
approxPlot n = [((domainRight - domainLeft ) / fromIntegral (n - 1) * i, c_i) | (i, c_i) <- zip [0..] coef] :: [(Double, Double)]
  where coef = coefficients n 

exactPlot :: [(Double, Double)]
exactPlot = [(x, u x) | x <- [domainLeft,0.1 .. domainRight]] :: [(Double, Double)]
  where u x = -10*x + 20

parseArg :: [String] -> Int
parseArg [arg] = read arg
--parseArg _ = 5

main :: IO ()
main = do
  args <- getChar
  let n = digitToInt args :: Int

  table $ prodMatrixB n
  print $ prodMatrixL n
  print $ coefficients n

  --putStrLn (show $ evalFuncSplit n 1 0)

  print $ show $ prodFuncB n (evalFuncSplit n) (evalFuncDerSplit n) 1 2
  print $ show $ prodFuncB n (evalFuncSplit n) (evalFuncDerSplit n) 2 1

  print $ show $ evalFuncDerSplit n 1 0.3 * evalFuncDerSplit n 2 0.3
  print $ show $ evalFuncDerSplit n 1 0.4 * evalFuncDerSplit n 2 0.4
  print $ show $ evalFuncDerSplit n 1 0 * evalFuncDerSplit n 2 0

  print $ show $ gaussIntegrate 0 0.4 (1>0) (testing (evalFuncDerSplit n 2) $ evalFuncDerSplit n 1)

  toFile def "solution.svg" $ do
   layout_title .= "-u\"(x) = 0,  u'(0) + u(0) = 0,  u(2) = 0"
   setColors [opaque blue, opaque blue, opaque red]
   plot (line "Approximate solution" [approxPlot n])
   plot (points "" $ approxPlot n)
   plot (line "Exact solution" [exactPlot])

  putStrLn  "Done."

testing :: (Double -> Double) -> (Double -> Double) -> (Double -> Double)
testing f1 f2 x = f1 x * f2 x

print_ x = putStr $ show x ++ "\t"

table xxs 
  | length (nub [length xs | xs <- xxs])/=1 = error "not simetric"
  | otherwise = mapM_ printRow xxs
      where printRow xs = (mapM_ print_) xs >> putStrLn ""