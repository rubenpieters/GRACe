module ExampleEuler where

import Control.Monad

import Compile0
import GL

number :: Int -> GCM (Port Int)
number n = do
  let nmod3 = n `mod` 3
  let nmod5 = n `mod` 5
  input <- createPort
  output <- createPort

  component $ do
    i <- value input
    o <- value output
    ifThenElse (lit nmod3 === 0 .|| lit nmod5 === 0) (i === 1) (i === 0)
    assert $ o === lit n * i

  return output

ifThenElse :: CPExp Bool -> CPExp Bool -> CPExp Bool -> CP ()
ifThenElse if' then' else' = assert (if' ==> then')
                             >> assert (nt if' ==> else')

allNumbers :: Int -> GCM [Port Int]
allNumbers x = mapM number [0..x]

equal :: (IsPort p1, IsPort p2, CPType a) => p1 a -> p2 a -> CP ()
equal x y = do
  vx <- value x
  vy <- value y
  assert $ vx === vy

connect :: (CPType a) => GCM [Port a] -> GCM ([Port a], Port a) -> GCM (Port a)
connect gcm1 gcm2 = do
  values1 <- gcm1
  (values2, out) <- gcm2

  let zipped = zip values1 values2

  forM_ zipped (component . uncurry equal)

  return out

numbersGcm :: GCM ()
numbersGcm = do
  a <- allNumbers 50

  outputAll a "a"

example :: GCM ()
example = do
  a <- connect (allNumbers 9) (sumGCM 10)

  output a "a"

outputAll :: [Port a] -> String -> GCM ()
outputAll ports lbl = forM_ indexedPorts (uncurry $ indexedOutput lbl)
  where indexedPorts = zip [0..] ports

indexedOutput :: String -> Int -> Port a -> GCM ()
indexedOutput lbl idx port = output port (lbl ++ "_" ++ show idx)

showExample :: IO ()
showExample = runGCM example >>= putStr
