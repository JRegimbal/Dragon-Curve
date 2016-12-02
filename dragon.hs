import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- Number of iterations
iterations :: Integer
iterations = 20 

-- Scaling factor
factor :: (Real a, Fractional a) => a
factor = 1000


-- Returns a vector rotated by 90-deg or -90-deg
turnVector :: Floating t => V2 t -> Bool -> V2 t
turnVector lastDir turnRight =
    if turnRight 
        then lastDir # rotateBy (1/4)
        else lastDir # rotateBy (-1/4)

-- Creates a curve using the series of turns - True -> right, False -> left
curveMaker' :: (Real t, Floating t) => [Bool] -> V2 t -> [V2 t]
curveMaker' [] _ = []
curveMaker' (x:xs) vLast =
    let vCurrent = (turnVector vLast x)
    in vCurrent:(curveMaker' xs vCurrent)

curveMaker :: (Real t, Floating t) => [Bool] -> [V2 t]
curveMaker x = curveMaker' x (factor *^ unit_X)

-- Generates the list of turns in a dragon curve of length x
dragon' :: Integer -> Integer -> [Bool] -> [Bool]
dragon' count max last
    | count < max = let tailEnd = reverse . map not $ last
        in dragon' (count + 1) max (last ++ [True] ++ tailEnd)
    | otherwise = last

dragon :: Integer -> [Bool]
dragon x = dragon' 1 x []

diagram :: Diagram B
diagram = bg white (fromOffsets . curveMaker . dragon $ iterations) 

main = mainWith diagram
