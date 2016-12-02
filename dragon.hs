import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

turnVector :: Floating t => V2 t -> Bool -> V2 t
turnVector lastDir turnRight =
    if turnRight 
        then lastDir # rotateBy (1/4)
        else lastDir # rotateBy (-1/4)

curveMaker :: Floating t => [Bool] -> V2 t -> [V2 t]
curveMaker [] vLast = []
curveMaker (x:xs) vLast =
    let vCurrent = (turnVector vLast x)
    in vCurrent:(curveMaker xs vCurrent)

dragon :: Integer -> [Bool]
dragon 1 = [True]
dragon x = 
    let lastDragon = (dragon (x-1))
    in lastDragon ++ [True] ++ (reverse (map not lastDragon))

main = mainWith (fromOffsets (curveMaker (dragon 10) unitY) # bg white :: Diagram B)
