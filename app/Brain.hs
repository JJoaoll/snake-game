

module Brain where

import qualified Dir as Dir
import Control.Monad.State
import System.Random 
import Dir

type GA a = State StdGen a
-- for testin
brainTest :: DecisionTree
brainTest =
  Condition (FoodAhead, Front) Dir.LT 10
    (Action StayFront)  
    (Condition (WallAhead, Front) Dir.LT 1.0
      (Action StayFront) 
      (Action StayFront) 
    )

mutationRate :: Float
mutationRate = 0.05 

crossoverRate :: Float
crossoverRate = 0.70 
merge :: DecisionTree -> DecisionTree -> DecisionTree
merge (Action moveA) _ = Action moveA


-- INITIAL IDEA WITH A CLASSICAL DISTRIBUITION:
randomElement :: StdGen -> [a] -> (a, StdGen)
randomElement gen xs =
    let (idx, newGen) = randomR (0, length xs - 1) gen
    in (xs !! idx, newGen)

allMoves :: [Move]
allMoves = [TurnLeft, TurnRight, StayFront]

allSensors :: [Sensor]
allSensors = [WallAhead, FoodAhead, TailAhead]

allDirs :: [Dir]
allDirs = 
  [ Dir.Front
  , Dir.Left
  , Dir.Right
  , Dir.Back
  , Dir.FrontLeft
  , Dir.FrontRight
  , Dir.BackLeft
  , Dir.BackRight
  ]

allOperators :: [Operator]
allOperators = [Dir.LT, Dir.LEQ, Dir.GEQ, Dir.GT]



