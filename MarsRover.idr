module Main

import Data.Fin

data Direction = North | South | East | West

Position : Type
Position = (Nat, Nat)

record MarsRover where
       constructor MkRover
       upperBound : Position
       currentPosition : (Fin (fst upperBound), Fin (snd upperBound))
       direction : Direction
%name MarsRover rover, rover1, rover2

turnRight : MarsRover -> MarsRover
turnRight rover = MkRover (upperBound rover) (currentPosition rover) newDirection
          where newDirection = case direction rover of
                                    North => East
                                    South => West
                                    East  => South
                                    West  => North

turnLeft : MarsRover -> MarsRover
turnLeft rover = MkRover (upperBound rover) (currentPosition rover) newDirection
          where newDirection = case direction rover of
                                    North => West
                                    South => East
                                    East  => North
                                    West  => South

getPosition : MarsRover -> (Integer, Integer)
getPosition rover = let pos = currentPosition rover in (finToInteger (fst pos), finToInteger (snd pos))

move : MarsRover -> MarsRover
move = ?todo
