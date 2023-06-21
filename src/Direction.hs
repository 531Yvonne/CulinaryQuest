module Direction where

-- Direction type contains 4 cardinal directions
data Direction = N | S | E | W
  deriving (Eq)

instance Show Direction where
  show :: Direction -> String
  show dir =
    case dir of
      N -> "north"
      S -> "south"
      E -> "east"
      W -> "west"
