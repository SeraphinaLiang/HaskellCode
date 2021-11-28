data Move = Rock | Paper | Scissors
  deriving (Show, Eq)

beat :: Move -> Move
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock     = Scissors
lose Paper    = Rock
lose Scissors = Paper

data Result = Win | Lose | Draw
  deriving (Show, Eq)

outcome :: Move -> Move -> Result
outcome Rock     Rock     = Draw
outcome Paper    Paper    = Draw
outcome Scissors Scissors = Draw
outcome Rock     Paper    = Lose
outcome Paper    Scissors = Lose
outcome Scissors Rock     = Lose
outcome _        _        = Win
