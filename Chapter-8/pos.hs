
-- type Pos
type Pos = (Int, Int)
-- Move
data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x, y) = (x, y+1)
