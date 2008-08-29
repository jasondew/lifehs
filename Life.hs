module Life (Board, Position, nextGeneration) where

type Position = (Integer, Integer)
type Board = [Position]

width, height :: Integer
width = 10
height = 10

isAlive :: Board -> Position -> Bool
isAlive board position = elem position board

isEmpty :: Board -> Position -> Bool
isEmpty board position = not (isAlive board position)

wrap :: Position -> Position
wrap (x, y) = (((x - 1) `mod`  width) + 1,
               ((y - 1) `mod` height  + 1))

neighbors :: Position -> [Position]
neighbors (x, y) = map wrap [(x - 1, y - 1), (    x, y - 1), (x + 1, y - 1),
                             (x - 1,     y),                 (x + 1,     y),
                             (x - 1, y + 1), (    x, y + 1), (x + 1, y + 1)]

liveNeighborCount :: Board -> Position -> Integer
liveNeighborCount board = toInteger . length . filter (isAlive board) . neighbors

survivors :: Board -> [Position]
survivors board = [position | position <- board, (liveNeighborCount board position) `elem` [2,3]]

births :: Board -> [Position]
births board = [(x, y) | x <- [1..width],
                         y <- [1..height],
                         isEmpty board (x, y),
                         liveNeighborCount board (x, y) == 3]

nextGeneration :: Board -> Board
nextGeneration board = survivors board ++ births board
