module Life (Board, Position, life) where

import Control.Concurrent (threadDelay)
import AsciiHelpers (clearScreen, writeAt)

type Position = (Int, Int)
type Board = [Position]

width, height, delay :: Int
width = 10
height = 10
delay = 10000

translate :: Position -> (Int, Int)
translate (0, 0) = translate (1, 1)
translate (0, y) = translate (1, y)
translate (x, 0) = translate (x, 1)
translate (x, y) = ((x - 1) * 2 + 2, (y - 1) * 2 + 2)

drawBoard :: IO ()
drawBoard = do clearScreen
               sequence_ [drawVertical x | x <- map (+ 1) (map (* 2) [0..width])]
               sequence_ [drawHorizontal y | y <- map (+ 1) (map (* 2) [0..height])]

drawVertical, drawHorizontal :: Int -> IO ()
drawVertical   x = sequence_ [writeAt (x, y) "|" | y <- [2..(2 * height + 1)]]
drawHorizontal y = writeAt (1, y) (init (concat (take (width + 1) (repeat "+-"))))

showBoard :: Board -> IO ()
showBoard board = do drawBoard
                     sequence_ [writeAt (translate b) "X" | b <- board]
                     putStrLn ""

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

liveNeighborCount :: Board -> Position -> Int
liveNeighborCount board = length . filter (isAlive board) . neighbors

survivors :: Board -> [Position]
survivors board = [position | position <- board, (liveNeighborCount board position) `elem` [2,3]]

births :: Board -> [Position]
births board = [(x, y) | x <- [1..width],
                         y <- [1..height],
                         isEmpty board (x, y),
                         liveNeighborCount board (x, y) == 3]

nextGeneration :: Board -> Board
nextGeneration board = survivors board ++ births board

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

life :: Board -> IO ()
life board = do clearScreen
                showBoard board
                threadDelay delay
                life (nextGeneration board)
