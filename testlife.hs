import Life

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

main :: IO ()
main = life glider
