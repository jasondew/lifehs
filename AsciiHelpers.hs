module AsciiHelpers (clearScreen, writeAt) where

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: (Int, Int) -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs
