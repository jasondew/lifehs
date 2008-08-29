import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo
import Control.Monad.Trans ( liftIO )
import Control.Concurrent (threadDelay)
import Life

width, height :: Int
width  = 500
height = 500

rows, columns :: Int
rows    = 10
columns = 10

borderWidth :: Double
borderWidth = 1.0

cellWidth, cellHeight :: Double
cellWidth  = (fromIntegral width)  / (fromIntegral columns)
cellHeight = (fromIntegral height) / (fromIntegral rows)

run :: Render () -> IO ()
run act = do
  initGUI
  dia <- dialogNew
  dialogAddButton dia stockClose ResponseClose
  contain <- dialogGetUpper dia

  canvas <- drawingAreaNew
  canvas `onSizeRequest` return (Requisition (fromIntegral width) (fromIntegral height))
  canvas `onExpose` updateCanvas canvas act
  boxPackStartDefaults contain canvas

  widgetShow canvas
  dialogRun dia
  widgetDestroy dia
  -- Flush all commands that are waiting to be sent to the graphics server.
  -- This ensures that the window is actually closed before ghci displays the
  -- prompt again.
  flush

  where updateCanvas :: DrawingArea -> Render () -> Event -> IO Bool
        updateCanvas canvas act (Expose {}) = do
          win <- widgetGetDrawWindow canvas
          renderWithDrawable win act
          return True
        updateCanvas canvas act _ = return False



setRed :: Render ()
setRed = do
  setSourceRGB 1 0 0



setFat :: Render ()
setFat = do
  setLineWidth 20
  setLineCap LineCapRound



drawSquare :: Double -> Double -> Render ()
drawSquare width height = do
  (x,y) <- getCurrentPoint
  lineTo (x+width) y
  lineTo (x+width) (y+height)
  lineTo x (y+height)
  closePath
  stroke



drawHCirc :: Double -> Double -> Double -> Render ()
drawHCirc x y radius = do
  arc x y radius 0 pi
  stroke



drawStr :: String -> Render ()
drawStr txt = do
  lay <- createLayout txt
  showLayout lay



drawStr_ :: String -> Render ()
drawStr_ txt = do
  lay <- liftIO $ do
    ctxt <- cairoCreateContext Nothing
    descr <- contextGetFontDescription ctxt
    descr `fontDescriptionSetSize` 20
    ctxt `contextSetFontDescription` descr
    layoutText ctxt txt
  showLayout lay

updateCanvas :: DrawingArea -> Render () -> Event -> IO Bool
updateCanvas canvas act (Expose {}) = do win <- widgetGetDrawWindow canvas
                                         renderWithDrawable win act
                                         return True
updateCanvas canvas act _           = return False



drawBoard :: Board -> Render ()
drawBoard board = do
  setLineWidth borderWidth
  drawBorder
  drawVerticalLines
  drawHorizontalLines
  drawLife board
  
  where
    drawLife :: Board -> Render ()
    drawLife board = sequenceMap drawLiveCell (map translate board)

    translate :: Position -> (Double, Double)
    translate (x, y) = ((fromIntegral x) * cellWidth,
                        (fromIntegral y) * cellHeight)

    drawLiveCell :: (Double, Double) -> Render ()
    drawLiveCell (x, y) = setRed >> (moveTo x y) >> (drawSquare cellWidth cellHeight)

    drawBorder :: Render ()
    drawBorder = do
      moveTo 0 0
      drawSquare (fromIntegral width) (fromIntegral height)

    drawVerticalLines = sequenceMap drawVertical (finiteIterate columns cellWidth)
    drawHorizontalLines = sequenceMap drawHorizontal (finiteIterate rows cellHeight)

    finiteIterate :: Int -> Double -> [Double]
    finiteIterate count size = take count (iterate (+ size) size)

    drawVertical :: Double -> Render ()
    drawVertical x = do
      moveTo x 0
      lineTo x (fromIntegral height)
      closePath
      stroke
  
    drawHorizontal :: Double -> Render ()
    drawHorizontal y = do
      moveTo 0 y
      lineTo (fromIntegral width) y
      closePath
      stroke

    sequenceMap :: (a -> Render ()) -> [a] -> Render ()
    sequenceMap f list = sequence_ (map f list)
      
glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

main = do initGUI
          Just xml <- xmlNew "LifeGUI.glade"

          window <- xmlGetWidget xml castToWindow "window"
          onDestroy window mainQuit

          canvas <- xmlGetWidget xml castToDrawingArea "drawingArea"
          canvas `onSizeRequest` return (Requisition (fromIntegral width) (fromIntegral height))
          canvas `onExpose` updateCanvas canvas (drawBoard glider)

          stepButton <- xmlGetWidget xml castToButton "button1"
          onClicked stepButton $ do (updateCanvas canvas (drawBoard (nextGeneration glider)) (Expose {}))
                                    return ()

          widgetShowAll window
          mainGUI
