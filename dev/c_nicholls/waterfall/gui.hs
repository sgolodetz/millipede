module Main where

import Data.Array.Base (readArray,writeArray )
import Data.Array.Unboxed       (IArray, Ix, UArray, amap, bounds, elems, listArray, (!) )
import Data.Word                (Word8, Word16)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

main :: IO ()
main  = do
  initGUI
  Just xml <- xmlNew "Waterfall.glade"
  window <- xmlGetWidget xml castToWindow "window1"
  onDestroy window mainQuit
 
  images <- mapM (\n -> pixbufNewFromFile  ("/home/scratch/output/output" ++ show n ++ ".pgm")) [1..50]
  image <- xmlGetWidget xml castToImage "image1"

  
  scroller <- xmlGetWidget xml castToHScrollbar "hscrollbar1"
  afterAdjustBounds scroller  (setImage image images)
  
  --close button
  
  closeButton <- xmlGetWidget xml castToButton "button1"  
  onClicked closeButton $ do {widgetDestroy window}
  --laonClicked closeButton $ do {set image [imageSource := testimage]}

  widgetShowAll image
  widgetShowAll window
  mainGUI
  

setImage image pixbuffs x= do
  imageSetFromPixbuf image (pixbuffs !! (max 0$min 49 (fromEnum x)))


