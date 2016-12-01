module Graphics.Alhacen.Window
    ( Window
    , windowRect
    , windowPx
    , fromImage
    , crop
    , slideByPixels
    ) where

import           Graphics.Alhacen.Rect (Rect, intersection, rect, rectHeight,
                                        rectWidth, rectX, rectY)

import           Codec.Picture         (Image, Pixel, imageHeight, imageWidth,
                                        pixelAt)
import           Data.Word             (Word32)

data Window a = Window
    { windowRect :: !(Rect Word32)
    , windowPx   :: Word32 -> Word32 -> a }

intToWordForce :: Int -> Word32
intToWordForce i = if i < 0
                   then error "Cannot convert negative Int to a Word32!"
                   else (fromInteger . toInteger) i

wordToIntForce :: Word32 -> Int
wordToIntForce w = if w > intToWordForce (maxBound :: Int)
                   then error "Word32 is too large to convert to an Int!"
                   else (fromInteger . toInteger) w

imageRect :: Image a -> Rect Word32
imageRect image = rect 0 0 w h
  where
    w = (intToWordForce . imageWidth) image
    h = (intToWordForce . imageHeight) image

fromImage :: (Pixel a) => Image a -> Window a
fromImage image = Window winRect pixelFn
  where
    winRect = imageRect image
    pixelFn x y = pixelAt image (wordToIntForce x) (wordToIntForce y)

rectContains :: Rect Word32 -> Word32 -> Word32 -> Bool
rectContains r x y = (x >= minx) && (x < maxx) && (y >= miny) && (y < maxy)
  where
    minx = rectX r
    miny = rectY r
    maxx = minx + rectWidth r
    maxy = miny + rectHeight r

crop :: Rect Word32 -> Window a -> Window a
crop cropRect srcWindow = Window dstRect pixelFn
  where
    dstRect = intersection cropRect (windowRect srcWindow)
    xmin = rectX dstRect
    ymin = rectY dstRect
    contains = rectContains dstRect
    pixelFn x y = if contains x y
                  then windowPx srcWindow (x + xmin) (y + ymin)
                  else error "Index out of bounds in cropped window."

slideByPixels :: Word32 -> Word32 -> Window a -> [Window a]
slideByPixels width height srcWindow =
    [subWin x y | y <- [ymin..ymax], x <- [xmin..xmax]]
  where
    subWin x y = crop (rect x y width height) srcWindow
    r = windowRect srcWindow
    xmin = rectX r
    ymin = rectY r
    xmax = rectWidth r + xmin - width
    ymax = rectHeight r + ymin - height
