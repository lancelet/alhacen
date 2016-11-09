module Graphics.Alhacen where

import qualified Codec.Picture as CP
import           Data.List (tails)
import           Data.Word (Word8, Word16)
import           Text.Printf (printf)

data Interval = Interval
    { intervalStart :: !Int
    , intervalWidth :: !Int }
    deriving (Show, Eq)

intervalIntersect :: Interval -> Interval -> Maybe Interval
intervalIntersect i1@(Interval x1 w1) i2@(Interval x2 w2)
    | x2 < x1 = intervalIntersect i2 i1
    | (x1 + w1) < x2 = Nothing
    | otherwise = Just $ Interval x2 w
  where
    x1max = x1 + w1
    x2max = x2 + w2
    w = (min x1max x2max) - x2

data Rect = Rect
    { rectX :: !Int
    , rectY :: !Int
    , rectWidth :: !Int
    , rectHeight :: !Int }
    deriving (Show, Eq)

rect :: Int -> Int -> Int -> Int -> Rect
rect x y w h = Rect x' y' w' h'
  where
    (x', w') = if w > 0 then (x, w) else (x-w, -w)
    (y', h') = if h > 0 then (y, h) else (y-h, -h)

rectIntersect :: Rect -> Rect -> Maybe Rect
rectIntersect (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
    let
        maybeix = intervalIntersect (Interval x1 w1) (Interval x2 w2)
        maybeiy = intervalIntersect (Interval y1 h1) (Interval y2 h2)
    in
        case (maybeix, maybeiy) of
            (Just ix, Just iy) -> Just $ rect x y w h
                                    where
                                      x = intervalStart ix
                                      y = intervalStart iy
                                      w = intervalWidth ix
                                      h = intervalWidth iy
            _ -> Nothing

data Color = Color !Float !Float !Float

px2color :: CP.PixelRGBF -> Color
px2color (CP.PixelRGBF r g b) = Color r g b

color2px :: Color -> CP.PixelRGBF
color2px (Color r g b) = CP.PixelRGBF r g b

data Image = Image
    { imageWidth :: !Int
    , imageHeight :: !Int
    , imagePixelAt :: Int -> Int -> Color }

data Window = Window
    { windowX :: !Int
    , windowY :: !Int
    , windowImage :: !Image }

imageRect :: Image -> Rect
imageRect image = Rect 0 0 (imageWidth image) (imageHeight image)

juicy2image :: CP.Image CP.PixelRGBF -> Image
juicy2image jimage = Image w h fpx
  where
    w = CP.imageWidth jimage
    h = CP.imageHeight jimage
    fpx x y = px2color $ CP.pixelAt jimage x y

image2juicy :: Image -> CP.Image CP.PixelRGBF
image2juicy (Image w h fpx) = CP.generateImage fpxj w h
  where
    fpxj x y = color2px $ fpx x y

cropImage :: Image -> Rect -> Maybe Window
cropImage base winRect = do
    (Rect wx wy ww wh) <- rectIntersect winRect (imageRect base)
    let
        fpx x y = imagePixelAt base (x + wx) (y + wy)
        image = Image ww wh fpx
    return $ Window wx wy image

slideInner :: Image -> Int -> Int -> [Window]
slideInner image rw rh = [f x y | y <- [0..ymax], x <- [0..xmax]]
  where
    f x y = fromJust $ cropImage image (Rect x y rw rh)
    w = imageWidth image
    h = imageHeight image
    xmax = w - rw
    ymax = h - rh

fastSlides :: Image -> [Window]
fastSlides img = slideInner img 7 7

translateCoords :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
translateCoords cs dx dy = fmap dp cs
  where
    dp (x, y) = (x + dx, y + dy)

fastCircle :: [(Int, Int)]
fastCircle = [ ( 3, 0), ( 3, 1), ( 2, 2), ( 1, 3)
             , ( 0, 3), (-1, 3), (-2, 2), (-3, 1)
             , (-3, 0), (-3,-1), (-2,-2), (-1,-3)
             , ( 0,-3), ( 1,-3), ( 2,-2), ( 3,-1) ]

fastCircle' :: [(Int, Int)]
fastCircle' = translateCoords fastCircle 3 3

fastSampleWinColors :: Window -> [Color]
fastSampleWinColors (Window _ _ img) = fmap fs fastCircle'
  where
    fs (x, y) = imagePixelAt img x y

fastSamples :: Image -> [(Int, Int, Color, [Color])]  -- x, y, list of 16 colors
fastSamples img = fmap unpackit (zip centralPixels (zip coords' colors))
  where
    windows = fastSlides img
    coords = fmap (\(Window x y _) -> (x, y)) windows
    coords' = translateCoords coords 3 3
    colors = fmap fastSampleWinColors windows
    centralPixels = fmap (\(Window _ _ i) -> imagePixelAt i 3 3) windows
    unpackit (cp, ((x, y), cs)) = (x, y, cp, cs)

-- | Copy the first `n` items from a list to the end of the list.
recap :: Int -> [a] -> [a]
recap n as = concat [ as, take n as ]

fastSatisfied :: Float    -- ^ central pixel intensity
              -> Float    -- ^ threshold
              -> Int      -- ^ number of contiguous pixels
              -> [Float]  -- ^ pixel looped values in the circle
              -> Bool
fastSatisfied p t n floatPixels = any aboveOrBelow (windows n floatPixels)
  where
    aboveOrBelow xs = above xs || below xs
    above xs = all (\x -> x > (p + t)) xs
    below xs = all (\x -> x < (p - t)) xs
    windows m = filter (\l -> length l == m) . map (take m) . tails
    {-
    windows m xs = takeLengthOf (drop (m-1) xs) (windows' m xs)
    windows' m = map (take m) . tails
    takeLengthOf = zipWith (flip const)
    -}

fastFeatureDetector :: Image             -- ^ image in which to detect features
                    -> (Color -> Float)  -- ^ intensity function
                    -> Int               -- ^ number of contiguous pixels
                    -> Float             -- ^ threshold value
                    -> [(Int, Int)]      -- ^ detected feature pixel coords
fastFeatureDetector image intensityFn n threshold = features
  where
    features = fmap getXY
               $ filter fastSatisfiedPred
               $ fastSamples
               $ image
    getXY (x, y, _, _) = (x, y)
    fastSatisfiedPred :: (Int, Int, Color, [Color]) -> Bool
    fastSatisfiedPred (_, _, p, cs) = fastSatisfied (intensityFn p) threshold n
                                      $ recap (n-1)
                                      $ fmap intensityFn cs

fast12Red :: Image -> Float -> [(Int, Int)]
fast12Red image threshold = fastFeatureDetector image red 12 threshold
  where
    red (Color r _ _) = r

----

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "fromJust called on Nothing!"

fromRight :: Either b a -> a
fromRight (Right x) = x
fromRight (Left _)  = error "fromRight called on Left!"

testCrop :: Image -> Image
testCrop img = windowImage $ fromJust $ cropImage img (Rect 50 50 100 100)

word8ToFloat :: Word8 -> Float
word8ToFloat x = realToFrac x / ub
  where
    ub = realToFrac $ (maxBound :: Word8)

word16ToFloat :: Word16 -> Float
word16ToFloat x = realToFrac x / ub
  where
    ub = realToFrac $ (maxBound :: Word16)

rgba8Torgbf :: CP.PixelRGBA8 -> CP.PixelRGBF
rgba8Torgbf (CP.PixelRGBA8 ri gi bi _) = CP.PixelRGBF r g b
  where
    r = word8ToFloat ri
    g = word8ToFloat gi
    b = word8ToFloat bi

rgba16Torgbf :: CP.PixelRGBA16 -> CP.PixelRGBF
rgba16Torgbf (CP.PixelRGBA16 ri gi bi _) = CP.PixelRGBF r g b
  where
    r = word16ToFloat ri
    g = word16ToFloat gi
    b = word16ToFloat bi

asRGBF :: (Monad m) => CP.DynamicImage -> m (CP.Image CP.PixelRGBF)
asRGBF (CP.ImageRGBF img)   = return img
asRGBF (CP.ImageRGB8 _)     = fail "Unhandled image type (rgb8)"
asRGBF (CP.ImageRGBA8 img)  = return $ CP.pixelMap rgba8Torgbf img
asRGBF (CP.ImageRGB16 _)    = fail "Unhandled image type (rgb16)"
asRGBF (CP.ImageRGBA16 img) = return $ CP.pixelMap rgba16Torgbf img
asRGBF _                    = fail "Unhandled image type (type not recorded)"

saveNumberedWindow :: (Int, Window) -> IO ()
saveNumberedWindow (n, w) = do
    putStrLn $ "Saving: " ++ name
    CP.saveTiffImage name dynImage
  where
    name = printf "window-%05d.tif" n
    dynImage = CP.ImageRGBF $ image2juicy $ windowImage w

main :: IO ()
main = do

    let fileName = "statuetest.tif"
    eitherImage <- CP.readImage fileName
    let
        image = juicy2image $ fromRight $ (eitherImage >>= asRGBF)
        features = fast12Red image 0.4
    putStrLn $ show features
    
    --eitherImage <- CP.readImage fileName

    {-
    let
        inImage = juicy2image $ fromRight $ (eitherImage >>= asRGBF)
        sliderImgs = take 1000 $ slideInner inImage 7 7
        sliders = zip [0..] sliderImgs

    sequence_ $ fmap saveNumberedWindow sliders
    -}
