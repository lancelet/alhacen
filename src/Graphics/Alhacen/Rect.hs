module Graphics.Alhacen.Rect
    ( Rect
    , rectX
    , rectY
    , rectWidth
    , rectHeight
    , empty
    , rect
    , null
    , intersection
    , hull
    , contains
    , containsPt
    , inflate
    , deflate
    ) where

import           Numeric.Interval (Interval, (...))
import qualified Numeric.Interval as I (contains, elem, empty, hull, inf,
                                        intersection, null, width)
import           Prelude          hiding (null)
import           Test.QuickCheck  (Arbitrary, arbitrary, frequency)

data Rect a = Rect { rectX      :: !a
                   , rectY      :: !a
                   , rectWidth  :: !a
                   , rectHeight :: !a }
            | Empty
            deriving (Eq, Show)

instance (Ord a, Num a, Arbitrary a) => Arbitrary (Rect a) where
    arbitrary = frequency
                [ ( 1, pure empty)
                , (10, rect <$> arbitrary <*> arbitrary <*>
                                arbitrary <*> arbitrary) ]

empty :: Rect a
empty = Empty
{-# INLINE empty #-}

rect :: (Num a, Ord a) => a -> a -> a -> a -> Rect a
rect x y w h | pos w && pos h = Rect x y w h
             | neg w = rect (x + w) y (abs w) h
             | neg h = rect x (y + h) w (abs h)
             | otherwise = Empty
  where
    pos p = p > (fromInteger 0)
    neg p = p < (fromInteger 0)
{-# INLINE rect #-}

null :: Rect a -> Bool
null Empty = True
null _     = False
{-# INLINE null #-}

hInt :: (Num a, Ord a) => Rect a -> Interval a
hInt Empty          = I.empty
hInt (Rect x _ w _) = x ... x + w
{-# INLINE hInt #-}

vInt :: (Num a, Ord a) => Rect a -> Interval a
vInt Empty          = I.empty
vInt (Rect _ y _ h) = y ... y + h
{-# INLINE vInt #-}

intervalsToRect :: (Ord a, Num a) => Interval a -> Interval a -> Rect a
intervalsToRect hi vi
    | not (I.null hi || I.null vi) = rect x y w h
    | otherwise                    = Empty
  where
    x = I.inf hi
    y = I.inf vi
    w = I.width hi
    h = I.width vi
{-# INLINE intervalsToRect #-}

ivalOp :: (Num a, Ord a)
       => (Interval a -> Interval a -> Interval a)
       -> Rect a
       -> Rect a
       -> Rect a
ivalOp f r1 r2 = intervalsToRect
                 (f (hInt r1) (hInt r2))
                 (f (vInt r1) (vInt r2))
{-# INLINE ivalOp #-}

intersection :: (Num a, Ord a) => Rect a -> Rect a -> Rect a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection r1 r2   = ivalOp I.intersection r1 r2
{-# INLINE intersection #-}

hull :: (Num a, Ord a) => Rect a -> Rect a -> Rect a
hull Empty r = r
hull r Empty = r
hull r1 r2   = ivalOp I.hull r1 r2
{-# INLINE hull #-}

contains :: (Num a, Ord a) => Rect a -> Rect a -> Bool
contains _ Empty = True
contains outr innr = xContained && yContained
  where
    xContained = I.contains (hInt outr) (hInt innr)
    yContained = I.contains (vInt outr) (vInt innr)
{-# INLINE contains #-}

containsPt :: (Num a, Ord a) => Rect a -> a -> a -> Bool
containsPt r x y = I.elem x (hInt r) && I.elem y (vInt r)
{-# INLINE containsPt #-}

inflate :: (Num a, Ord a) => a -> Rect a -> Rect a
inflate _ Empty = Empty
inflate amt (Rect x y w h) = if w' > 0 && h' > 0
                             then rect x' y' w' h'
                             else empty
  where
    x' = x - amt
    y' = y - amt
    w' = w + 2 * amt
    h' = h + 2 * amt
{-# INLINE inflate #-}

deflate :: (Num a, Ord a) => a -> Rect a -> Rect a
deflate _ Empty = Empty
deflate amt r   = inflate (negate amt) r
{-# INLINE deflate #-}
