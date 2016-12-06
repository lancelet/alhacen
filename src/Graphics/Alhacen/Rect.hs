{-|
Module      : Graphics.Alhacen.Rect
Description : Rectangles
Copyright   : (c) Jonathan Merritt, 2016
License     : BSD3
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental
Portability : POSIX

A generic rectangle type, with special handling of empty rectangles.

Normal rectangles, of type 'Rect'@ a@, have the following 4 scalar properties,
all of type @a@:

  [@x@] x-coordinate of a corner of the rectangle
  [@y@] y-coordinate of a corner of the rectangle
  [@width@] width of the rectangle
  [@height@] height of the rectangle

However, rectangles can also be empty, meaning that they have zero width and/or
zero height. When a rectangle becomes empty, it loses its area and also its @(x,
y)@ position.

Rectangles are constructed using either the 'rect' or 'empty' functions:

>>> rect 10 20 30 40 :: Rect Int
Rect 10 20 30 40
>>> empty
Empty

The existence of the empty rectangle is motivated primarily by the combinator
function 'intersection'. The purpose of this function is to compute the
rectangle which is the largest intersection of two other rectangles. However,
clearly it is possible to construct two rectangles that do /not/ overlap. In
this case, instead of having the partiality of the result represented by an
external disjunction, such as a @Maybe@ type, the partiality was encoded in
the rectangle type itself:

>>> intersection (rect 0 0 5 5 :: Rect Int) (rect 100 100 5 5 :: Rect Int)
Empty
-}
module Graphics.Alhacen.Rect
    ( -- * Types
      Rect
      -- * Properties
      -- $nonEmptyProps
    , rectX
    , rectY
    , rectWidth
    , rectHeight
    , withDims
      -- * Construct rectangles
    , empty
    , rect
      -- * Empty check
    , null
      -- * Combinators
    , intersection
    , hull
      -- * Containment tests
    , contains
    , containsPt
      -- * Resizing
    , inflate
    , deflate
    ) where

import           Numeric.Interval (Interval, (...))
import qualified Numeric.Interval as I (contains, elem, empty, hull, inf,
                                        intersection, null, width)
import           Prelude          hiding (null)
import           Test.QuickCheck  (Arbitrary, arbitrary, frequency)

-- $nonEmptyProps
--
-- Rectangles can be either empty or non-empty. However, the normal properties
-- of a rectangle (@x@, @y@, @width@ and @height@) are not well-defined for an
-- empty rectangle. Thus, the functions that retreive these properties must
-- return a @Maybe@ type in order to be total.

-- | Rectangle with dimensions of type @a@.
data Rect a =
    -- | Non-empty rectangle (x, y, width, height).
    Rect !a !a !a !a
    -- | Empty rectangle.
    | Empty
    deriving (Eq, Show)

-- | @x@ coordinate of a non-empty rectangle.
--
-- >>> rectX (rect 10 20 30 40 :: Rect Int)
-- Just 10
-- >>> rectX empty
-- Nothing
--
-- If more than one dimension is required, it may be more efficient to use
-- 'withDims' instead.
rectX :: Rect a -> Maybe a
rectX (Rect x _ _ _) = Just x
rectX Empty          = Nothing

-- | @y@ coordinate of a non-empty rectangle.
--
-- >>> rectY (rect 10 20 30 40 :: Rect Int)
-- Just 20
-- >>> rectY empty
-- Nothing
--
-- If more than one dimension is required, it may be more efficient to use
-- 'withDims' instead.
rectY :: Rect a -> Maybe a
rectY (Rect _ y _ _) = Just y
rectY Empty          = Nothing

-- | Width of a non-empty rectangle.
--
-- >>> rectWidth (rect 10 20 30 40 :: Rect Int)
-- Just 30
-- >>> rectWidth empty
-- Nothing
--
-- The width of a rectangle is always positive, if it exists:
--
-- prop> \r -> maybe True (\w -> w > 0) (rectWidth r)
--
-- If more than one dimension is required, it may be more efficient to use
-- 'withDims' instead.
rectWidth :: Rect a -> Maybe a
rectWidth (Rect _ _ w _) = Just w
rectWidth Empty          = Nothing

-- | Height of a non-empty rectangle.
--
-- >>> rectHeight (rect 10 20 30 40 :: Rect Int)
-- Just 40
-- >>> rectHeight empty
-- Nothing
--
-- The height of a rectangle is always positive, if it exists:
--
-- prop> \r -> maybe True (\h -> h > 0) (rectHeight r)
--
-- If more than one dimension is required, it may be more efficient to use
-- 'withDims' instead.
rectHeight :: Rect a -> Maybe a
rectHeight (Rect _ _ _ h) = Just h
rectHeight Empty          = Nothing

-- | Performs an operation with a non-empty rectangle.
--
-- If a rectangle is non-empty, then all of its dimensions are defined. If more
-- than one dimension is required, then this operation is more efficient than
-- calling 'rectX', 'rectY', 'rectWidth' and 'rectHeight' individually.
--
-- For example, to compute the area of a rectangle:
--
-- >>> let r = rect 10 20 30 40 :: Rect Int
-- >>> withDims r (\_ _ w h -> w * h)
-- Just 1200
-- >>> withDims empty (\_ _ w h -> w * h)
-- Nothing
withDims :: Rect a -> (a -> a -> a -> a -> b) -> Maybe b
withDims (Rect x y w h) f = Just (f x y w h)
withDims Empty          _ = Nothing

instance (Ord a, Num a, Arbitrary a) => Arbitrary (Rect a) where
    arbitrary = frequency
                [ ( 1, pure empty)
                , (10, rect <$> arbitrary <*> arbitrary <*>
                                arbitrary <*> arbitrary) ]

-- | An empty rectangle.
--
-- >>> empty :: Rect Int
-- Empty
--
-- >>> null empty
-- True
empty :: Rect a
empty = Empty
{-# INLINE empty #-}

-- | Creates a rectange.
--
-- If the width and height of the rectangle are both positive, then the
-- rectangle is created with the exact values specified:
--
-- >>> rect 50 60 300 400 :: Rect Int
-- Rect 50 60 300 400
--
-- In the case where either the width or height are negative when passed-in,
-- then the x and y coordinates are adjusted, so that the width and height are
-- positive as stored by the rectangle:
--
-- >>> rect 50 60 (-300) (-400) :: Rect Int
-- Rect (-250) (-340) 300 400
--
-- If the width or height are exactly zero for type 'a' (ie. they are not
-- positive or negative), then an @Empty@ rectangle is created:
--
-- >>> rect 50 60 0 400 :: Rect Int
-- Empty
--
-- However, the 'empty' function is a better way to create an empty rectangle if
-- it is definitely known to be empty.
rect :: (Num a, Ord a)
     => a -- ^ x-coordinate
     -> a -- ^ y-coordinate
     -> a -- ^ width
     -> a -- ^ height
     -> Rect a
rect x y w h | pos w && pos h = Rect x y w h
             | neg w = rect (x + w) y (abs w) h
             | neg h = rect x (y + h) w (abs h)
             | otherwise = Empty
  where
    pos p = p > (fromInteger 0)
    neg p = p < (fromInteger 0)
{-# INLINE rect #-}

-- | Checks if a rectangle is empty.
--
-- >>> null empty
-- True
--
-- >>> null (rect (-20) 10 440 30 :: Rect Int)
-- False
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

-- | Intersection of two rectangles.
--
-- >>> r1 = rect 5 5 20 10 :: Rect Int
-- >>> r2 = rect 8 9 50 40 :: Rect Int
-- >>> intersection r1 r2
-- Rect 8 9 17 6
--
-- If a pair of rectangles has no intersection then the intersection is also
-- @Empty@:
--
-- >>> intersection (rect 0 0 5 5 :: Rect Int) (rect 5 5 5 5 :: Rect Int)
-- Empty
--
-- @intersection@ is commutative:
--
-- prop> \a b -> intersection a b == intersection b a
--
-- The intersection of any rectangle with an empty rectangle is also empty:
--
-- prop> \r -> intersection r empty == empty
intersection :: (Num a, Ord a) => Rect a -> Rect a -> Rect a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection r1 r2   = ivalOp I.intersection r1 r2
{-# INLINE intersection #-}

-- | Smallest bounding rectangle containing its two inputs.
--
-- >>> r1 = rect  0  0 10 10 :: Rect Int
-- >>> r2 = rect 95 95  5  5 :: Rect Int
-- >>> hull r1 r2
-- Rect 0 0 100 100
--
-- @hull@ is commutative:
--
-- prop> \a b -> hull a b == hull b a
--
-- @hull@ with any empty rectangle is the original rectangle:
--
-- prop> \r -> hull r empty == r
hull :: (Num a, Ord a) => Rect a -> Rect a -> Rect a
hull Empty r = r
hull r Empty = r
hull r1 r2   = ivalOp I.hull r1 r2
{-# INLINE hull #-}

-- | Checks if an outer rectangle contains an inner one.
--
-- >>> contains (rect 0 0 10 10 :: Rect Int) (rect 2 2 3 3 :: Rect Int)
-- True
-- >>> contains (rect 0 0 10 10 :: Rect Int) (rect 5 6 8 2 :: Rect Int)
-- False
--
-- Every rectangle contains the empty rectangle:
--
-- prop> \r -> contains r empty
--
-- But the empty rectangle contains no other rectangle except the empty one:
--
-- prop> \r -> contains empty r == null r
contains :: (Num a, Ord a)
         => Rect a -- ^ outer rectangle to test
         -> Rect a -- ^ inner rectangle to test
         -> Bool   -- ^ @True@ if the outer rectangle fully contains the inner
contains _ Empty = True
contains outr innr = xContained && yContained
  where
    xContained = I.contains (hInt outr) (hInt innr)
    yContained = I.contains (vInt outr) (vInt innr)
{-# INLINE contains #-}

-- | Checks if a rectangle contains a point.
--
-- >>> containsPt (rect 10 20 10 10) 12 25
-- True
-- >>> containsPt (rect 10 20 10 10) 4 25
-- False
--
-- The empty rectangle contains no points:
--
-- prop> \x y -> containsPt (empty :: Rect Int) x y == False
containsPt :: (Num a, Ord a)
           => Rect a
           -> a -- ^ x coordinate
           -> a -- ^ y coordinate
           -> Bool
containsPt r x y = I.elem x (hInt r) && I.elem y (vInt r)
{-# INLINE containsPt #-}

-- | Uniformly inflate a rectangle by a given amount.
--
-- >>> inflate 5 (rect 10 20 30 40 :: Rect Int)
-- Rect 5 15 40 50
--
-- An empty rectangle cannot be inflated:
--
-- prop> \a -> null (inflate a empty)
--
-- Negative inflation is the same as deflation:
--
-- prop> \r a -> inflate a r == deflate (-a) r
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

-- | Uniformly deflate a rectangle by a given amount.
--
-- >>> deflate 5 (rect 5 15 40 50)
-- Rect 10 20 30 40
--
-- A rectangle can be deflated so far that it becomes empty:
--
-- >>> deflate 40 (rect 5 15 40 50)
-- Empty
--
-- Negative deflation is the same as inflation:
--
-- prop> \r a -> deflate a r == inflate (-a) r
deflate :: (Num a, Ord a) => a -> Rect a -> Rect a
deflate _ Empty = Empty
deflate amt r   = inflate (negate amt) r
{-# INLINE deflate #-}
