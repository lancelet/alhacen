module Graphics.Alhacen.RectTest (tests) where

import           Graphics.Alhacen.Rect (Rect, contains, containsPt, deflate,
                                        empty, hull, inflate, intersection,
                                        null, rect, rectHeight, rectWidth,
                                        rectX, rectY)

import qualified Control.Exception     as CE (assert)
import           Data.Ratio            (Rational)
-- import           Debug.Trace           (trace)
import           System.Random         (Random)

import           Test.QuickCheck       (Gen, choose)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (Assertion, assertBool, testCase, (@=?))
import           Test.Tasty.QuickCheck (Property, forAll, testProperty)

import           Prelude               hiding (null)

tests :: TestTree
tests = testGroup "Graphics.Alhacen.Rect"
        [ createTests
        , intersectionTests
        , hullTests
        , containsTests
        , resizeTests ]

-------------------------------------------------------------------------------

createTests :: TestTree
createTests = testGroup "creation"
              [ rectCreate
              , rectCreateNegWidth
              , rectCreateNegHeight
              , rectCreateZeroWidth
              , rectCreateZeroHeight
              , rectCreateEmpty
              , rectCreateQC ]

assertSize :: (Show a, Eq a) => a -> a -> a -> a -> Rect a -> Assertion
assertSize x y w h r = do
    x @=? rectX r
    y @=? rectY r
    w @=? rectWidth r
    h @=? rectHeight r

rectCreate :: TestTree
rectCreate = testCase "typical Rect" $ do
    let r = rect 100 101 20 21 :: Rect Int
    assertSize 100 101 20 21 r

rectCreateNegWidth :: TestTree
rectCreateNegWidth = testCase "negative width" $ do
    let r = rect 100 101 (-20) 21 :: Rect Int
    assertSize 80 101 20 21 r

rectCreateNegHeight :: TestTree
rectCreateNegHeight = testCase "negative height" $ do
    let r = rect 100 101 20 (-21) :: Rect Int
    assertSize 100 80 20 21 r

rectCreateZeroWidth :: TestTree
rectCreateZeroWidth = testCase "zero width" $ do
    let r = rect 100 101 0 21 :: Rect Int
    assertBool "empty rect expected" (null r)

rectCreateZeroHeight :: TestTree
rectCreateZeroHeight = testCase "zero height" $ do
    let r = rect 100 101 20 0 :: Rect Int
    assertBool "empty rect expected" (null r)

rectCreateEmpty :: TestTree
rectCreateEmpty = testCase "empty" $ do
    let r = empty
    assertBool "empty rect expected" (null r)

rectCreateQC :: TestTree
rectCreateQC = testProperty "quickcheck" f
  where
    f :: Rational -> Rational -> Rational -> Rational -> Bool
    f x y w h = rect x y w h == expected
      where
        expected = if w == zero || h == zero then empty else rect x' y' w' h'
        zero = (fromInteger 0) :: Rational
        x' = if w > 0 then x else x + w
        y' = if h > 0 then y else y + h
        w' = abs w
        h' = abs h

-------------------------------------------------------------------------------

intersectionTests :: TestTree
intersectionTests = testGroup "intersection"
                    [ simpleIntersection
                    , intersectionWithEmpty
                    , commutativeIntersection
                    , withinIntersectionProp
                    , containsIntersectionProp
                    , nonIntersectingProp ]

simpleIntersection :: TestTree
simpleIntersection = testCase "one-off intersection" $ do
    let r1 = rect 2 1 4 3 :: Rect Int
        r2 = rect 3 2 4 4 :: Rect Int
        expected = rect 3 2 3 2
    expected @=? intersection r1 r2

intersectionWithEmpty :: TestTree
intersectionWithEmpty = testProperty "intersection with empty is null" f
  where
    f :: Rect Rational -> Bool
    f r = (null $ intersection r empty) && (null $ intersection empty r)

commutativeIntersection :: TestTree
commutativeIntersection = testProperty "intersection is commutative" f
  where
    f :: Rect Rational -> Rect Rational -> Bool
    f r1 r2 = intersection r1 r2 == intersection r2 r1

withinIntersectionProp :: TestTree
withinIntersectionProp =
    testProperty "intersection is contained in both parent rectangles" f
  where
    f :: Rect Rational -> Rect Rational -> Bool
    f r1 r2 = (contains r1 ir) && (contains r2 ir)
      where
        ir = intersection r1 r2

containsIntersectionProp :: TestTree
containsIntersectionProp =
    testProperty "sub-rectangle intersection is the sub-rectangle" f
  where
    f :: Rect Int -> Property
    f r = forAll (subRect r) (g r)

    g :: Rect Int -> Rect Int -> Bool
    g rOuter rInner = rInner == intersection rOuter rInner

nonIntersectingProp :: TestTree
nonIntersectingProp =
    testProperty "non-intersecting rectangles have an empty intersection" f
  where
    f :: Rect Int -> Bool
    f r = (null $ intersection r (rectBelow r)) &&
          (null $ intersection r (rectAbove r))
    rectAbove r = rect (rectX r + rectWidth r) (rectY r + rectHeight r) 5 6
    rectBelow r = rect (rectX r - fromInteger 5) (rectY r - fromInteger 6) 5 6

-------------------------------------------------------------------------------

hullTests :: TestTree
hullTests = testGroup "hull"
            [ simpleHull
            , hullRectWithItself
            , hullSubrect
            , commutitativeHull
            , hullContainsChildren
            , hullContainsIntersection ]

simpleHull :: TestTree
simpleHull = testCase "one-off hull" $ do
    let r1 = rect 2 1 4 3 :: Rect Int
        r2 = rect 3 2 4 4 :: Rect Int
        expected = rect 2 1 5 5
    expected @=? hull r1 r2

hullRectWithItself :: TestTree
hullRectWithItself =
    testProperty "hull of rect with itself is the original rect" f
  where
    f :: Rect Rational -> Bool
    f r = r == hull r r

hullSubrect :: TestTree
hullSubrect = testProperty "hull with subrectangle is original rectangle" f
  where
    f :: Rect Int -> Property
    f r = forAll (subRect r) (g r)

    g :: Rect Int -> Rect Int -> Bool
    g rOuter rInner = rOuter == hull rOuter rInner

commutitativeHull :: TestTree
commutitativeHull = testProperty "hull is commutitative" f
  where
    f :: Rect Rational -> Rect Rational -> Bool
    f r1 r2 = hull r1 r2 == hull r2 r1

hullContainsChildren :: TestTree
hullContainsChildren =
    testProperty "hull contains both of its child rectangles" f
  where
    f :: Rect Rational -> Rect Rational -> Bool
    f r1 r2 = contains h r1 && contains h r2
      where
        h = hull r1 r2

hullContainsIntersection :: TestTree
hullContainsIntersection =
    testProperty "hull contains the intersection of its two child rectangles" f
  where
    f :: Rect Rational -> Rect Rational -> Bool
    f r1 r2 = contains (hull r1 r2) (intersection r1 r2)

-------------------------------------------------------------------------------

containsTests :: TestTree
containsTests = testGroup "contains"
                [ simpleContains
                , hullContainment
                , intersectContainment
                , simpleContainsPt ]

simpleContains :: TestTree
simpleContains = testCase "one-off contains" $ do
    let r1 = rect 5 6 50 51 :: Rect Int
        r2 = rect 10 11 8 9 :: Rect Int
    True  @=? contains r1 r2
    False @=? contains r2 r1

hullContainment :: TestTree
hullContainment =
     testProperty "r1 `contains` (hull r1 r2) iff r1 `contains` r2" f
  where
    f :: Rect Rational -> Rect Rational -> Bool
    f r1 r2 = if r1 `contains` r2
              then r1 `contains` (hull r1 r2)
              else not $ r1 `contains` (hull r1 r2)

intersectContainment :: TestTree
intersectContainment =
    testProperty "(intersection r1 r2) `contains` r2 iff r1 `contains` r2" f
  where
    f :: Rect Rational -> Rect Rational -> Bool
    f r1 r2 = if r1 `contains` r2
              then (intersection r1 r2) `contains` r2
              else not $ (intersection r1 r2) `contains` r2

simpleContainsPt :: TestTree
simpleContainsPt = testCase "one-off containsPt" $ do
    let r = rect 5 5 2 2 :: Rect Int
    True  @=? containsPt r 6 6
    False @=? containsPt r 4 6
    False @=? containsPt r 8 6
    False @=? containsPt r 6 4
    False @=? containsPt r 8 4

-------------------------------------------------------------------------------

resizeTests :: TestTree
resizeTests = testGroup "resize"
              [ simpleInflate
              , simpleDeflate
              , deflateToEmpty ]

simpleInflate :: TestTree
simpleInflate = testCase "one-off inflate" $ do
    let r = rect 100 101 20 21 :: Rect Int
        r' = inflate 2 r
    assertSize 98 99 24 25 r'

simpleDeflate :: TestTree
simpleDeflate = testCase "one-off deflate" $ do
    let r = rect 100 101 20 21 :: Rect Int
        r' = deflate 3 r
    assertSize 103 104 14 15 r'

deflateToEmpty :: TestTree
deflateToEmpty = testCase "deflating beyond zero produces empty rect" $ do
    let r1 = rect 100 101 20 21 :: Rect Int
        r2 = rect 100 101 21 20 :: Rect Int
    True @=? (null $ deflate 20 r1)
    True @=? (null $ deflate 20 r2)

-------------------------------------------------------------------------------

-- | Generator of random sub-rectangles.
subRect :: (Ord a, Num a, Random a) => Rect a -> Gen (Rect a)
subRect r | null r = pure empty
          | otherwise = do
                x <- choose (rectX r, rectX r + rectWidth r)
                y <- choose (rectY r, rectY r + rectHeight r)
                w <- choose (0, rectWidth r - (x - rectX r))
                h <- choose (0, rectHeight r - (y - rectY r))
                let r' = rect x y w h
                CE.assert (contains r r') (pure r')
