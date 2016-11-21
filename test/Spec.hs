import qualified Graphics.Alhacen.RectTest as Rect (tests)

import Test.Tasty (TestTree, testGroup, defaultMain)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Alhacen"
        [ Rect.tests ]
