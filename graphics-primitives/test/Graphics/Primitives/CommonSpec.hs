module Graphics.Primitives.CommonSpec
  ( spec
  )
where

import           Graphics.Primitives.Common
import           Graphics.Primitives.Scene
import           Test.Hspec
import           Data.Array
import qualified Data.IntSet                   as S
import           Control.Monad

-- | Determine if a perimeter as an anticlockwise wound simple polygon.
isValidPerimeter :: [Point] -> IO ()
isValidPerimeter path = do
  windingDirection path `shouldBe` CCW
  intersectsWithSelf path `shouldBe` False

-- | Determine if we have a valid triangulation of a polygon.
isValidTriangulation :: Path -> [Triangle] -> IO ()
isValidTriangulation path triangles = do
  let pathLength = length path
      allTriangulatedPoints =
        S.fromList $ concatMap (\(a, b, c) -> [a, b, c]) triangles
      indexedPoints = listArray (0, pathLength - 1) path
      isValidTriangle (a, b, c) =
        windingDirection
            [indexedPoints ! a, indexedPoints ! b, indexedPoints ! c]
          == CCW

  -- Check that we have exactly the minimum number of triangles and every point
  -- is on at least one triangle.
  length triangles `shouldBe` (pathLength - 2)
  S.toAscList allTriangulatedPoints `shouldBe` [0 .. pathLength - 1]

  -- Check that all the triangles are wound anticlockwise.
  forM_ triangles (`shouldSatisfy` isValidTriangle)

spec :: Spec
spec = parallel $ do
  describe "triangulate"        triangulateSpec
  describe "perimeter"          perimeterSpec
  describe "segmentsIntersect"  segmentsIntersectSpec
  describe "intersectsWithSelf" intersectsWithSelfSpec
  describe "windingDirection" $ do
    it "detects a clockwise polygon"
      $          windingDirection [Point 0 0, Point 1 0, Point 1 1, Point 0 1]
      `shouldBe` CW
    it "detects an anticlockwise polygon"
      $          windingDirection [Point 0 0, Point 0 1, Point 1 1, Point 1 0]
      `shouldBe` CCW

segmentsIntersectSpec :: Spec
segmentsIntersectSpec = do
  it "detects a simple intersection"
    $ segmentsIntersect (Point (-1) 0, Point 1 0) (Point 0 (-1), Point 0 1)
    `shouldBe` True
  it "detects touching line segments"
    $          segmentsIntersect (Point 0 0, Point 2 2) (Point 1 1, Point 3 1)
    `shouldBe` True
  it "detects touching line segments (left)"
    $          segmentsIntersect (Point (-2) (-2), Point (-2) 2)
                                 (Point (-2) 0   , Point 0 0)
    `shouldBe` True
  it "detects touching line segments (right)"
    $ segmentsIntersect (Point 0 (-4), Point 4 (-4)) (Point 4 0, Point 4 (-8))
    `shouldBe` True
  it "detects colinear lines"
    $          segmentsIntersect (Point 0 0, Point 10 10) (Point 2 2, Point 6 6)
    `shouldBe` True
  it "detects colinear lines reverse"
    $          segmentsIntersect (Point 10 0, Point 0 10) (Point 6 4, Point 4 6)
    `shouldBe` True
  it "detects duplicate lines"
    $ segmentsIntersect (Point 0 0, Point 10 10) (Point 10 10, Point 0 0)
    `shouldBe` True
  it "ignores parallel lines" $ do
    segmentsIntersect (Point 0 0, Point 10 10) (Point 0 1, Point 10 11)
      `shouldBe` False
    segmentsIntersect (Point 0 0, Point 10 10) (Point 10 0, Point 20 10)
      `shouldBe` False
    segmentsIntersect (Point 0 0, Point 0 5) (Point 4 6, Point 4 7)
      `shouldBe` False
  it "ignores non-intersecting segments of intersecting lines"
    $          segmentsIntersect (Point 0 0, Point 0 2) (Point 4 4, Point 6 4)
    `shouldBe` False
  it "ignores almost intersecting segments of intersecting lines"
    $          segmentsIntersect (Point 0 0, Point 2 2) (Point 1 4, Point 4 0)
    `shouldBe` False
  it "ignores non-overlapping segments of co-linear lines"
    $          segmentsIntersect (Point 0 0, Point 2 2) (Point 4 4, Point 6 6)
    `shouldBe` False

intersectsWithSelfSpec :: Spec
intersectsWithSelfSpec = do
  it "detects a simple polygon"
    $          intersectsWithSelf christmasTree
    `shouldBe` False
  it "detects a self intersecting polygon"
    $          intersectsWithSelf
                 [Point 0 1, Point 1 0, Point 3 2, Point 4 1, Point 3 0, Point 1 2]
    `shouldBe` True
  it "detects a polygon with a point touching a line"
    $ intersectsWithSelf [Point 0 0, Point 3 0, Point 2 1, Point 2 0, Point 1 1]
    `shouldBe` True
  it "detects a polygon with a point touching a line on the left"
    $ intersectsWithSelf [Point 0 0, Point 1 0, Point 0 1, Point 1 2, Point 0 2]
    `shouldBe` True
  it "detects a polygon with a point touching a line on the right"
    $ intersectsWithSelf [Point 0 0, Point 1 0, Point 1 2, Point 0 2, Point 1 1]
    `shouldBe` True
  it "detects a polygon with two points touching"
    $          intersectsWithSelf
                 [Point 1 0, Point 2 0, Point 2 1, Point 1 0, Point 1 1, Point 0 1]
    `shouldBe` True
  it "detects a polygon with overlapping line segments"
    $ intersectsWithSelf [Point 0 0, Point 0 1, Point 1 1, Point 1 0, Point 0 0]
    `shouldBe` True

perimeterSpec :: Spec
perimeterSpec = do
  it "creates a valid perimeter for a rectangle"
    $ isValidPerimeter
    $ perimeter
    $ Rectangle (Size 10 5)
  it "creates a valid perimeter for a rounded rectangle"
    $ isValidPerimeter
    $ perimeter
    $ RoundedRectangle (Size 40 20) 6
  it "creates a valid perimeter for a degenerate rounded rectangle"
    $ isValidPerimeter
    $ perimeter
    $ RoundedRectangle (Size 40 20) 0
  it "creates a valid perimeter for a small rounded rectangle"
    $ isValidPerimeter
    $ perimeter
    $ RoundedRectangle (Size 40 20) 2
  it "creates a valid perimeter for a very small rounded rectangle"
    $ isValidPerimeter
    $ perimeter
    $ RoundedRectangle (Size 40 20) 1
  it "creates a valid perimeter for an elipse"
    $ isValidPerimeter
    $ perimeter
    $ Elipse (Size 10 5)
  it "creates a valid perimeter for a circle" $ do
    isValidPerimeter $ perimeter $ Elipse (Size 10 10)
    length (perimeter $ Elipse (Size 10 10)) `shouldBe` 20

triangulateSpec :: Spec
triangulateSpec = do
  it "ignores 'polygons' with less than 3 vertices" $ do
    triangulate [] `shouldBe` []
    triangulate [Point 0 0] `shouldBe` []
    triangulate [Point 0 0, Point 1 1] `shouldBe` []
  it "triangulates a triangle"
    $          triangulate [Point 0 0, Point 0 1, Point 1 1]
    `shouldBe` [(0, 1, 2)]
  it "triangulates non-rectilinear quadrilaterals" $ do
    triangulate [Point 0 0, Point (-1) 0.5, Point (-0.5) 1, Point 1 0.5]
      `shouldBe` [(2, 0, 1), (3, 0, 2)]
    triangulate [Point (-0.5) 1, Point 1 0.5, Point 0 0, Point (-1) 0.5]
      `shouldBe` [(0, 2, 3), (1, 2, 0)]
    triangulate [Point 0 0, Point (-1) 0.5, Point 0.5 1, Point 1 0.5]
      `shouldBe` [(2, 0, 1), (2, 3, 0)]
    triangulate [Point 0.5 1, Point 1 0.5, Point 0 0, Point (-1) 0.5]
      `shouldBe` [(0, 2, 3), (0, 1, 2)]
  it "triangulates a diamond"
    $ triangulate [Point 0 0, Point (-0.5) 0.5, Point 0 1, Point 0.5 0.5]
    `shouldBe` [(2, 0, 1), (2, 3, 0)]
  it "triangulates a rectangle"
    $          triangulate [Point 0 0, Point 0 1, Point 1 1, Point 1 0]
    `shouldBe` [(1, 3, 0), (2, 3, 1)]
  it "triangulates a uni-monotone quadrilateral" $ do
    let points = [Point 0 1, Point 3 1, Point 2 0, Point 1 0]
    triangulate points `shouldBe` [(0, 2, 3), (1, 2, 0)]
    triangulate (drop 1 points ++ take 1 points)
      `shouldBe` [(3, 1, 2), (0, 1, 3)]
    triangulate (drop 2 points ++ take 2 points)
      `shouldBe` [(2, 0, 1), (3, 0, 2)]
    triangulate (drop 3 points ++ take 3 points)
      `shouldBe` [(1, 3, 0), (2, 3, 1)]
  it "triangulates an upside down uni-monotone quadrilateral" $ do
    let points = [Point 0 0, Point 1 1, Point 2 1, Point 3 0]
    triangulate points `shouldBe` [(0, 1, 2), (3, 0, 2)]
    triangulate (drop 1 points ++ take 1 points)
      `shouldBe` [(3, 0, 1), (2, 3, 1)]
    triangulate (drop 2 points ++ take 2 points)
      `shouldBe` [(2, 3, 0), (1, 2, 0)]
    triangulate (drop 3 points ++ take 3 points)
      `shouldBe` [(1, 2, 3), (0, 1, 3)]
  it "triangulates a polygon with a vertical concavity"
    $          triangulate [Point 0 0, Point (-1) 1, Point 0 0.5, Point 1 1]
    `shouldBe` [(2, 0, 1), (2, 3, 0)]
  it "triangulates a polygon with a horizontal concavity on the left"
    $          triangulate [Point 0 0, Point 0.5 1, Point 0 2, Point 1 1]
    `shouldBe` [(1, 3, 0), (3, 1, 2)]
  it "triangulates a polygon with a horizontal concavity on the right"
    $          triangulate [Point 0 1, Point 1 2, Point 0.5 1, Point 1 0]
    `shouldBe` [(2, 3, 0), (1, 2, 0)]
  it "triangulates a polygon with one complex concavity on the upper right"
    $ triangulate [Point 0 1, Point 1.5 2, Point 3 1.8, Point 1 1, Point 2 0]
    `shouldBe` [(1, 3, 0), (3, 4, 0), (1, 2, 3)]
  it "triangulates a polygon with one complex concavity on the lower right"
    $ triangulate [Point 0 1, Point 2 2, Point 1 1, Point 3 0.8, Point 1.5 0]
    `shouldBe` [(2, 4, 0), (1, 2, 0), (3, 4, 2)]
  it "triangulates a polygon with two complex concavities on the right"
    $          triangulate
                 [ Point 0   1
                 , Point 1   2
                 , Point 1.5 1.5
                 , Point 0.5 1
                 , Point 1.5 0.5
                 , Point 1   0
                 ]
    `shouldBe` [(3, 5, 0), (1, 3, 0), (4, 5, 3), (1, 2, 3)]
  it "triangulates a christmas tree"
    $          triangulate christmasTree
    `shouldBe` [ (4, 2 , 3)
               , (2, 0 , 1)
               , (9, 10, 0)
               , (7, 8 , 9)
               , (5, 9 , 4)
               , (4, 9 , 2)
               , (2, 9 , 0)
               , (5, 7 , 9)
               , (6, 7 , 5)
               ]
  it "triangulates a skate ramp"
    $ triangulate [Point 5 0, Point 4 2, Point 3 3, Point 0 4, Point 6 4]
    `shouldBe` [(4, 0, 1), (4, 1, 2), (4, 2, 3)]
  it "triangulates a series of spikes"
    $          triangulate
                 [ Point 0 3
                 , Point 6 3
                 , Point 5 2
                 , Point 4 0
                 , Point 3 2
                 , Point 2 1
                 , Point 1 2
                 , Point 1 0
                 ]
    `shouldBe` [ (0, 6, 7)
               , (0, 4, 6)
               , (6, 4, 5)
               , (0, 2, 4)
               , (4, 2, 3)
               , (1, 2, 0)
               ]
  it "triangulates an upside down series of spikes"
    $          triangulate
                 [ Point 0 0
                 , Point 1 3
                 , Point 1 1
                 , Point 2 2
                 , Point 3 1
                 , Point 4 3
                 , Point 5 1
                 , Point 6 0
                 ]
    `shouldBe` [ (1, 2, 0)
               , (0, 2, 4)
               , (2, 3, 4)
               , (0, 4, 6)
               , (4, 5, 6)
               , (7, 0, 6)
               ]
  it "triangulates a circle"
    $ let shape = perimeter . Elipse $ Size 5 5
      in  isValidTriangulation shape $ triangulate shape
  it "triangulates a rounded rectangle"
    $ let shape = perimeter $ RoundedRectangle (Size 40 20) 5
      in  isValidTriangulation shape $ triangulate shape

-- A figure of a Christmas tree. Lots of spikes on the left and right, so a good
-- work-out for the triangulate function.
christmasTree :: Path
christmasTree =
  [ Point 4 0
  , Point 2 1
  , Point 3 1
  , Point 1 2
  , Point 2 2
  , Point 0 3
  , Point 8 3
  , Point 6 2
  , Point 7 2
  , Point 5 1
  , Point 6 1
  ]
