module Graphics.Primitives.CommonSpec
  ( spec
  )
where

import           Graphics.Primitives.Common
import           Graphics.Primitives.Scene
import           Test.Hspec

spec :: Spec
spec = describe "triangulate" $ do
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
      `shouldBe` [(2, 0, 1), (3, 0, 2)]
    triangulate [Point 0.5 1, Point 1 0.5, Point 0 0, Point (-1) 0.5]
      `shouldBe` [(0, 2, 3), (1, 2, 0)]
  it "triangulates a diamond"
    $ triangulate [Point 0 0, Point (-0.5) 0.5, Point 0 1, Point 0.5 0.5]
    `shouldBe` [(2, 0, 1), (3, 0, 2)]
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
    `shouldBe` [(2, 0, 1), (3, 0, 2)]
  it "triangulates a polygon with a horizontal concavity on the left"
    $          triangulate [Point 0 0, Point 0.5 1, Point 0 2, Point 1 1]
    `shouldBe` [(1, 3, 0), (3, 1, 2)]
  it "triangulates a polygon with a horizontal concavity on the right"
    $          triangulate [Point 0 1, Point 1 2, Point 0.5 1, Point 1 0]
    `shouldBe` [(3, 0, 2), (1,2,0)]
  it "triangulates a polygon with one complex concavity on the upper right"
    $ triangulate [Point 0 1, Point 1.5 2, Point 3 1.8, Point 1 1, Point 2 0]
    `shouldBe` [(0, 1, 3), (4, 0, 3), (1, 2, 3)]
  it "triangulates a polygon with one complex concavity on the lower right"
    $ triangulate [Point 0 1, Point 2 2, Point 1 1, Point 3 0.8, Point 1.5 0]
    `shouldBe` [(0, 2, 4), (1, 2, 0), (3, 4, 2)]
  it "triangulates a polygon with two complex concavities on the right"
    $          triangulate
                 [ Point 0   1
                 , Point 1   2
                 , Point 1.5 1.5
                 , Point 0.5 1
                 , Point 1.5 0.5
                 , Point 1   0
                 ]
    `shouldBe` [(0, 3, 5), (1, 3, 0), (4, 5, 3), (2, 3, 1)]

