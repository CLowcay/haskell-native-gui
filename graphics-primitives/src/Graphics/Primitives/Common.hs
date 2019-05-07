{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Graphics.Primitives.Common
-- Description: Geometric utilities
-- Copyright: (c) Callum Lowcay 2019
-- License: BSD3
-- Maintainer: cwslowcay@gmail.com
--
-- This module contains common geometric operations that might be shared among
-- backends.
module Graphics.Primitives.Common
  (
  -- * Path operations
    perimeter
  , isValidSimplePolygon
  , Triangle
  , triangulate
  -- * Utilities
  -- | Some useful geometric operations.
  , Side(..)
  , whichSide
  , WindingDirection(..)
  , windingDirection
  , segmentsIntersect
  , segmentsAdjacent
  , intersectsWithSelf
  )
where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.State.Strict
import           Data.Array
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Ord
import           GHC.Generics
import           Graphics.Primitives.Scene
import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S

-- | Construct a path around the perimeter of a shape. The path is wound
-- anticlockwise.
perimeter :: Shape -> Path
perimeter (Rectangle (Size w h)) = [Point 0 0, Point 0 h, Point w h, Point w 0]
perimeter (Polygon   path      ) = path
perimeter (Ellipse (Size w h)) =
  let nSteps   = floor (w `min` h) :: Int
      stepSize = pi / fromIntegral nSteps
      rx       = w / 2
      ry       = h / 2
  in  (\theta -> Point (rx + rx * cos theta) (ry - ry * sin theta))
        .   (* stepSize)
        .   fromIntegral
        <$> [0 .. nSteps * 2 - 1]
perimeter (RoundedRectangle (Size w h) r0) = if r0 < 1
  then perimeter $ Rectangle (Size w h)
  else
    let
      r                   = r0 `min` (w / 2) `min` (h / 2)
      nIntermediatePoints = floor r - 2 :: Int
      stepSize            = pi / (2 * (1 + fromIntegral nIntermediatePoints))
      intermediatePoints =
        (\theta -> (r * cos theta, r * sin theta))
          .   (* stepSize)
          .   fromIntegral
          <$> [1 .. nIntermediatePoints]
      lowerLeft (a, b) = Point (r - a) (h - r + b)
      lowerRight (a, b) = Point (w - r + a) (h - r + b)
      upperRight (a, b) = Point (w - r + a) (r - b)
      upperLeft (a, b) = Point (r - a) (r - b)
    in
      (Point 0 r : Point 0 (h - r) : (lowerLeft <$> intermediatePoints))
      ++ ( Point r       h
         : Point (w - r) h
         : (lowerRight <$> reverse intermediatePoints)
         )
      ++ (Point w (h - r) : Point w r : (upperRight <$> intermediatePoints))
      ++ ( Point (w - r) 0
         : Point r       0
         : (upperLeft <$> reverse intermediatePoints)
         )

-- | The direction in which a polygon is wound.
data WindingDirection = CW   -- ^ Clockwise winding.
                      | CCW  -- ^ Counter-clockwise (anticlockwise) winding.
                      deriving (Eq, Ord, Show, Bounded, Enum, Generic)

-- | Determine the winding direction of a polygon.
windingDirection :: Path -> WindingDirection
windingDirection path =
  if sum (zipWith signedArea path $ drop 1 (cycle path)) > -2e-300
    then CCW
    else CW
  where signedArea (Point ax ay) (Point bx by) = (ay + by) * (bx - ax)

-- | Which side of a line a point is on.
data Side = LeftSide | RightSide deriving (Eq, Ord, Show, Bounded, Enum)

-- | A data type for line segments that are ordered from top to bottom
data Segment = Segment !Point !Point deriving (Eq, Show)
instance Ord Segment where
  Segment a1@(Point ax1 _) a2 <= Segment b1@(Point bx1 _) b2 = if ax1 <= bx1
    then case whichSide a1 a2 b1 of
      Just side -> side == RightSide
      Nothing   -> whichSide a1 a2 b2 /= Just LeftSide
    else case whichSide b1 b2 a1 of
      Just side -> side == LeftSide
      Nothing   -> whichSide b1 b2 a2 == Just LeftSide

-- | Give three points a, b, and c, determine which side of the line directed
-- from a to b does c fall on. Returns 'Nothing' if the three points are
-- co-linear.
whichSide :: Point -> Point -> Point -> Maybe Side
whichSide (Point x1 y1) (Point x2 y2) (Point x y) =
  let det     = (x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)
      epsilon = 2e-300
  in  if det > epsilon
        then Just LeftSide
        else if det < -epsilon then Just RightSide else Nothing

-- | Determine if two line segments intersect.
segmentsIntersect :: (Point, Point) -> (Point, Point) -> Bool
segmentsIntersect (a1, a2) (b1, b2) =
  boundingBoxesOverlap
    && fromMaybe True ((/=) <$> whichSide a1 a2 b1 <*> whichSide a1 a2 b2)
    && fromMaybe True ((/=) <$> whichSide b1 b2 a1 <*> whichSide b1 b2 a2)
 where
  boundingBoxesOverlap =
    let
      Point ax1 ay1 = a1
      Point ax2 ay2 = a2
      Point bx1 by1 = b1
      Point bx2 by2 = b2
      sort2 (a, b) = if a <= b then (a, b) else (b, a)
      rangesOverlap (p1, p2) (p3, p4) = if p1 <= p3 then p3 <= p2 else p1 <= p4
    in
      rangesOverlap (sort2 (ax1, ax2)) (sort2 (bx1, bx2))
        && rangesOverlap (sort2 (ay1, ay2)) (sort2 (by1, by2))

-- | Determine if two line segments are adjacent.
segmentsAdjacent :: (Point, Point) -> (Point, Point) -> Bool
segmentsAdjacent (aStart, aEnd) (bStart, bEnd) =
  aStart == bEnd || aStart == bStart || aEnd == bStart || aEnd == bEnd

-- | Determine if a path intersects with itself
intersectsWithSelf :: Path -> Bool
intersectsWithSelf path =
  not (allUnique path) || hasIntersection S.empty endPoints
 where
  allUnique l = length l == S.size (S.fromList l)
  endPoints =
    sortOn endPoint
      . concat
      $ zipWith
          (\p1 p2 -> if p1 <= p2
            then [(LeftSide, p1, p2), (RightSide, p1, p2)]
            else [(LeftSide, p2, p1), (RightSide, p2, p1)]
          )
          path
      $ drop 1 (cycle path)
  endPoint segment = case segment of
    (LeftSide , p, _) -> p
    (RightSide, _, p) -> p

  intersectsNotAdjacent (Segment a1 a2) (Segment b1 b2) =
    let a = (a1, a2)
        b = (b1, b2)
    in  segmentsIntersect a b && not (segmentsAdjacent a b)

  hasIntersection :: S.Set Segment -> [(Side, Point, Point)] -> Bool
  hasIntersection _         []               = False
  hasIntersection sweepLine (segment : rest) = case segment of
    (LeftSide, start, end) ->
      let
        thisSegment  = Segment start end
        segmentAbove = S.lookupLT thisSegment sweepLine
        segmentBelow = S.lookupGT thisSegment sweepLine
        intersectsAbove =
          maybe False (intersectsNotAdjacent thisSegment) segmentAbove
        intersectsBelow =
          maybe False (intersectsNotAdjacent thisSegment) segmentBelow
      in
        intersectsAbove
        || intersectsBelow
        || hasIntersection (S.insert thisSegment sweepLine) rest
    (RightSide, start, end) ->
      let
        thisSegment  = Segment start end
        segmentAbove = S.lookupLT thisSegment sweepLine
        segmentBelow = S.lookupGT thisSegment sweepLine
        intersects =
          fromMaybe False
            $   intersectsNotAdjacent
            <$> segmentAbove
            <*> segmentBelow
      in
        intersects || hasIntersection (S.delete thisSegment sweepLine) rest

-- | Check if a polygon is simple and wound anticlockwise. These conditions are
-- required by the 'triangulate' function.
isValidSimplePolygon :: Path -> Bool
isValidSimplePolygon path =
  windingDirection path == CCW && not (intersectsWithSelf path)

-- | A triangle formed from the vertices of a path, represented as indices into
-- the path.
type Triangle = (Int, Int, Int)

-- | Internal state for our triangulation algorithm. The algorithm is
-- triangulation by trapezoid decomposition. This is a scan-line algorithm.
data ScanLine = ScanLine {
    sSegments :: !(M.Map Segment Trapezoid) -- ^ Line segments that intersect the current scan line.
  , sDiagonals :: !(IM.IntMap [Int])        -- ^ Extra diagonals that have been added during trapezoidation.
} deriving (Eq, Ord, Show)

-- | Information about an unclosed trapezoid.
data Trapezoid = Trapezoid {
    tStart :: !Int         -- ^ The leftmost point of the trapezoid.
  , tSegment1 :: !Segment  -- ^ The line segment that marks the upper boundary of this trapezoid.
  , tSegment2 :: !Segment  -- ^ The line segment that marks the lower boundary of this trapezoid.
} deriving (Eq, Ord, Show)

-- | Information about the two points on the polygon that are adjacent to some
-- other point.
data AdjacentPoints = TwoOnRight !Int !Int        -- ^ The two points are to the right of the original point.
                     | OneLeftOneRight !Int !Int  -- ^ The two points are on either side of the original point.  They are ordered left to right.
                     | TwoOnLeft !Int !Int        -- ^ The two points are to the left of the other original point.
                     deriving (Eq, Ord, Show)

-- | Triangulate a simple polygon described by a 'Path'. The path must be
-- validated by 'isValidSimplePolygon', otherwise 'triangulate' will return
-- incorrect results or throw an exception. If the path has less than 3 vertices
-- then return an empty triangulation.
triangulate :: Path -> [Triangle]
triangulate []        = []
triangulate [_]       = []
triangulate [_, _]    = []
triangulate [_, _, _] = [(0, 1, 2)]
triangulate path =
  concat $ evalState (traverse doTriangulation sortedPoints) $ ScanLine
    M.empty
    IM.empty
 where
  makeSegment i j =
    let a = indexedPath ! i
        b = indexedPath ! j
    in  if a <= b then Segment a b else Segment b a
  endYCoord (Segment _ (Point _ y)) = y
  pathLength = length path
  prev i = (i + pathLength - 1) `mod` pathLength
  next i = (i + 1) `mod` pathLength
  indexedPath  = listArray (0, pathLength - 1) path
  sortedPoints = sortOn (indexedPath !) [0 .. pathLength - 1]
  isAdjacent a b = next a == b || next b == a

  -- | Given the index of a point, find the two adjacent points.
  getAdjacentPoints :: Int -> AdjacentPoints
  getAdjacentPoints i =
    let (i1, i2)     = (prev i, next i)
        (pc, p1, p2) = (indexedPath ! i, indexedPath ! i1, indexedPath ! i2)
    in  if
          | p1 <= pc && pc <= p2 -> OneLeftOneRight i1 i2
          | p2 <= pc && pc <= p1 -> OneLeftOneRight i2 i1
          | p1 <= pc && p2 <= pc -> TwoOnLeft i1 i2
          | otherwise            -> TwoOnRight i1 i2

  -- | Given a point, get the trapezoid (if any) that this point lines inside of.
  getEnclosingTrapezoid :: Int -> State ScanLine (Maybe Trapezoid)
  getEnclosingTrapezoid i = do
    segments <- gets sSegments
    let s     = makeSegment i i
    let above = snd <$> M.lookupLE s segments
    let below = snd <$> M.lookupGE s segments
    pure $ do
      tAbove <- above
      tBelow <- below
      if tAbove == tBelow then Just tAbove else Nothing

  -- | Perform one round of the scan-line algorithm from the given point.
  doTriangulation :: Int -> State ScanLine [Triangle]
  doTriangulation i = case getAdjacentPoints i of
    TwoOnRight i1 i2 -> do
      mEnclosing <- getEnclosingTrapezoid i
      case mEnclosing of
        Nothing ->
          -- A completely new trapezoid.
          addTrapezoid $ Trapezoid i (makeSegment i i1) (makeSegment i i2)
        Just Trapezoid {..} -> do
          -- Point i lies inside some other trapezoid, so split that trapezoid in two.
          let (iTop, iBottom) = sortVertical (i1, i2)
          let [sTop, sBottom] = sortOn endYCoord [tSegment1, tSegment2]
          addDiagonal tStart i
          addTrapezoid $ Trapezoid i sTop (makeSegment i iTop)
          addTrapezoid $ Trapezoid i sBottom (makeSegment i iBottom)
      pure []

    TwoOnLeft i1 i2 -> do
      let s1 = makeSegment i i1
      let s2 = makeSegment i i2
      t1 <- getTrapezoid s1
      t2 <- getTrapezoid s2

      -- Since we scan left to right, we must have already seen both of these
      -- line segments, so getTrapezoid will always return a Just value.
      assert (isJust t1) $ pure ()
      assert (isJust t2) $ pure ()
      let tz1 = fromJust t1
      let tz2 = fromJust t2

      -- If the two trapezoids are the same then we're done. Close the last
      -- trapezoid and wind the final polygon.
      unitonePolygons <- if tz1 == tz2
        then catMaybes <$> sequence
          [ closeTrapezoid (tStart tz1) i
          , Just <$> if next i == i1 then windPolygon i i1 else windPolygon i1 i
          ]
        else do
          -- Otherwise we're merging two trapezoids. Create a new trapezoid with
          -- the two line segments that aren't complete yet (the two line
          -- segments not equal to s1 or s2)
          addTrapezoid $ Trapezoid
            i
            (if s1 == tSegment1 tz1 || s2 == tSegment1 tz1
              then tSegment2 tz1
              else tSegment1 tz1
            )
            (if s1 == tSegment1 tz2 || s2 == tSegment1 tz2
              then tSegment2 tz2
              else tSegment1 tz2
            )
          catMaybes <$> sequence
            [closeTrapezoid (tStart tz1) i, closeTrapezoid (tStart tz2) i]

      pure . concat $ triangulateUnitone <$> unitonePolygons

    OneLeftOneRight l r -> do
      let oldSegment = makeSegment l i
      mtz <- getTrapezoid oldSegment

      -- Since we scan left to right, we must have already seen oldSegment, so
      -- getTrapezoid must return a Just value.
      assert (isJust mtz) $ pure ()
      let tz = fromJust mtz

      addTrapezoid $ Trapezoid
        i
        (makeSegment i r)
        (if tSegment1 tz == oldSegment then tSegment2 tz else tSegment1 tz)

      maybe [] triangulateUnitone <$> closeTrapezoid (tStart tz) i

  -- Sort two vertices in order of their y coordinates.
  sortVertical :: (Int, Int) -> (Int, Int)
  sortVertical (a, b) =
    let Point xa ya = indexedPath ! a
        Point xb yb = indexedPath ! b
    in  if (ya, xb) <= (yb, xa) then (a, b) else (b, a)

  -- Add a diagonal across the polygon.
  addDiagonal :: Int -> Int -> State ScanLine ()
  addDiagonal d1 d2 = modify $ \s -> s
    { sDiagonals = insertMapList d2 d1 . insertMapList d1 d2 $ sDiagonals s
    }
    where insertMapList k v = IM.alter (\l -> ((v :) <$> l) <|> Just [v]) k

  -- Add a trapezoid to the current state.
  addTrapezoid :: Trapezoid -> State ScanLine ()
  addTrapezoid t@Trapezoid {..} = modify $ \s -> s
    { sSegments = M.insert tSegment1 t $ M.insert tSegment2 t $ sSegments s
    }

  -- Get the trapezoid associated with a line segment and remove it from the
  -- current state.
  getTrapezoid :: Segment -> State ScanLine (Maybe Trapezoid)
  getTrapezoid segment = do
    s <- get
    let r = M.lookup segment $ sSegments s
    put $ s { sSegments = M.delete segment $ sSegments s }
    pure r

  -- If this trapezoid can be closed, then add a diagonal and carve off a
  -- uni-monotone polygon for triangulation. If this trapezoid cannot be closed
  -- then return Nothing.
  closeTrapezoid :: Int -> Int -> State ScanLine (Maybe [(Int, Point)])
  closeTrapezoid start i = if isAdjacent start i
    then pure Nothing
    else do
      let (d1, d2) = sortVertical (start, i)
      addDiagonal d2 d1
      Just <$> windPolygon d2 d1

  -- Given a line segment directed anticlockwise, extract the polygon by walking
  -- around its perimeter.
  windPolygon :: Int -> Int -> State ScanLine [(Int, Point)]
  windPolygon i0 i1 = do
    allDiagonals <- gets sDiagonals
    let closestToi0 =
          minimumBy (comparing $ \d -> (i0 - d + pathLength) `mod` pathLength)
    let nextCandidates i = next i : IM.findWithDefault [] i allDiagonals

    -- Make sure we have at least 3 distinct points.
    let i2 = closestToi0 . filter (/= i0) $ nextCandidates i1

    let winding = flip unfoldr i2 $ \i ->
          -- Choose the next point that gets us back to i0 the fastest. This
          -- will give the smallest possible polygon while maintaining
          -- anticlockwise winding.
          let i3 = closestToi0 $ nextCandidates i
          in  if i3 == i0 then Nothing else Just ((i3, indexedPath ! i3), i3)
    pure
      $ (i0, indexedPath ! i0)
      : (i1, indexedPath ! i1)
      : (i2, indexedPath ! i2)
      : winding

-- | Triangulate a uni-monotone polygon. The input points are augmented with
-- their indices in the original path.
triangulateUnitone :: [(Int, Point)] -> [Triangle]
triangulateUnitone []                       = assert False []
triangulateUnitone [_]                      = assert False []
triangulateUnitone [_, _]                   = assert False []
triangulateUnitone [(a, _), (b, _), (c, _)] = [(a, b, c)]
triangulateUnitone path =
  let (stack, triangles) = mapAccumL clipEars [v0] crown
  in  concat triangles ++ fanLeftOvers stack
 where
  xcoord (Point x _) = x

  -- Index of v0 in the original path.
  iv0 = fst . minimumBy (comparing $ xcoord . snd . snd) $ [0 ..] `zip` path

  -- Index of v1 in the oriented path.
  maxXcoord = maximum $ xcoord . snd <$> orientedPath
  iv1
    | (xcoord . snd . head $ orientedPath) == maxXcoord
    = 0
    | (xcoord . snd . last $ orientedPath) == maxXcoord
    = length orientedPath - 1
    | otherwise
    = error
      $  "v0 and v1 are not adjacent, this is not a unitone polygon: "
      ++ show path
      ++ " "
      ++ show (v0 : orientedPath)

  -- The oriented path is a rotation of the original path with v0 as the first
  -- vertex.
  v0 : orientedPath = let (l, r) = splitAt iv0 path in r ++ l
  v1                = orientedPath !! iv1

  -- The direction we have to turn in to get an ear. If the crown is above the
  -- line v0-v1 then the winding direction is to the right. If the crown is
  -- below the line v0-v1 then the winding direction is to the left.
  turnDirection     = if iv1 == 0 then RightSide else LeftSide

  -- The crown always runs from left to right, even if that is counter to the
  -- original winding order.
  crown             = case turnDirection of
    LeftSide  -> init orientedPath
    RightSide -> reverse $ tail orientedPath

  -- Given the current stack and the next point in the crown, clip as many ears
  -- as possible and return those triangles along with the new stack.
  clipEars :: [(Int, Point)] -> (Int, Point) -> ([(Int, Point)], [Triangle])
  clipEars stack pt = clipAvailableEars [] (pt : stack)

  -- Given a stack of points, recursively clip as many ears as possible
  -- returning those triangles along with the new stack. This function uses an
  -- accumulator parameter for tail recursion.
  clipAvailableEars
    :: [Triangle] -> [(Int, Point)] -> ([(Int, Point)], [Triangle])
  clipAvailableEars acc []     = ([], acc)
  clipAvailableEars acc [a]    = ([a], acc)
  clipAvailableEars acc [b, a] = ([b, a], acc)
  clipAvailableEars acc stack@(c : b : a : rest) =
    let triangle = case turnDirection of
          LeftSide  -> (fst a, fst b, fst c)
          RightSide -> (fst a, fst c, fst b)
    in  if isEar (snd a) (snd b) (snd c)
          then clipAvailableEars (triangle : acc) (c : a : rest)
          else (stack, acc)

  isEar :: Point -> Point -> Point -> Bool
  isEar a b c = whichSide a b c == Just turnDirection

  -- Given the left over stack when ear-clipping is complete, create a fan of
  -- triangles centred on v1.
  fanLeftOvers :: [(Int, Point)] -> [Triangle]
  fanLeftOvers [] =
    error
      $  "Empty stack after attempting to triangulate "
      ++ show path
      ++ " "
      ++ show (v0 : orientedPath)
  fanLeftOvers stack =
    let makeTriangle a b = (fst v1, fst a, fst b)
    in  case turnDirection of
          LeftSide  -> zipWith makeTriangle (drop 1 stack) stack
          RightSide -> zipWith makeTriangle stack (drop 1 stack)
