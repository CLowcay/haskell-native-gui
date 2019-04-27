{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  ( perimeter
  , Triangle
  , triangulate
  )
where

import           Control.Exception
import           Control.Monad.State.Strict
import           Control.Applicative
import           Data.Array
import           Data.Foldable
import           Data.List
import           Data.Ord
import           Debug.Trace
import           Graphics.Primitives.Scene
import qualified Data.IntMap.Strict            as M

-- | Construct a path around the perimeter of a shape. The path is wound
-- anticlockwise.
perimeter :: Shape -> Path
perimeter (Rectangle (Size w h)) = [Point 0 0, Point 0 h, Point w h, Point w 0]
perimeter (Polygon   path      ) = path
perimeter (Elipse (Size w h)) =
  let nSteps   = floor (w `min` h) :: Int
      stepSize = pi / fromIntegral nSteps
      rx       = w / 2
      ry       = h / 2
  in  (\theta -> Point (rx + rx * cos theta) (ry - ry * sin theta))
        .   (* stepSize)
        .   fromIntegral
        <$> [0 .. (nSteps * 2)]
perimeter (RoundedRectangle (Size w h) r0) = if r0 < 1
  then perimeter $ Rectangle (Size w h)
  else
    let
      r                   = r0 `min` (w / 2) `min` (h / 2)
      nIntermediatePoints = floor r - 2 :: Int
      stepSize            = pi / (2 * fromIntegral nIntermediatePoints)
      intermediatePoints =
        (\theta -> (r * cos theta, r * sin theta))
          .   (* stepSize)
          .   fromIntegral
          <$> [0 .. nIntermediatePoints - 1]
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

-- | A triangle formed from the vertices of a path, represented as indices into
-- the path.
type Triangle = (Int, Int, Int)

-- | Triangulate a polygon described by a Path. If the Path has less than 3
-- vertices then return an empty triangulation. Returns triples of indexes into
-- the path, each triple representing one of the triangles.
triangulate :: Path -> [Triangle]
triangulate []        = []
triangulate [_]       = []
triangulate [_, _]    = []
triangulate [_, _, _] = [(0, 1, 2)]
triangulate path      = traceShow ("--start--", path) $ concat $ evalState
  (traverse doTriangulation sortedPoints)
  initState
 where
  -- The initial state includes a diagonal from the last vertex back to the
  -- start. This simplifies the windPolygon function.
  initState  = Trapezoidation M.empty M.empty
  pathLength = length path
  prev i = (i + pathLength - 1) `mod` pathLength
  next i = (i + 1) `mod` pathLength
  indexedPath  = listArray (0, pathLength - 1) path
  sortedPoints = sortOn (indexedPath !) [0 .. pathLength - 1]
  hasDiagonal a b = next a == b || next b == a

  -- Determine if a point is inside a trapezoid.
  isInsideTrapezoid :: Point -> Trapezoid -> Bool
  isInsideTrapezoid (Point x y) Trapezoid {..} =
    let Point x0 _    = indexedPath ! tStart
        (start1, _)   = geoSort (prev tSegment1, next tSegment1)
        (start2, _)   = geoSort (prev tSegment2, next tSegment2)
        Point ax0 ay0 = indexedPath ! start1
        Point ax1 ay1 = indexedPath ! tSegment1
        Point bx0 by0 = indexedPath ! start2
        Point bx1 by1 = indexedPath ! tSegment2
        p1            = (ax1 - ax0) / (x - ax0)
        p2            = (bx1 - bx0) / (x - bx0)
        yb1           = (ay1 - ay0) * p1 + ay0
        yb2           = (by1 - by0) * p2 + by0
    in  assert (x >= x0 && x <= ax1 && x <= bx1)
          $  (yb1 <= y && y <= yb2)
          || (yb2 <= y && y <= yb1)

  -- Get and remove the trapezoid that this point is inside of, or return
  -- Nothing if there is no such trapezoid.
  getEnclosingTrapezoid :: Point -> State Trapezoidation (Maybe Trapezoid)
  getEnclosingTrapezoid pt = do
    mtz <- find (pt `isInsideTrapezoid`) . concat . toList <$> gets trapezoids
    case mtz of
      Nothing -> pure Nothing
      Just tz -> do
        modify (\s -> s { trapezoids = removeTrapezoid (trapezoids s) tz })
        pure $ Just tz

  -- If this trapezoid can be closed, then add a diagonal and carve off a
  -- uni-monotone polygon for triangulation. If this trapezoid cannot be closed
  -- then return an empty list.
  closeTrapezoid :: Trapezoid -> Int -> State Trapezoidation [(Int, Point)]
  closeTrapezoid Trapezoid {..} i = if hasDiagonal tStart i
    then traceShow ("HAS DIAG", tStart, i) $ pure []
    else do
      traceShow ("NO DIAG", tStart, i) $ pure ()
      addDiagonal tStart i
      windPolygon tStart i

  -- Given a line segment on the lower right hand side of a polygon, extract the
  -- polygon by walking around its perimeter.
  windPolygon :: Int -> Int -> State Trapezoidation [(Int, Point)]
  windPolygon d1 d2 = do
    -- Walk the line segment bottom to top.
    let (i1, i0) = sortVertical (d1, d2)
    traceShow ("WIND", i0, i1) $ pure ()

    allDiagonals <- gets diagonals
    let
      winding = flip unfoldr i0 $ \i ->
        -- Choose the next point that gets us back to i0 the fastest. This
        -- will give the smallest possible polygon while maintaining
        -- anticlockwise winding.
        let
          i2 =
            minimumBy (comparing $ \d -> (i0 - d + pathLength) `mod` pathLength)
              $ next i
              : M.findWithDefault [] i allDiagonals
        in  if i2 == i0 then Nothing else Just ((i2, indexedPath ! i2), i2)
    pure $ (i0, indexedPath ! i0) : (i1, indexedPath ! i1) : winding

  -- Sort two vertices in order of their y coordinates.
  sortVertical :: (Int, Int) -> (Int, Int)
  sortVertical (a, b) =
    let Point xa ya = indexedPath ! a
        Point xb yb = indexedPath ! b
    in  if (ya, xb) <= (yb, xa) then (a, b) else (b, a)

  -- Sort two vertices lexicographically by their coordinates.
  geoSort :: (Int, Int) -> (Int, Int)
  geoSort (a, b) =
    if indexedPath ! a <= indexedPath ! b then (a, b) else (b, a)

  -- Triangulate up to the given vertex.
  doTriangulation :: Int -> State Trapezoidation [Triangle]
  doTriangulation i = do
    state1 <- get
    traceShow ("triangulate", i, state1) $pure ()
    allTrapezoids <- getTrapezoids i
    state2        <- get
    traceShow ("trapsRemoved", i, state2) $pure ()
    case allTrapezoids of
      [] -> do
        mtz <- getEnclosingTrapezoid (indexedPath ! i)
        case mtz of
          Nothing -> do
            -- Create a new trapezoid.
            let (s1, s2) = geoSort (next i, prev i)
            addTrapezoid $ Trapezoid i s1 s2
            pure []
          Just tz -> do
            -- Split an existing trapezoid.
            let (s1, s4) = sortVertical (tSegment1 tz, tSegment2 tz)
            let (s2, s3) = sortVertical (next i, prev i)
            let (u1, u2) = geoSort (s1, s2) -- points for the upper trapezoid.
            let (l1, l2) = geoSort (s3, s4) -- points for the lower trapezoid.

            addDiagonal (tStart tz) i
            addTrapezoid $ Trapezoid i u1 u2
            addTrapezoid $ Trapezoid i l1 l2
            pure []

      tz1 : rest -> do
        -- We always process tSegment1 first.
        assert (i == tSegment1 tz1) $ pure ()

        -- There are never more than two trapezoids to consider.
        assert (length rest <= 1) $ pure ()

        -- If there are two trapezoids, then neither trapezoid is degenerate.
        assert
            (  null rest
            || (  (tSegment1 tz1 /= tSegment2 tz1)
               && ((tSegment1 $ head rest) /= (tSegment2 $ head rest))
               )
            )
          $ pure ()

        -- Get up to two possible uni-monotone polygons by closing off the
        -- current trapezoid(s).
        unitonePolygons <- case rest of
          [tz2] -> do
            -- Create the next trapezoid by merging the two current trapezoids.
            let (s1, s2) = geoSort (tSegment2 tz1, tSegment2 tz2)
            addTrapezoid $ Trapezoid (tSegment1 tz1) s1 s2

            -- Close the trapezoids. No degenerate cases to consider here.
            sequence [closeTrapezoid tz1 i, closeTrapezoid tz2 i]

          [] -> if tSegment1 tz1 /= tSegment2 tz1
            then do
              -- This is the normal case. Create the next trapezoid then close
              -- the previous one.
              let start = tSegment1 tz1
              let
                (s1, s2) = geoSort
                  (tSegment2 tz1, snd $ geoSort (next start, prev start))
              addTrapezoid $ Trapezoid start s1 s2

              sequence [closeTrapezoid tz1 i]
            else do
              -- Handle the degenerate case where tSegment1 == tSegment2.
              -- Attempt to close the current polygon first, then wind up the
              -- final polygon.
              sequence
                [closeTrapezoid tz1 i, windPolygon (tStart tz1) (tSegment1 tz1)]

          tooManyTrapezoids -> do
            s <- get
            error
              $  "Too many trapezoids: "
              ++ show tooManyTrapezoids
              ++ ".  Full state was "
              ++ show s
              ++ " from the original path "
              ++ show path

        -- Assert that there are no infinite loops in the generated polygons.
        assert
            (all (\p -> length (take (pathLength + 1) p) <= pathLength)
                 unitonePolygons
            )
          $ pure ()

        traceShow ("unitone: ", i, unitonePolygons) $ pure ()

        -- Triangulate the polygons.
        pure
          .   concat
          $   triangulateUnitone
          <$> filter (not . null) unitonePolygons

  -- Add a diagonal across the polygon.
  addDiagonal :: Int -> Int -> State Trapezoidation ()
  addDiagonal d1 d2 = modify $ \ts ->
    ts { diagonals = insertMapList d2 d1 . insertMapList d1 d2 $ diagonals ts }
    where insertMapList k v = M.alter (\l -> ((v :) <$> l) <|> Just [v]) k

  -- Add a trapezoid to the current trapezoidation state.
  addTrapezoid :: Trapezoid -> State Trapezoidation ()
  addTrapezoid tz@Trapezoid {..} = modify $ \ts -> ts
    { trapezoids = M.unionWith (++) (trapezoids ts)
                     $ M.fromList [(tSegment1, [tz]), (tSegment2, [tz])]
    }

  -- Get and remove all trapezoids with a line segment ending at a specified
  -- point.
  getTrapezoids :: Int -> State Trapezoidation [Trapezoid]
  getTrapezoids i = do
    tzs <- gets trapezoids
    case M.lookup i tzs of
      Nothing              -> pure []
      Just foundTrapezoids -> do
        modify $ \s ->
          s { trapezoids = foldl' removeTrapezoid tzs foundTrapezoids }
        pure foundTrapezoids

  -- Completely remove a trapezoid from the trapezoid list
  removeTrapezoid :: M.IntMap [Trapezoid] -> Trapezoid -> M.IntMap [Trapezoid]
  removeTrapezoid tzs tz@Trapezoid {..} =
    M.alter doRemoveTrapezoid tSegment2
      . M.alter doRemoveTrapezoid tSegment1
      $ tzs
   where
    doRemoveTrapezoid mltz = case mltz of
      Nothing -> Nothing
      Just ltz ->
        let ltz' = delete tz ltz in if null ltz' then Nothing else Just ltz'

-- | Current state of the trapezoidation calculation. The incomplete trapezoids
-- are indexed by their two neighbours, which means that each trapezoid is
-- indexed twice. When we remove a trapezoid we must be careful to also remove
-- its copy.
--
-- The diagonals are stored as an adjacency list. Each point maps to a list of
-- diagonals starting at that point.
data Trapezoidation = Trapezoidation {
    trapezoids :: !(M.IntMap [Trapezoid])  -- ^ Incomplete trapezoids.
  , diagonals :: !(M.IntMap [Int])         -- ^ Extra diagonals that have been added during trapezoidation.
} deriving (Eq, Ord, Show)

-- | Information about an incomplete trapezoid. The trapezoid is defined by the
-- vertex that introduced the trapezoid and two points that terminate the line
-- segments that form the top and bottom of the trapezoid. The terminating
-- vertices are lexicographically ordered.
-- 
-- The trapezoid will be complete when we process one of the two terminating
-- vertices.
data Trapezoid = Trapezoid {
    tStart :: !Int     -- ^ The first vertex we saw in this trapezoid.
  , tSegment1 :: !Int  -- ^ The first terminating vertex.
  , tSegment2 :: !Int  -- ^ The second terminating vertex.
} deriving (Eq, Ord, Show)

data Side = LeftSide | RightSide deriving (Eq, Ord, Show, Bounded, Enum)

-- | Triangulate a uni-monotone polygon. The input points are augmented with
-- their indices in the original path.
triangulateUnitone :: [(Int, Point)] -> [Triangle]
triangulateUnitone []                       = assert False []
triangulateUnitone [_]                      = assert False []
triangulateUnitone [_, _]                   = assert False []
triangulateUnitone [(a, _), (b, _), (c, _)] = [(a, b, c)]
triangulateUnitone path =
  let (stack, triangles) =
          traceShow (path, v0, v1, crown, turnDirection)
            $ mapAccumL clipEars [v0] crown
  in  concat triangles ++ fanLeftOvers stack
 where
  xcoord (Point x _) = x

  -- Index of v0 in the original path.
  iv0 = fst . minimumBy (comparing $ xcoord . snd . snd) $ [0 ..] `zip` path

  -- Index of v1 in the oriented path.
  iv1 =
    fst . maximumBy (comparing $ xcoord . snd . snd) $ [0 ..] `zip` orientedPath

  -- The oriented path is a rotation of the original path with v0 as the first
  -- vertex.
  v0 : orientedPath = let (l, r) = splitAt iv0 path in r ++ l
  v1                = orientedPath !! iv1

  -- The direction we have to turn in to get an ear. If the crown is above the
  -- line v0-v1 then the winding direction is to the right. If the crown is
  -- below the line v0-v1 then the winding direction is to the left.
  turnDirection
    | iv1 == 0
    = RightSide
    | iv1 == length orientedPath - 1
    = LeftSide
    | otherwise
    = error
      $  "v0 and v1 are not adjacent, this is not a unitone polygon: "
      ++ show path
      ++ " "
      ++ show (v0 : orientedPath)

  -- The crown always runs from left to right, even if that is counter to the
  -- original winding order.
  crown = case turnDirection of
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
  isEar = onSide turnDirection

  -- Determine if a point is on the specified side of the oriented line defined
  -- by the first two points.
  onSide :: Side -> Point -> Point -> Point -> Bool
  onSide side (Point x1 y1) (Point x2 y2) (Point x y) =
    let det = (x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)
    in  traceShow ("isEar", (x1, y1), (x2, y2), (x, y), det) $ case side of
          RightSide -> det < 0
          LeftSide  -> det > 0

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
    let makeTriangle (a, b) = (fst v1, fst a, fst b)
    in  case turnDirection of
          LeftSide  -> fmap makeTriangle $ drop 1 stack `zip` stack
          RightSide -> fmap makeTriangle $ stack `zip` drop 1 stack
