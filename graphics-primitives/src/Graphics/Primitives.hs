-- |
-- Module: Graphics.Primitives
-- Description: Simple drawing operations
-- Copyright: (c) Callum Lowcay 2019
-- License: BSD3
-- Maintainer: cwslowcay@gmail.com
--
-- This module contains simple functions to construct Scene structures.
module Graphics.Primitives
  ( solid
  , solidA
  , gradient
  , gradientA
  , empty
  , rectangle
  , roundedRectangle
  , circle
  , ellipse
  , simplePolygon
  , fillAndStroke
  )
where

import           Data.Colour
import           Data.Colour.SRGB.Linear
import           Graphics.Primitives.Scene
import           Graphics.Primitives.Common

-- | Create a solid colour paint.
solid :: Colour Double -> Paint
solid colour = let RGB r g b = toRGB colour in Solid $ LRGBA r g b 1

-- | Create a solid colour paint with alpha.
solidA :: Colour Double -> Double -> Paint
solidA colour a = let RGB r g b = toRGB colour in Solid $ LRGBA r g b a

-- | Create a gradient paint.
gradient :: Angle -> [Stop (Colour Double)] -> Paint
gradient angle = Gradient angle . fmap
  (\(Stop pos colour) ->
    let RGB r g b = toRGB colour in Stop pos $ LRGBA r g b 1
  )

-- | Create a gradient paint with alpha.
gradientA :: Angle -> [Stop (Colour Double, Double)] -> Paint
gradientA angle = Gradient angle . fmap
  (\(Stop pos (colour, a)) ->
    let RGB r g b = toRGB colour in Stop pos $ LRGBA r g b a
  )

-- | Create an empty Scene.
empty :: Scene
empty = Empty

-- | Create a 'Rectangle'.
rectangle :: Size -> Shape
rectangle = Rectangle

-- | Create a 'RoundedRectangle'.
roundedRectangle :: Size -> Radius -> Shape
roundedRectangle = RoundedRectangle

-- | Create a circle.
circle :: Radius -> Shape
circle r = Ellipse $ Size r r

-- | Create an 'Ellipse'.
ellipse :: Size -> Shape
ellipse = Ellipse

-- | Create a simple polygon given a path. The path must not include any
-- duplicate edges. The polygon is closed by adding a line from the last vertex
-- in the path back to the first vertex in the path. The polygon described by
-- the path must not intersect itself and it must be wound anticlockwise. If any
-- of these conditions are not met then this function returns Nothing.
simplePolygon :: Path -> Maybe Shape
simplePolygon path =
  if isValidSimplePolygon path then Just $ Polygon path else Nothing

-- | Add fill and stroke to a shape.
fillAndStroke :: Shape -> [Paint] -> [(Width, LineJoin)] -> Scene
fillAndStroke shape fill [] = Object shape fill
fillAndStroke shape fill stroke =
  let path = perimeter shape
      makeStroke (width, lineJoin) = ClosedLine width lineJoin path
  in  Group [] (Object shape fill : (makeStroke <$> stroke))
