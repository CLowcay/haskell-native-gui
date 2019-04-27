-- |
-- Module: Graphics.Primitives
-- Description: Simple drawing operations
-- Copyright: (c) Callum Lowcay 2019
-- License: BSD3
-- Maintainer: cwslowcay@gmail.com
--
-- This module contains simple functions to constuct Scene structures.
module Graphics.Primitives where

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

-- | Add fill and stroke to a shape.
fillAndStroke :: Shape -> [Paint] -> [(Width, LineJoin)] -> Scene
fillAndStroke shape fill [] = Object shape fill
fillAndStroke shape fill stroke =
  let path = perimeter shape
      makeStroke (width, lineJoin) = ClosedLine width lineJoin path
  in  Group [] (Object shape fill : (makeStroke <$> stroke))
