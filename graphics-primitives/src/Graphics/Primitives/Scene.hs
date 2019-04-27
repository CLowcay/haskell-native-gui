{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module: Graphics.Primitives.Scene
-- Description: Scene graph data structures
-- Copyright: (c) Callum Lowcay 2019
-- License: BSD3
-- Maintainer: cwslowcay@gmail.com
--
-- This module contains data declarations for the scene graph.
module Graphics.Primitives.Scene
  ( Size(..)
  , Point(..)
  , Vector(..)
  , Radius
  , Width
  , Path
  , Angle
  , Stop(..)
  , LRGBA(..)
  , Shape(..)
  , Paint(..)
  , LineJoin(..)
  , Scene(..)
  , Transform(..)
  )
where

import           Data.Hashable
import           GHC.Generics

-- | A colour in a linear RGB space with an alpha channel.
data LRGBA a = LRGBA !a !a !a !a deriving (Eq, Ord, Show, Generic, Hashable)

-- | A point in 2D space.
data Point = Point !Double !Double deriving (Eq, Ord, Show, Generic, Hashable)

-- | A 2D vector.
data Vector = Vector !Double !Double deriving (Eq, Ord, Show, Generic, Hashable)

-- | A 2D bounding box.
data Size = Size !Double !Double deriving (Eq, Ord, Show, Generic, Hashable)

-- | Width in pixels.
type Width = Double

-- | Radius of a cicle.
type Radius = Double

-- | A 2D path.
type Path = [Point]

-- | An angle in radians.
type Angle = Double

-- | A gradient stop. A pair of a Double between 0 and 1 indicating the position
-- of this stop, and the colour of this stop.
data Stop colour = Stop !Double !colour deriving (Eq, Ord, Show, Generic, Hashable)

-- | A shape.
data Shape = Rectangle !Size                -- ^ A rectangle.
           | RoundedRectangle !Size !Radius -- ^ A rounded rectangle.
           | Elipse !Size                   -- ^ An elipse.
           | Polygon !Path                  -- ^ A closed polygon.  The path must be wound anticlockwise.
           deriving (Eq, Ord, Show, Generic, Hashable)

-- | A fill for a shape.
data Paint = Solid !(LRGBA Double)                 -- ^ A solid color fill.
           | Gradient !Angle [Stop (LRGBA Double)] -- ^ A gradient fill.
           deriving (Eq, Ord, Show, Generic, Hashable)

-- | The line join type.
data LineJoin = Bevel | Mitre
              deriving (Eq, Ord, Bounded, Enum, Show, Generic, Hashable)

-- | A scene graph.
data Scene = Empty                          -- ^ The empty scene graph.
           | ClosedLine !Width !LineJoin !Path -- ^ A line with Width pixels.  To create a border from the perimeter path of a Shape, the Path must be wound anticlockwise and the Width a positive number.
           | Object !Shape ![Paint]         -- ^ A shape with some fills.
           | Group ![Transform] ![Scene]    -- ^ A group with a transformation.
           deriving (Eq, Ord, Show, Generic, Hashable)

-- | An affine transformation.
data Transform = Rotation !Angle            -- ^ Anticlockwise rotation through an angle in radians.
               | Scale !Vector              -- ^ Scaling by a vector.
               | Translation !Vector        -- ^ A translation along a vector.
               deriving (Eq, Ord, Show, Generic, Hashable)
