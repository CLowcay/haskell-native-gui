{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.StateVar
import           Foreign.C.Types
import           Foreign.Marshal
import           Foreign.Ptr
import           Data.Proxy
import           Foreign.Storable
import qualified Data.ByteString               as B
import qualified Graphics.Rendering.OpenGL     as GL
import qualified SDL
import           Control.Monad
import           Control.Monad.Reader

data RenderState = RenderState {
    renderIdentityMatrix :: GL.GLmatrix Float
  , renderProgram :: GL.Program
  , renderWindow :: SDL.Window
} deriving (Eq, Ord, Show)

data Paint = Paint {
    paintVertexBuffer :: !GL.BufferObject
  , paintElementBuffer :: !GL.BufferObject
  , paintVertexArray :: !GL.VertexArrayObject
  , paintNumElements :: !GL.GLint
  --, paintTexture :: Maybe TextureObject
} deriving (Eq, Ord, Show)

data Shape = Shape {
    geoVertexBuffer :: !GL.BufferObject
  , geoElementBuffer :: !GL.BufferObject
  , geoVertexArray :: !GL.VertexArrayObject
  , geoNumElements :: !GL.GLint
  , geoSize :: !(GL.GLmatrix Float)
} deriving (Eq, Ord, Show)

data Object = Object {
    objectShape :: !Shape
  , objectPaint :: [Paint]
} deriving (Eq, Ord, Show)

data Direction = Vertical | Horizontal deriving (Eq, Ord, Bounded, Enum, Show)

type Pos = (Float, Float)
type Size = (Float, Float)

renderObject :: Object -> ReaderT RenderState IO ()
renderObject Object {..} = do
  RenderState {..} <- ask
  let Shape {..} = objectShape
  liftIO $ do
    modelIn <- get $ GL.uniformLocation renderProgram "model"
    GL.uniform modelIn $= renderIdentityMatrix

    GL.colorMask $= GL.Color4 GL.Disabled GL.Disabled GL.Disabled GL.Disabled
    GL.bindVertexArrayObject $= Just geoVertexArray
    GL.drawElements GL.Triangles geoNumElements GL.UnsignedInt nullPtr
    GL.colorMask $= GL.Color4 GL.Enabled GL.Enabled GL.Enabled GL.Enabled

    GL.uniform modelIn $= geoSize
    GL.stencilOp $= (GL.OpKeep, GL.OpKeep, GL.OpKeep)
    forM_ objectPaint $ \Paint {..} -> do
      GL.bindVertexArrayObject $= Just paintVertexArray
      GL.drawElements GL.Triangles paintNumElements GL.UnsignedInt nullPtr
    GL.stencilOp $= (GL.OpReplace, GL.OpReplace, GL.OpReplace)

    GL.clear [GL.StencilBuffer]

lightPurple :: GL.Color4 Float
lightPurple = GL.Color4 0.87 0.74 0.98 1

darkPurple :: GL.Color4 Float
darkPurple = GL.Color4 0.4 0.16 0.56 1

makeExampleScene :: GL.Program -> IO [Object]
makeExampleScene program = do
  shape <- makeRoundedRect (50, 50) (100, 25) 4 program
  -- shape <- makeRect (50, 50) (100, 25) program
  fill1 <- makeGradient Vertical lightPurple lightPurple 0 0.05 program
  fill2 <- makeGradient Vertical lightPurple darkPurple 0.05 1 program
  pure [Object shape [fill1, fill2]]

makeRect :: Pos -> Size -> GL.Program -> IO Shape
makeRect (x, y) (w, h) program = do
  vertexBuffer <- loadVertexData [x, y, x + w, y, x + w, y + h, x, y + h]
  vao          <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  setVertexAttrib program "position" False 2 GL.Float (Proxy @Float) 2 0
  elementBuffer <- loadElementData [0 :: GL.GLuint, 3, 2, 2, 1, 0]

  modelMatrix   <- GL.newMatrix GL.ColumnMajor
    $ concat [[w, 0, 0, 0], [0, h, 0, 0], [0, 0, 1, 0], [x, y, 0, 1]]

  pure $ Shape vertexBuffer elementBuffer vao 6 modelMatrix

makeRoundedRect :: Pos -> Size -> Int -> GL.Program -> IO Shape
makeRoundedRect (x, y) (w, h) rounding0 program = if rounding0 < 1
  then makeRect (x, y) (w, h) program
  else do
    let r = fromIntegral rounding0 `min` (w / 2) `min` (h / 2)
    let baseVertexData =
          [ (x        , y + r)
          , (x + r    , y + r)
          , (x + r    , y)
          , (x + w - r, y)
          , (x + w - r, y + r)
          , (x + w    , y + r)
          , (x + w    , y + h - r)
          , (x + w - r, y + h - r)
          , (x + w - r, y + h)
          , (x + r    , y + h)
          , (x + r    , y + h - r)
          , (x        , y + h - r)
          ]
    print baseVertexData
    let nIntermediatePoints = floor r - 2
    let stepSize = pi / (2 * fromIntegral nIntermediatePoints)
    let intermediatePoints =
          (\theta -> (r * cos theta, r * sin theta))
            .   (* stepSize)
            .   fromIntegral
            <$> [0 .. nIntermediatePoints - 1]
    let
      allIntermediatePoints =
        ((\(a, b) -> (x + r - a, y + r - b)) <$> intermediatePoints)
          ++ ((\(a, b) -> (x + w - r + a, y + r - b)) <$> intermediatePoints)
          ++ ((\(a, b) -> (x + w - r + a, y + h - r + b)) <$> intermediatePoints
             )
          ++ ((\(a, b) -> (x + r - a, y + h - r + b)) <$> intermediatePoints)

    vertexBuffer <-
      loadVertexData
      .   concat
      $   (\(a, b) -> [a, b])
      <$> baseVertexData
      ++  allIntermediatePoints

    vao <- GL.genObjectName
    GL.bindVertexArrayObject $= Just vao
    setVertexAttrib program "position" False 2 GL.Float (Proxy @Float) 2 0

    let intermediateRange i = take nIntermediatePoints
          $ drop (12 + (nIntermediatePoints * i)) [0 ..]
    let triangles base outline =
          (\(a, b) -> (base, a, b)) <$> outline `zip` drop 1 outline
    let allTriangles =
          triangles 1 (0 : intermediateRange 0 ++ [2])
            ++ triangles 4  (5 : intermediateRange 1 ++ [3])
            ++ triangles 7  (6 : intermediateRange 2 ++ [8])
            ++ triangles 10 (11 : intermediateRange 3 ++ [9])

    let elements =
          concat
            $   (\(a, b, c) -> [a, b, c])
            <$> [ (0 :: GL.GLuint, 11, 10)
                , (10            , 1 , 0)
                , (1             , 4 , 3)
                , (3             , 2 , 1)
                , (1             , 7 , 4)
                , (4             , 7 , 6)
                , (6             , 5 , 4)
                , (1             , 10, 7)
                , (7             , 10, 9)
                , (9             , 8 , 7)
                ]
            ++  allTriangles

    print elements

    elementBuffer <- loadElementData elements

    modelMatrix   <- GL.newMatrix GL.ColumnMajor
      $ concat [[w, 0, 0, 0], [0, h, 0, 0], [0, 0, 1, 0], [x, y, 0, 1]]

    pure $ Shape vertexBuffer
                 elementBuffer
                 vao
                 (fromIntegral $ length elements)
                 modelMatrix

makeGradient
  :: Direction
  -> GL.Color4 Float
  -> GL.Color4 Float
  -> Float
  -> Float
  -> GL.Program
  -> IO Paint
makeGradient direction startColor endColor startPos endPos program = do
  let GL.Color4 r0 g0 b0 a0 = startColor
  let GL.Color4 r1 g1 b1 a1 = endColor
  let vertexData =
        [ (startPos, 0, r0, g0, b0, a0)
        , (startPos, 1, r0, g0, b0, a0)
        , (endPos  , 1, r1, g1, b1, a1)
        , (endPos  , 0, r1, g1, b1, a1)
        ]
  let flatVertexData = case direction of
        Horizontal ->
          concatMap (\(a, b, c, d, e, f) -> [a, b, c, d, e, f]) vertexData
        Vertical ->
          concatMap (\(a, b, c, d, e, f) -> [b, a, c, d, e, f]) vertexData

  vbo <- loadVertexData flatVertexData
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  setVertexAttrib program "position" False 2 GL.Float (Proxy @Float) 6 0
  setVertexAttrib program "color"    False 4 GL.Float (Proxy @Float) 6 2

  elementBuffer <- loadElementData $ case direction of
    Horizontal -> [0 :: GL.GLuint, 1, 2, 2, 3, 0]
    Vertical   -> [0 :: GL.GLuint, 3, 2, 2, 1, 0]

  pure $ Paint vbo elementBuffer vao 6

setVertexAttrib
  :: forall a
   . Storable a
  => GL.Program
  -> String
  -> Bool
  -> GL.NumComponents
  -> GL.DataType
  -> Proxy a
  -> GL.GLint
  -> Int
  -> IO ()
setVertexAttrib program input normalize numComponents dataType _ stride offset
  = do
    let dataSize = sizeOf (undefined :: a)
    inputAttrib <- get $ GL.attribLocation program input
    GL.vertexAttribArray inputAttrib $= GL.Enabled
    GL.vertexAttribPointer inputAttrib
      $= ( if normalize then GL.ToNormalizedFloat else GL.ToFloat
         , GL.VertexArrayDescriptor numComponents
                                    dataType
                                    (stride * fromIntegral dataSize)
                                    (nullPtr `plusPtr` (offset * dataSize))
         )

setVertexAttribConst :: GL.VertexAttrib a => GL.Program -> String -> a -> IO ()
setVertexAttribConst program input value = do
  inputAttrib <- get $ GL.attribLocation program input
  GL.vertexAttribArray inputAttrib $= GL.Disabled
  GL.vertexAttrib GL.ToFloat inputAttrib value

loadVertexData :: forall a . Storable a => [a] -> IO GL.BufferObject
loadVertexData = loadBufferData GL.ArrayBuffer

loadElementData :: forall a . Storable a => [a] -> IO GL.BufferObject
loadElementData = loadBufferData GL.ElementArrayBuffer

loadBufferData
  :: forall a . Storable a => GL.BufferTarget -> [a] -> IO GL.BufferObject
loadBufferData bufferTarget vdata = do
  vbo <- GL.genObjectName
  GL.bindBuffer bufferTarget $= Just vbo
  withArray vdata $ \pdata ->
    GL.bufferData bufferTarget
      $= ( fromIntegral $ length vdata * sizeOf (undefined :: a)
         , pdata
         , GL.StaticRead
         )
  pure vbo

viewportMatrix :: CInt -> CInt -> IO (GL.GLmatrix Float)
viewportMatrix w h = GL.newMatrix GL.ColumnMajor $ concat
  [ [2.0 / fromIntegral w, 0, 0, 0]
  , [0, -2.0 / fromIntegral h, 0, 0]
  , [0, 0, 1, 0]
  , [-1, 1, 0, 1]
  ]

setViewport :: CInt -> CInt -> ReaderT RenderState IO ()
setViewport w h = do
  program <- asks renderProgram
  liftIO $ do
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    projectionIn <- get $ GL.uniformLocation program "projection"
    mat          <- viewportMatrix w h
    GL.uniform projectionIn $= mat

loadShaders :: IO GL.Program
loadShaders = do
  vshader       <- GL.createShader GL.VertexShader
  vshaderSource <- B.readFile "test.vert.glsl"
  GL.shaderSourceBS vshader $= vshaderSource
  GL.compileShader vshader

  fshader       <- GL.createShader GL.FragmentShader
  fshaderSource <- B.readFile "test.frag.glsl"
  GL.shaderSourceBS fshader $= fshaderSource
  GL.compileShader fshader

  program <- GL.createProgram
  GL.attachShader program vshader
  GL.attachShader program fshader
  GL.bindFragDataLocation program "outColor" $= 0
  GL.linkProgram program
  GL.currentProgram $= Just program

  pure program

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow
    "Example window"
    SDL.defaultWindow
      { SDL.windowOpenGL    = Just $ SDL.defaultOpenGL
                                { SDL.glProfile = SDL.Core SDL.Normal 3 3
                                }
      , SDL.windowResizable = True
      }
  glContext <- SDL.glCreateContext window
  SDL.glMakeCurrent window glContext
  SDL.swapInterval $= SDL.SynchronizedUpdates

  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.stencilTest $= GL.Enabled
  GL.stencilMask $= 0xFF
  GL.stencilFunc $= (GL.Equal, 1, 0xFF)
  GL.stencilOp $= (GL.OpReplace, GL.OpReplace, GL.OpReplace)
  program  <- loadShaders

  idMatrix <- GL.newMatrix GL.ColumnMajor
    $ concat [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]

  let renderState = RenderState idMatrix program window

  SDL.V2 w h <- SDL.glGetDrawableSize window
  runReaderT (setViewport w h) renderState

  exampleScene <- makeExampleScene program

  SDL.showWindow window
  runReaderT (eventLoop exampleScene) renderState
  SDL.destroyWindow window

eventLoop :: [Object] -> ReaderT RenderState IO ()
eventLoop scene = eventLoop'
 where
  eventLoop' = do
    RenderState {..} <- ask
    liftIO $ GL.clearColor $= GL.Color4 1 1 1 1
    liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer, GL.StencilBuffer]
    forM_ scene renderObject
    SDL.glSwapWindow renderWindow

    mevent <- SDL.pollEvent
    case mevent of
      Nothing    -> eventLoop'
      Just event -> case SDL.eventPayload event of
        SDL.WindowSizeChangedEvent _ -> do
          SDL.V2 w h <- SDL.glGetDrawableSize renderWindow
          setViewport w h
          eventLoop'
        SDL.QuitEvent -> pure ()
        _             -> eventLoop'
