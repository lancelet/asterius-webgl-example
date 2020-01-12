{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NegativeLiterals #-}
module Main where

import Data.Colour (Colour)
import qualified Data.Colour
import qualified Data.Colour.Names
import Data.Colour.SRGB (sRGB)
import qualified Data.Vector
import qualified Data.Vector.Unboxed
import Linear (V3(V3))

import qualified WebGL
import qualified Windowing

main :: IO ()
main = do
  wnd <- Windowing.create
  canvas <- Windowing.createCanvas wnd
  gl <- WebGL.createWebGLCanvasContext canvas

  Windowing.consoleDebug wnd "Created app"

  let resizeHandler :: IO () = do
        Windowing.consoleDebug wnd "Resize handler"
        Windowing.sizeToFull wnd canvas
        WebGL.clear gl (WebGL.GL_COLOR_BUFFER_BIT)
  Windowing.addResizeEventListener wnd resizeHandler

  let mouseMoveCallback :: Windowing.MouseEvent -> IO () =
        \mouseEvent -> do
          Windowing.consoleDebug wnd (show mouseEvent)
  Windowing.addMouseMoveCallback wnd canvas mouseMoveCallback

  (vertexBuffer, colorBuffer, indexBuffer) <- createBuffers gl
  shaderProgram <- WebGL.compileFixedShaderProgram gl fixedShaderProgram

  let bgColor = Data.Colour.Names.grey `Data.Colour.withOpacity` 0.9

  WebGL.clearColor gl bgColor
  WebGL.clear gl (WebGL.GL_COLOR_BUFFER_BIT)

createBuffers
  :: WebGL.WebGL IO
  -> IO (WebGL.Buffer, WebGL.Buffer, WebGL.Buffer)
createBuffers gl = do
  -- vertex buffer
  vertexBuffer <- WebGL.createBuffer gl
  WebGL.bindBuffer gl WebGL.ArrayBuffer vertexBuffer
  WebGL.bufferDataV3Float gl vertices WebGL.StaticDraw
  
  -- colour buffer
  colorBuffer <- WebGL.createBuffer gl
  WebGL.bindBuffer gl WebGL.ArrayBuffer colorBuffer
  WebGL.bufferDataColourFloat gl colors WebGL.StaticDraw

  -- index buffer
  indexBuffer <- WebGL.createBuffer gl
  WebGL.bindBuffer gl WebGL.ElementArrayBuffer indexBuffer
  WebGL.bufferDataInt gl WebGL.ElementArrayBuffer indices WebGL.StaticDraw

  pure (vertexBuffer, colorBuffer, indexBuffer)

vertices :: Data.Vector.Unboxed.Vector (V3 Float)
vertices = Data.Vector.Unboxed.fromList
  [ V3 -1 -1 -1, V3  1 -1 -1, V3  1  1 -1, V3 -1  1 -1
  , V3 -1 -1  1, V3  1 -1  1, V3  1  1  1, V3 -1  1  1
  , V3 -1 -1 -1, V3 -1  1 -1, V3 -1  1  1, V3 -1 -1  1
  , V3  1 -1 -1, V3  1  1 -1, V3  1  1  1, V3  1 -1  1
  , V3 -1 -1 -1, V3 -1 -1  1, V3  1 -1  1, V3  1 -1 -1
  , V3 -1  1 -1, V3 -1  1  1, V3  1  1  1, V3  1  1 -1
  ]

colors :: Data.Vector.Vector (Colour Float)
colors = Data.Vector.fromList
  [ sRGB 0.5 0.3 0.7, sRGB 0.5 0.3 0.7, sRGB 0.5 0.3 0.7, sRGB 0.5 0.3 0.7
  , sRGB 0.3 0.3 1.0, sRGB 0.3 0.3 1.0, sRGB 0.3 0.3 1.0, sRGB 0.3 0.3 1.0
  , sRGB 0 0 1, sRGB 0 0 1, sRGB 0 0 1, sRGB 0 0 1
  , sRGB 1 0 0, sRGB 1 0 0, sRGB 1 0 0, sRGB 1 0 0
  , sRGB 1 1 0, sRGB 1 1 0, sRGB 1 1 0, sRGB 1 1 0
  , sRGB 0 1 0, sRGB 0 1 0, sRGB 0 1 0, sRGB 0 1 0
  ]

indices :: Data.Vector.Unboxed.Vector Int
indices = Data.Vector.Unboxed.fromList
  [ 0,1,2, 0,2,3, 4,5,6, 4,6,7 
  , 8,9,10, 8,10,11, 12,13,14, 12,14,15 
  , 16,17,18, 16,18,19, 20,21,22, 20,22,23 
  ]

fixedShaderProgram :: WebGL.FixedShaderProgram
fixedShaderProgram = WebGL.FixedShaderProgram
  { WebGL.fixedVertexShaderSource =
    unlines
    [ "attribute vec3 position;"
    , "uniform mat4 Pmatrix;"
    , "uniform mat4 Vmatrix;"
    , "uniform mat4 Mmatrix;"
    , "attribute vec3 color;"
    , "varying vec3 vColor;"
    , ""
    , "void main(void) { "
    , "  gl_Position = Pmatrix*Vmatrix*Mmatrix*vec4(position, 1.);"
    , "    vColor = color;"
    , "}"
    ]
  , WebGL.fixedFragmentShaderSource =
    unlines
    [ "precision mediump float;"
    , "varying vec3 vColor;"
    , "void main(void) {"
    , "  gl_FragColor = vec4(vColor, 1.);"
    , "}"
    ]
  }
