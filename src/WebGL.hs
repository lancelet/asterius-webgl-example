{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module WebGL where

import Asterius.Types
import Data.Colour (AlphaColour, Colour)
import qualified Data.Colour
import Data.Colour.SRGB (RGB(RGB))
import qualified Data.Colour.SRGB
import qualified Data.Vector
import qualified Data.Vector.Unboxed
import Linear (V3(V3))

import Windowing (Canvas, unCanvas)

pattern GL_COLOR_BUFFER_BIT :: Int
pattern GL_COLOR_BUFFER_BIT = 0x00004000

type GLbitfield = Int

pattern STATIC_DRAW :: Int
pattern STATIC_DRAW = 0x88E4

pattern STREAM_DRAW :: Int
pattern STREAM_DRAW = 0x88E0

pattern DYNAMIC_DRAW :: Int
pattern DYNAMIC_DRAW = 0x88E8

data BufferUsage = StaticDraw | DynamicDraw | StreamDraw

pattern ARRAY_BUFFER :: Int
pattern ARRAY_BUFFER = 0x8892

pattern ELEMENT_ARRAY_BUFFER :: Int
pattern ELEMENT_ARRAY_BUFFER = 0x8893

data BufferType = ArrayBuffer | ElementArrayBuffer

newtype Buffer = Buffer { unBuffer :: JSVal }

newtype Shader = Shader { unShader :: JSVal }

pattern VERTEX_SHADER :: Int
pattern VERTEX_SHADER = 0x8B31

pattern FRAGMENT_SHADER :: Int
pattern FRAGMENT_SHADER = 0x8B30
  
data ShaderType = VertexShader | FragmentShader

newtype Program = Program { unProgram :: JSVal }

data FixedShaderProgram
  = FixedShaderProgram
    { fixedVertexShaderSource   :: String
    , fixedFragmentShaderSource :: String 
    }

compileFixedShaderProgram
  :: Monad m
  => WebGL m
  -> FixedShaderProgram
  -> m Program
compileFixedShaderProgram gl fsp = do
  -- vertex shader
  vertShader <- createShader gl VertexShader
  shaderSource gl vertShader (fixedVertexShaderSource fsp)
  compileShader gl vertShader
  -- fragment shader
  fragShader <- createShader gl FragmentShader
  shaderSource gl fragShader (fixedFragmentShaderSource fsp)
  compileShader gl fragShader
  -- program
  program <- createProgram gl
  attachShader gl program vertShader
  attachShader gl program fragShader
  linkProgram gl program
  pure program

bufferDataV3Float
  :: WebGL m
  -> Data.Vector.Unboxed.Vector (V3 Float)
  -> BufferUsage
  -> m ()
bufferDataV3Float gl v u = bufferDataFloat gl ArrayBuffer v' u
  where
    v' = Data.Vector.Unboxed.concatMap convert v
    convert (V3 x y z) = Data.Vector.Unboxed.fromList [x, y, z]

bufferDataColourFloat
  :: WebGL m
  -> Data.Vector.Vector (Colour Float)
  -> BufferUsage
  -> m ()
bufferDataColourFloat gl v u = bufferDataFloat gl ArrayBuffer v' u
  where
    v' = Data.Vector.Unboxed.fromList
         . concat
         . fmap convert
         . Data.Vector.toList
         $ v
    convert color = [r, g, b]
      where
        RGB r g b = Data.Colour.SRGB.toSRGB color

data WebGL m
  = WebGL
    { attachShader    :: Program -> Shader -> m ()
    , bindBuffer      :: BufferType -> Buffer -> m ()
    , bufferDataFloat :: BufferType
                      -> Data.Vector.Unboxed.Vector Float
                      -> BufferUsage
                      -> m ()
    , bufferDataInt   :: BufferType
                      -> Data.Vector.Unboxed.Vector Int
                      -> BufferUsage
                      -> m ()
    , clear           :: GLbitfield -> m ()
    , clearColor      :: AlphaColour Float -> m ()
    , compileShader   :: Shader -> m ()
    , createBuffer    :: m Buffer
    , createProgram   :: m Program
    , createShader    :: ShaderType -> m Shader
    , linkProgram     :: Program -> m ()
    , shaderSource    :: Shader -> String -> m ()
    }

createWebGLCanvasContext :: Canvas -> IO (WebGL IO)
createWebGLCanvasContext canvas = do
  context <- rawGetWebGLContext (unCanvas canvas)
  pure $ WebGL
    { attachShader = \program shader ->
        rawAttachShader context (unProgram program) (unShader shader)
    , bindBuffer   = \bufferType buffer ->
        rawBindBuffer context (bufferTypeToInt bufferType) (unBuffer buffer)
    , bufferDataFloat = \bufferType v usage -> do
        float32Array <- floatVectorToFloat32Array v
        rawBufferData context
                      (bufferTypeToInt bufferType)
                      float32Array
                      (bufferUsageToInt usage)
    , bufferDataInt = \bufferType v usage -> do
        intArray <- intVectorToUint16Array v
        rawBufferData context
                      (bufferTypeToInt bufferType)
                      intArray
                      (bufferUsageToInt usage)
    , clear        = rawClear context
    , clearColor   = rawTypedClearColor context
    , compileShader = \shader -> rawCompileShader context (unShader shader)
    , createBuffer = Buffer <$> rawCreateBuffer context
    , createProgram = Program <$> rawCreateProgram context
    , createShader = \shaderType ->
                       Shader <$> rawCreateShader context 
                                  (shaderTypeToInt shaderType)
    , linkProgram = \program -> rawLinkProgram context (unProgram program)
    , shaderSource = \shader code -> rawShaderSource context
                                                     (unShader shader)
                                                     (toJSString code)
    }

bufferTypeToInt :: BufferType -> Int
bufferTypeToInt = \case
  ArrayBuffer        -> ARRAY_BUFFER
  ElementArrayBuffer -> ELEMENT_ARRAY_BUFFER

bufferUsageToInt :: BufferUsage -> Int
bufferUsageToInt = \case
  StaticDraw  -> STATIC_DRAW
  DynamicDraw -> DYNAMIC_DRAW
  StreamDraw  -> STREAM_DRAW

shaderTypeToInt :: ShaderType -> Int
shaderTypeToInt = \case
  VertexShader   -> VERTEX_SHADER
  FragmentShader -> FRAGMENT_SHADER

foreign import javascript
  "${1}.getContext('webgl')"
  rawGetWebGLContext
  :: JSVal -> IO JSVal

foreign import javascript
  "${1}.attachShader(${2}, ${3})"
  rawAttachShader
  :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript
  "${1}.bindBuffer(${2}, ${3})"
  rawBindBuffer
  :: JSVal -> Int -> JSVal -> IO ()

foreign import javascript
  "${1}.bufferData(${2}, ${3}, ${4})"
  rawBufferData
  :: JSVal -> Int -> JSVal -> Int -> IO ()

foreign import javascript
  "${1}.clear(${2})"
  rawClear
  :: JSVal -> GLbitfield -> IO ()

foreign import javascript
  "${1}.clearColor(${2}, ${3}, ${4}, ${5})"
  rawClearColor
  :: JSVal -> Float -> Float -> Float -> Float -> IO ()

rawTypedClearColor :: JSVal -> AlphaColour Float -> IO ()
rawTypedClearColor gl alphaColour = rawClearColor gl red green blue alpha
  where
    Data.Colour.SRGB.RGB red green blue = srgb
    srgb = Data.Colour.SRGB.toSRGB
      (alphaColour `Data.Colour.over` Data.Colour.black)
    alpha = Data.Colour.alphaChannel alphaColour

foreign import javascript
  "${1}.compileShader(${2})"
  rawCompileShader
  :: JSVal -> JSVal -> IO ()

foreign import javascript
  "${1}.createBuffer()"
  rawCreateBuffer
  :: JSVal -> IO JSVal

foreign import javascript
  "${1}.createProgram()"
  rawCreateProgram
  :: JSVal -> IO JSVal

foreign import javascript
  "${1}.createShader(${2})"
  rawCreateShader
  :: JSVal -> Int -> IO JSVal

foreign import javascript
  "${1}.linkProgram(${2})"
  rawLinkProgram
  :: JSVal -> JSVal -> IO ()

foreign import javascript
  "${1}.shaderSource(${2}, ${3})"
  rawShaderSource
  :: JSVal -> JSVal -> JSString -> IO ()

---- Vector / Array handling

intVectorToUint16Array :: Data.Vector.Unboxed.Vector Int -> IO JSVal
intVectorToUint16Array v = do
  let len = Data.Vector.Unboxed.length v
  array <- rawNewUint16ArrayByLen len
  Data.Vector.Unboxed.imapM_ (rawSetIntElement array) v
  pure array

floatVectorToFloat32Array :: Data.Vector.Unboxed.Vector Float -> IO JSVal
floatVectorToFloat32Array v = do
  let len = Data.Vector.Unboxed.length v
  array <- rawNewFloat32ArrayByLen len
  Data.Vector.Unboxed.imapM_ (rawSetFloatElement array) v
  pure array

foreign import javascript
  "new Float32Array(${1})"
  rawNewFloat32ArrayByLen
  :: Int -> IO JSVal

foreign import javascript
  "new Uint16Array(${1})"
  rawNewUint16ArrayByLen
  :: Int -> IO JSVal

foreign import javascript
  "${1}[${2}] = ${3}"
  rawSetFloatElement
  :: JSVal -> Int -> Float -> IO ()

foreign import javascript
  "${1}[${2}] = ${3}"
  rawSetIntElement
  :: JSVal -> Int -> Int -> IO ()
