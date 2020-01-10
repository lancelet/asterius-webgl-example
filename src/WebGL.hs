{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
module WebGL where

import Asterius.Types (JSVal)
import Data.Colour (AlphaColour)
import qualified Data.Colour
import qualified Data.Colour.SRGB

import Windowing (Canvas, unCanvas)

pattern GL_COLOR_BUFFER_BIT :: Int
pattern GL_COLOR_BUFFER_BIT = 0x00004000

type GLbitfield = Int

data WebGL m
  = WebGL
    { glClear      :: GLbitfield -> m ()
    , glClearColor :: AlphaColour Float -> m ()
    }

createWebGLCanvasContext :: Canvas -> IO (WebGL IO)
createWebGLCanvasContext canvas = do
  context <- rawGetWebGLContext (unCanvas canvas)
  pure $ WebGL
    { glClear = rawGLClear context
    , glClearColor = rawTypedClearColor context
    }

foreign import javascript
  "${1}.getContext('webgl')"
  rawGetWebGLContext
  :: JSVal -> IO JSVal
  
foreign import javascript
  "${1}.clear(${2})"
  rawGLClear
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
