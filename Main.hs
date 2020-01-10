{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Colour
import Data.Colour.Names

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
        WebGL.glClear gl (WebGL.GL_COLOR_BUFFER_BIT)
  Windowing.addResizeEventListener wnd resizeHandler

  let mouseMoveCallback :: Windowing.MouseEvent -> IO () =
        \mouseEvent -> do
          Windowing.consoleDebug wnd (show mouseEvent)
  Windowing.addMouseMoveCallback wnd canvas mouseMoveCallback

  let bgColor = Data.Colour.Names.grey `Data.Colour.withOpacity` 0.9

  WebGL.glClearColor gl bgColor
  WebGL.glClear gl (WebGL.GL_COLOR_BUFFER_BIT)
