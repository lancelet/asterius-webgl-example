module Main where

import Asterius.Types
import Foreign.StablePtr

foreign import javascript "console.debug(${1})" jsConsoleDebug :: JSString -> IO ()
consoleDebug :: String -> IO ()
consoleDebug msg = (jsConsoleDebug . toJSString) msg

foreign import javascript
  "(() => {                                           \
  \  const canvas = document.createElement('canvas'); \
  \  canvas.width = ${1};                             \
  \  canvas.height = ${2};                            \
  \  document.body.appendChild(canvas);               \
  \  return canvas;                                   \
  \ })()"
  createCanvas :: Int -> Int -> IO JSObject

foreign import javascript "document.body.appendChild(${1})" appendToBody :: JSObject -> IO ()

foreign import javascript "${1}.getContext('webgl')" getWebGLContext :: JSObject -> IO JSObject

foreign import javascript "${1}.clearColor(${2}, ${3}, ${4}, ${5})" glClearColor :: JSObject -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript "${1}.clear(${2})" glClear :: JSObject -> Int -> IO ()

gl_COLOR_BUFFER_BIT :: Int
gl_COLOR_BUFFER_BIT = 0x00004000

foreign export javascript "mouseDownCallback" mouseDownCallback :: JSVal -> IO ()

mouseDownCallback :: JSVal -> IO ()
mouseDownCallback _ = consoleDebug "mouse down"

foreign import javascript "${1}" js_stableptr_id :: StablePtr Int -> IO (StablePtr Int)

foreign import javascript "${1}.addEventListener(${2}, ${3})" jsAddEventListener :: JSObject -> JSString -> JSFunction -> IO ()
addEventListener :: JSObject -> String -> (JSVal -> IO ()) -> IO ()
addEventListener obj name fn = do
  jsFn <- makeHaskellCallback1 fn
  jsAddEventListener obj (toJSString name) jsFn

main :: IO ()
main = do
  consoleDebug "Hello World from awgl!"
  canvas <- createCanvas 640 480
  gl <- getWebGLContext canvas
  glClearColor gl 0.5 0.5 0.5 0.9
  glClear gl gl_COLOR_BUFFER_BIT
  addEventListener canvas "mousedown" mouseDownCallback
  pure ()
