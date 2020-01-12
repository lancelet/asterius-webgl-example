module Windowing where

import           Asterius.Types
import           Data.Bits                      ( testBit )

newtype Canvas = Canvas { unCanvas :: JSVal }

data Windowing m
  = Windowing
    { createCanvas           :: m Canvas
    , sizeToFull             :: Canvas -> m ()
    , consoleDebug           :: String -> m ()
    , addResizeEventListener :: m () -> m ()
    , addMouseMoveCallback   :: Canvas -> (MouseEvent -> m ()) -> m ()
    }

data Button
  = Primary
  | Secondary
  | Auxilary
  deriving (Show)

data MouseEvent
  = MouseEvent
    { mouseClientX :: Int
    , mouseClientY :: Int
    , mouseButtons :: [Button]
    }
  deriving (Show)

create :: IO (Windowing IO)
create = pure $ Windowing
  { createCanvas           = Canvas <$> rawCreateCanvas
  , sizeToFull             = rawSizeToFull . unCanvas
  , consoleDebug           = \message -> rawConsoleDebug (toJSString message)
  , addResizeEventListener = \action -> do
                               jsFn <- makeHaskellCallback action
                               rawAddWindowListener (toJSString "resize") jsFn
  , addMouseMoveCallback   =
    \canvas action -> do
      let jsValCallback = \jsObject -> do
            mouseEvent <- convertMouseEvent jsObject
            action mouseEvent
      jsFn <- makeHaskellCallback1 jsValCallback
      rawAddEventListener (unCanvas canvas) (toJSString "mousemove") jsFn
  }

convertMouseEvent :: JSVal -> IO MouseEvent
convertMouseEvent obj = do
  clientX <- rawGetClientX obj
  clientY <- rawGetClientY obj
  buttons <- rawGetButtons obj
  pure MouseEvent { mouseClientX = clientX
                  , mouseClientY = clientY
                  , mouseButtons = convertButtonsBitField buttons
                  }

convertButtonsBitField :: Int -> [Button]
convertButtonsBitField x = concat
  [ if testBit x 0 then [Primary] else []
  , if testBit x 1 then [Secondary] else []
  , if testBit x 2 then [Auxilary] else []
  ]

foreign import javascript
  "${1}.addEventListener(${2}, ${3})"
  rawAddEventListener
  :: JSVal -> JSString -> JSFunction -> IO ()

foreign import javascript
  "${1}.clientX"
  rawGetClientX
  :: JSVal -> IO Int

foreign import javascript
  "${1}.clientY"
  rawGetClientY
  :: JSVal -> IO Int

foreign import javascript
  "${1}.buttons"
  rawGetButtons
  :: JSVal -> IO Int

foreign import javascript
  "(() => {                                           \
  \  document.body.style.margin = 0;                  \
  \  document.body.style.overflow = 'hidden';         \
  \  document.body.style.height = '100pc';            \
  \  const canvas = document.createElement('canvas'); \
  \  canvas.width = document.body.clientWidth;        \
  \  canvas.height = document.body.clientHeight;      \
  \  document.body.appendChild(canvas);               \
  \  return canvas;                                   \
  \ })()"
  rawCreateCanvas
  :: IO JSVal

foreign import javascript
  "window.addEventListener(${1}, ${2})"
  rawAddWindowListener
  :: JSString -> JSFunction -> IO ()

foreign import javascript
  "(() => {                                    \
  \  ${1}.width = document.body.clientWidth;   \
  \  ${1}.height = document.body.clientHeight; \
  \ })()"
  rawSizeToFull
  :: JSVal -> IO ()

foreign import javascript
  "console.debug(${1})"
  rawConsoleDebug
  :: JSString -> IO ()

