{-# LANGUAGE OverloadedStrings #-}

module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Snake

import Brick
  ( App(..), AttrMap, BricEvent(..), EventM
  , Next, Widget, customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hbox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle, str, attrMap, withAttr, emptyWidget
  , AttrName, on, fg, (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))


data Tick = Tick

type Name = ()


data Cell = Snake | Food | Empty

app :: app Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) = continue $ step g
handleEvent (VtyEvent (V.EvKey V.Kup [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO initGame >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey V.KRight [VMctrl])) = handleSpeed g (+)
handleEvent g (VtyEvent (V.EvKey V.KLeft [VMctrl])) = handleSpeed g (-)

handleSpeed :: Game -> (Float -> Float -> Float) -> EventM n (Next Game)
handleSpeed g (+/-) = do
    let newSp = validS $ (g ^. speed) +/- speedInc
    liftIO $ atomically $ writeTVar (g ^. interval) (spToInt newSp)
    continue $ g & speed .~ newSp
handleEvent g _                                     = continue g

speedInc :: Float
speedInc = 0.01

drawUI :: Game -> [Widget Name]
drawUI g = [ C.center $ padRigth (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget name
drawStats g = hLImit 11 $ vBox [ drawScore (g ^. score)
                               , padTop (Pad 2) $ drawGameOver (g ^. dead)
                               ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n


drawGameOver :: Bol -> Widget name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Snake")
  $ vBox row
    where
      rows = [hBox $ cellsInRow r | r <- [height-1, height-2..0]]
      cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
      drawCoord = drawCell . cellAt
      cellAt c
        | c `elem` g ^. snake = Snake
        | c == g ^. food = Food
        | otherwise = Empty

drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw
drawCell Food = withAttr foodAttr cw
drawCell Empty = withAttr emtpyAttr cw

cw :: WidgetName
cw = str " "

stakeAttr, foodAttr, emptyAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr, V.blue `on` V.blue)
  , (foodAttr, V.red `on` V.red)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold )
  ]

gameMain :: IO ()
gameMain = do
  chan <- newBChan 10
  tv <- newTVarIO (spToInt initialSpeed)
  forkIO $ forever $ do
      writeBChan chan Tick
      int <- readTVarIO tv
      threadDelay int
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  customMain initialVty buildVty (Just chan) app (initialGame tv) >>= printResult
