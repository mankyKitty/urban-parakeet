{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo               #-}
module Main where

import qualified Control.Concurrent         as C
import           Control.Monad              (forever)

import           Reactive.Banana.Frameworks (AddHandler, MomentIO)
import qualified Reactive.Banana.Frameworks as RB

import           Reactive.Banana            ((<@))
import qualified Reactive.Banana            as RB

import qualified Graphics.Vty               as Vty

import           System.Exit                (exitSuccess)

import           TextInput (bTextInput)
import           Common                     (KeyInput (..), greenText, keyInEvt,
                                             keyInModEvt, leftMost, mkKeyInput,
                                             seqToList, updateImage, yellowText)
import           IncDec                     (bIncDec)

data ES a = ES
  { addHandler :: AddHandler a
  , fireMe     :: a -> IO ()
  }

data Tick = Tick

data InputTag
  = A
  | B
  deriving ( Show, Eq )

nextInp :: InputTag -> InputTag
nextInp A = B
nextInp B = A

eLoop
  :: Vty.Vty
  -> ES Vty.Event
  -> IO ()
eLoop vty es = forever $ do
  evt <- Vty.nextEvent vty
  fireMe es evt

eTickLoop
  :: ES Tick
  -> IO ()
eTickLoop es = forever $ do
  C.threadDelay (1000000 `div` 24)
  fireMe es Tick

quit
  :: Vty.Vty
  -> C.ThreadId
  -> IO ()
quit vty tickTid =
  C.killThread tickTid
  >> Vty.shutdown vty
  >> exitSuccess

networkDesc
  :: ES Vty.Event
  -> ES Tick
  -> Vty.Vty
  -> C.ThreadId
  -> MomentIO ()
networkDesc vtyES tick vty tickTid = do
  let
    eventLine = yellowText . show
    textInput = greenText
    toE       = RB.fromAddHandler . addHandler

  -- Pull in our events from the Vty system
  eKey <- mkKeyInput <$> toE vtyES
  -- We're ticking at (if my numbers are correct) 24/ticks a second.
  eTicked <- toE tick

  let
    eForwardTab  = keyInEvt eKey (Vty.KChar '\t')

    eQuit = leftMost
      [ keyInEvt eKey Vty.KEsc
      , keyInModEvt eKey (Vty.KChar 'c') [Vty.MCtrl]
      ]

  bCtrlBS <- RB.stepper Nothing ( Just <$> keyInAllEvts eKey )
  bNum    <- bIncDec eKey

  bActive <- RB.accumB A ( nextInp <$ eForwardTab )
  bMsg1 <- bTextInput eKey bActive (== A)
  bMsg2 <- bTextInput eKey bActive (== B)

  -- This is the bit that needs most of the work... how does tree
  let
    bImg' = mconcat <$> sequenceA
      [ textInput . seqToList <$> bMsg1
      , textInput . seqToList <$> bMsg2
      , eventLine             <$> bActive
      , eventLine             <$> bCtrlBS
      , bNum
      ]

  RB.reactimate $ updateImage vty <$> bImg' <@ eTicked
  RB.reactimate $ quit vty tickTid <$ eQuit

main :: IO ()
main = do
  let mkES = uncurry ES <$> RB.newAddHandler
  vtty <- Vty.mkVty Vty.defaultConfig

  vtyES <- mkES
  tick <- mkES
  tickTid <- C.forkIO $ eTickLoop tick

  network <- RB.compile (networkDesc vtyES tick vtty tickTid)
  RB.actuate network

  _ <- eLoop vtty vtyES
  Vty.shutdown vtty
  putStrLn "annnnnd we're done"
