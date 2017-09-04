{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Control.Concurrent         as C
import Control.Applicative (liftA2)
import           Control.Monad              (forever)

import           Reactive.Banana.Frameworks (AddHandler, MomentIO)
import qualified Reactive.Banana.Frameworks as RB

import           Reactive.Banana            ((<@))
import qualified Reactive.Banana            as RB

import qualified Graphics.Vty               as Vty

import qualified Data.Sequence as S
import Data.Monoid ((<>))

import           System.Exit                (exitSuccess)

data ES a = ES
  { addHandler :: AddHandler a
  , fireMe     :: a -> IO ()
  }

eLoop
  :: Vty.Vty
  -> ES Vty.Event
  -> IO ()
eLoop vty es = forever $ do
  evt <- Vty.nextEvent vty
  fireMe es evt

data Tick = Tick
  deriving Show

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

updateImage
  :: Vty.Vty
  -> Vty.Image
  -> IO ()
updateImage vty =
  Vty.update vty . Vty.picForImage

eKey
  :: RB.Event Vty.Event
  -> Vty.Key
  -> [Vty.Modifier]
  -> RB.Event Vty.Event
eKey allEvts k m =
  RB.filterE (== Vty.EvKey k m) allEvts

stolenLeftMost
  :: [RB.Event a]
  -> RB.Event a
stolenLeftMost =
  foldl (RB.unionWith const) RB.never

networkDesc
  :: ES Vty.Event
  -> ES Tick
  -> Vty.Vty
  -> C.ThreadId
  -> MomentIO ()
networkDesc vtyES tick vty tickT = do
  let eventLine e = Vty.string (Vty.defAttr `Vty.withBackColor` Vty.yellow) (show e)
      textInput t = Vty.string (Vty.defAttr `Vty.withBackColor` Vty.green) t
  -- Pull in our events from the Vty system
  eEvts <- RB.fromAddHandler $ addHandler vtyES

  let eKey' = eKey eEvts
  -- We're ticking at (if my numbers are correct) 24/ticks a second.
  eTicked <- RB.fromAddHandler $ addHandler tick

  let
      eEnter = eKey' Vty.KEnter []

      bkSpc msg = case S.viewr msg of
              S.EmptyR -> S.empty
              (xs S.:> _) -> xs

      isChar (Vty.EvKey (Vty.KChar k) _) = Just k
      isChar _                           = Nothing

      eChars =
        RB.filterJust (isChar <$> eEvts)

      -- I looked this up, not going to lie
      eQuit = stolenLeftMost
        [ eKey' Vty.KEsc []
        , eKey' (Vty.KChar 'c') [Vty.MCtrl]
        ]

  bMsg <- RB.accumB S.empty $ RB.unions
    [ (\c -> (S.|> c)) <$> eChars
    , bkSpc <$ eKey' Vty.KBS []
    ]

  bEnter <- RB.accumB False $ RB.unions
    [ const True <$ eEnter
    , const False <$ eTicked
    ]

  let bImg' = liftA2 (<>)
        (textInput . (foldr (:) []) <$> bMsg)
        (eventLine <$> bEnter)

  RB.reactimate $ updateImage vty <$> bImg' <@ eTicked
  RB.reactimate $ quit vty tickT <$ eQuit

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
