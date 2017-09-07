{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import qualified Control.Concurrent         as C
import           Control.Lens               (snoc, unsnoc)
import           Control.Monad              (forever)
import           Control.Monad.Fix          (MonadFix)

import           Reactive.Banana.Frameworks (AddHandler, MomentIO)
import qualified Reactive.Banana.Frameworks as RB

import           Reactive.Banana            ((<@))
import qualified Reactive.Banana            as RB

import qualified Graphics.Vty               as Vty

import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             ((<>))
import qualified Data.Sequence              as S

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

bText
  :: ( RB.MonadMoment m
     , MonadFix m
     )
  => RB.Event Vty.Event
  -> (Vty.Key -> [Vty.Modifier] -> RB.Event Vty.Event)
  -> m (RB.Behavior (S.Seq Char))
bText eEvts eKey' = mdo
  let tmsg = S.empty

  RB.accumB tmsg $ stolenLeftMost
    [ fromMaybe S.empty . fmap fst . unsnoc <$  eKey' Vty.KBS []
    , const S.empty                         <$  eKey' (Vty.KChar 'k') [Vty.MCtrl]
    , flip snoc                             <$> eAlphaNumChar
    ]
  where
    okChars =
      ['a'..'z'] <> ['A' .. 'Z'] <> ['0' .. '9']

    isAlphaNum (Vty.EvKey (Vty.KChar k) m)
      | k `elem` okChars && (null m || m == [Vty.MShift]) = Just k
      | otherwise = Nothing
    isAlphaNum _  = Nothing

    eAlphaNumChar =
      RB.filterJust (isAlphaNum <$> eEvts)

seqToList
  :: S.Seq a
  -> [a]
seqToList =
  foldr (:) []

lolLatin
  :: S.Seq Char
  -> S.Seq Char
lolLatin =
  S.fromList
  . unwords
  . fmap (`mappend` "us")
  . words
  . seqToList

bIncDec
  :: ( MonadFix m,
       RB.MonadMoment m
     )
  => (Vty.Key -> [Vty.Modifier] -> RB.Event Vty.Event)
  -> m (RB.Behavior Vty.Image)
bIncDec eKey' = mdo
  let z = (0 :: Int)
      ePlus  = eKey' (Vty.KChar '+') []
      eMinus = eKey' (Vty.KChar '-') []

      mkImg :: Int -> Vty.Image
      mkImg = Vty.string (Vty.defAttr `Vty.withBackColor` Vty.red) . show

  bNum' <- RB.accumB z $ stolenLeftMost
    [ (+ 1)        <$ ePlus
    , const z      <$ RB.filterE (<= 0) ( bNum' <@ eMinus )
    , (subtract 1) <$ eMinus
    ]

  pure $ mkImg <$> bNum'

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

  -- We're ticking at (if my numbers are correct) 24/ticks a second.
  eTicked <- RB.fromAddHandler $ addHandler tick

  let eKey' = eKey eEvts
      eEnter = eKey' Vty.KEnter []

      eTranslate = eKey' (Vty.KChar 't') [Vty.MCtrl]

      -- I looked this up, not going to lie
      eQuit = stolenLeftMost
        [ eKey' Vty.KEsc []
        , eKey' (Vty.KChar 'c') [Vty.MCtrl]
        ]

  bMsg <- bText eEvts eKey'

  bLatinus <- RB.stepper mempty
    ( lolLatin <$> bMsg <@ eTranslate )

  bEnter <- RB.accumB False $ RB.unions
    [ const True  <$ eEnter
    , const False <$ eTicked
    ]

  bCtrlBS <- RB.accumB Nothing $ RB.unions
    [ const . Just <$> eEvts
    ]

  bNum <- bIncDec eKey'
  -- This is the bit that needs most of the work... how does tree
  let bImg' = mconcat <$> sequenceA
                [ textInput . seqToList <$> bMsg
                , eventLine             <$> bEnter
                , eventLine             <$> bCtrlBS
                , textInput . seqToList <$> bLatinus
                , bNum
                ]

  RB.reactimate $ updateImage vty <$> bImg' <@ eTicked
  RB.reactimate $ quit vty tickT  <$ eQuit

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
