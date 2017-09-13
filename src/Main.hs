{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo               #-}
module Main where

import qualified Control.Concurrent         as C
import           Control.Lens               (snoc)
import           Control.Monad              (forever)
import           Control.Monad.Fix          (MonadFix)

import           Reactive.Banana.Frameworks (AddHandler, MomentIO)
import qualified Reactive.Banana.Frameworks as RB

import           Reactive.Banana            ((<@))
import qualified Reactive.Banana            as RB

import qualified Graphics.Vty               as Vty

import           Data.Maybe                 (fromMaybe)

import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE

import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import qualified Data.Sequence              as S
import           System.Exit                (exitSuccess)

import           Common                     (KeyInput (..), greenText, keyInEvt,
                                             keyInModEvt, leftMost, mkKeyInput,
                                             seqToList, updateImage, yellowText)
import           IncDec                     (bIncDecNum)
import           TextInput                  (bTextInput)

data ES a = ES
  { addHandler :: AddHandler a
  , fireMe     :: a -> IO ()
  }

data Tick = Tick

data InputTag
  = A
  | EmailFld Int
  deriving ( Show, Eq )

newtype FieldId
  = Fld InputTag
  deriving (Show, Eq)

type InputSeq
  = NonEmpty FieldId

shiftToNextInput
  :: InputSeq
  -> InputSeq
shiftToNextInput is =
  NE.head is :| snoc (NE.tail is) ( NE.head is )

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
    eForwardTab = keyInEvt eKey (Vty.KChar '\t')
    ePlus       = keyInEvt eKey (Vty.KChar '+')
    eMinus      = keyInEvt eKey (Vty.KChar '-')

    minSeq = Fld A :| []

    eQuit = leftMost
      [ keyInEvt eKey Vty.KEsc
      , keyInModEvt eKey (Vty.KChar 'c') [Vty.MCtrl]
      ]

  -- Active field is a ``Seq Field`` that is a combination of the initial
  -- field(s), plus the number of other fields that have been created.
  let rmvLastEmail ix =
        fromMaybe minSeq
        . NE.nonEmpty
        . NE.filter (\(Fld (EmailFld ix')) -> ix' == ix)

      addEmailField i fs
        = NE.head fs :| snoc (NE.tail fs) (Fld (EmailFld i))

  bCtrlBS <- RB.stepper Nothing ( Just <$> keyInAllEvts eKey )
  bNum    <- bIncDecNum eKey

  bActive <- RB.accumB minSeq $ leftMost
    [ shiftToNextInput <$  eForwardTab
    , rmvLastEmail     <$> bNum <@ eMinus
    , addEmailField    <$> bNum <@ ePlus
    ]

  let
    eIncDec = leftMost [ePlus, eMinus]

    isAField a xs
      = Fld a == ( NE.head xs )

    mkEmailField =
      bTextInput eKey bActive . isAField . EmailFld

    exeEmailFields
      :: ( RB.MonadMoment m
         , MonadFix m
         )
      => Int
      -> m ( RB.Behavior (S.Seq Text) )
    exeEmailFields n =
      sequenceA <$> traverse mkEmailField ( S.fromList [0 .. n] )

    eFieldList =
      exeEmailFields <$> bNum <@ eIncDec

  bName <- bTextInput eKey bActive ( isAField A )

  bFieldsBs <- RB.execute eFieldList >>=
    RB.switchB (pure S.empty)

  -- This is the bit that needs most of the work... how does tree
  let
    bImg' = mconcat <$> sequenceA
      [ textInput . seqToList             <$> bName
      , eventLine                         <$> bNum
      , eventLine                         <$> bCtrlBS
      , eventLine                         <$> bActive
      , foldMap (textInput . Text.unpack) <$> bFieldsBs
      ]

  RB.reactimate $ updateImage vty  <$> bImg' <@ eTicked
  RB.reactimate $ quit vty tickTid <$  eQuit

main :: IO ()
main = do
  let mkES = uncurry ES <$> RB.newAddHandler
  vtty <- Vty.mkVty Vty.defaultConfig

  vtyES   <- mkES
  tick    <- mkES
  tickTid <- C.forkIO $ eTickLoop tick

  network <- RB.compile (networkDesc vtyES tick vtty tickTid)
  RB.actuate network

  _ <- eLoop vtty vtyES
  Vty.shutdown vtty
  putStrLn "annnnnd we're done"
