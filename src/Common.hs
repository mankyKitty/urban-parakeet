module Common where

import qualified Data.Sequence   as S
import qualified Graphics.Vty    as Vty
import qualified Reactive.Banana as RB

data KeyInput = KeyInput
  { keyInAllEvts :: RB.Event Vty.Event
  , keyInModEvt    :: Vty.Key -> [Vty.Modifier] -> RB.Event Vty.Event
  , keyInEvt       :: Vty.Key -> RB.Event Vty.Event
  }

mkKeyInput
  :: RB.Event Vty.Event
  -> KeyInput
mkKeyInput eEvts =
  KeyInput eEvts
    (eKey eEvts)
    (\k -> eKey eEvts k [])

leftMost
  :: [RB.Event a]
  -> RB.Event a
leftMost =
  foldl (RB.unionWith const) RB.never

textBlocked
  :: Vty.Color
  -> String
  -> Vty.Image
textBlocked c =
  Vty.string (Vty.defAttr `Vty.withBackColor` c)

redText, greenText, yellowText :: String -> Vty.Image
redText    = textBlocked Vty.red
greenText  = textBlocked Vty.green
yellowText = textBlocked Vty.yellow

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
