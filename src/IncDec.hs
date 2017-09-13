{-# LANGUAGE RecursiveDo #-}
module IncDec where

import           Control.Monad.Fix (MonadFix)

import qualified Graphics.Vty      as Vty

import           Reactive.Banana   ((<@))
import qualified Reactive.Banana   as RB

import           Common            (KeyInput (..), redText)

bIncDecNum
  :: ( MonadFix m
     , RB.MonadMoment m
     )
  => KeyInput
  -> m (RB.Behavior Int)
bIncDecNum eKey = mdo
  let
    ePlus  = keyInEvt eKey (Vty.KChar '+')
    eMinus = keyInEvt eKey (Vty.KChar '-')

  bNum <- RB.accumB (0 :: Int) $ RB.unions
    [ (+ 1)        <$ ePlus
    , (subtract 1) <$ RB.filterE (> 0) ( bNum <@ eMinus )
    ]

  pure bNum

bIncDec
  :: ( MonadFix m
     , RB.MonadMoment m
     )
  => KeyInput
  -> m (RB.Behavior Vty.Image)
bIncDec eKey = mdo
  let
    ePlus  = keyInEvt eKey (Vty.KChar '+')
    eMinus = keyInEvt eKey (Vty.KChar '-')

    mkImg :: Int -> Vty.Image
    mkImg = redText . show

  bNum <- RB.accumB (0 :: Int) $ RB.unions
    [ (+ 1)        <$ ePlus
    , (subtract 1) <$ RB.filterE (> 0) ( bNum <@ eMinus )
    ]

  pure $ mkImg <$> bNum
