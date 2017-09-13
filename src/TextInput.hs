{-# LANGUAGE FlexibleContexts      #-}
module TextInput (bTextInput', bTextInput) where

import           Control.Lens      (Snoc, snoc, unsnoc)
import           Control.Monad.Fix (MonadFix)

import qualified Reactive.Banana   as RB

import qualified Graphics.Vty      as Vty

import           Common            (KeyInput, keyInAllEvts, keyInEvt,
                                    keyInModEvt, leftMost)

okChars
  :: [Char]
okChars =
  mconcat
  [ ['a' .. 'z']
  , ['A' .. 'Z']
  , ['0' .. '9']
  , [' ', ',', '.']
  ]

isAlphaNum
  :: Vty.Event
  -> Maybe Char
isAlphaNum (Vty.EvKey (Vty.KChar k) m)
  | k `elem` okChars && (null m || m == [Vty.MShift]) = Just k
isAlphaNum _  = Nothing

eAlphaNumChar
  :: KeyInput
  -> RB.Event Char
eAlphaNumChar eKeyIn =
  RB.filterJust (isAlphaNum <$> keyInAllEvts eKeyIn)

bTextInput'
  :: ( RB.MonadMoment m
     , MonadFix m
     , Monoid t
     , Snoc t t Char Char
     )
  => KeyInput
  -> RB.Behavior a
  -> ( a -> Bool )
  -> t
  -> m (RB.Behavior t)
bTextInput' eKeyIn bAcceptingInput tagFn startV = do
  let
    eBackSpace = keyInEvt eKeyIn Vty.KBS
    eKillAll   = keyInModEvt eKeyIn (Vty.KChar 'k') [Vty.MCtrl]

  RB.accumB startV . RB.whenE ( tagFn <$> bAcceptingInput ) $ leftMost
    [ maybe mempty fst . unsnoc       <$  eBackSpace
    , const mempty                    <$  eKillAll
    , flip snoc                       <$> eAlphaNumChar eKeyIn
    ]

bTextInput
  :: ( RB.MonadMoment m
     , MonadFix m
     , Monoid t
     , Snoc t t Char Char
     )
  => KeyInput
  -> RB.Behavior a
  -> ( a -> Bool )
  -> m (RB.Behavior t)
bTextInput eKeyIn bAcceptingInput tagFn =
  bTextInput' eKeyIn bAcceptingInput tagFn mempty
