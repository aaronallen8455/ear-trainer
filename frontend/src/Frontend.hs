{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Frontend where

import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (liftIO)
import           GHC.Float
import           Language.Javascript.JSaddle (JSM, liftJSM)
import           Language.Javascript.JSaddle.Types (MonadJSM)
import qualified System.Random as Rand

import qualified GHCJS.DOM.AudioContext as Ctx
import qualified GHCJS.DOM.AudioNode as Node
import qualified GHCJS.DOM.AudioParam as Param
import qualified GHCJS.DOM.GainNode as Gain
import qualified GHCJS.DOM.OscillatorNode as Osc

import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route

import           Reflex.Dom.Core

import           Common.Route
import           Types.GameState
import           Types.Pitch

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      prerender_ blank $ do
        startClick <- button "Begin"
        _ <- widgetHold blank (ui <$ startClick)
        pure ()
  }

ui :: (MonadJSM m, DomBuilder t m, MonadHold t m, MonadFix m)
   => m ()
ui = mdo
  stdGen <- liftIO Rand.newStdGen

  audioCtx <- liftJSM Ctx.newAudioContext

  osc1 <- liftJSM $ Ctx.createOscillator audioCtx
  gain1 <- Ctx.createGain audioCtx
  flip Param.setValue 0 =<< Gain.getGain gain1
  osc2 <- liftJSM $ Ctx.createOscillator audioCtx
  gain2 <- Ctx.createGain audioCtx
  flip Param.setValue 0 =<< Gain.getGain gain2
  dest <- liftJSM $ Ctx.getDestination audioCtx
  Node.connect osc1 gain1 Nothing Nothing
  Node.connect gain1 dest Nothing Nothing
  Node.connect osc2 gain2 Nothing Nothing
  Node.connect gain2 dest Nothing Nothing
  Osc.start osc1 Nothing
  Osc.start osc2 Nothing


  let initState = initGameState stdGen

  gameStateDyn <- foldDyn updateGameState initState noteEv

  noteEv <- noteButtons

  playCurClick <- button "Play Current"
  _ <- widgetHold blank
     $ liftJSM . playCurPair osc1 gain1 osc2 gain2 audioCtx
     <$> current gameStateDyn <@ playCurClick

  -- buttonClick <- button "click me"
  -- _ <- widgetHold blank (liftJSM audioTest <$ buttonClick)
  pure ()

playCurPair :: Osc.OscillatorNode
            -> Gain.GainNode
            -> Osc.OscillatorNode
            -> Gain.GainNode
            -> Ctx.AudioContext
            -> GameState
            -> JSM ()
playCurPair osc1 gain1 osc2 gain2 audioCtx gs = do
  let PitchPair l u = getCurrentPair gs
  playNote l osc1 gain1 audioCtx
  playNote u osc2 gain2 audioCtx

playPrevPair :: Osc.OscillatorNode
             -> Gain.GainNode
             -> Osc.OscillatorNode
             -> Gain.GainNode
             -> Ctx.AudioContext
             -> GameState
             -> JSM ()
playPrevPair osc1 gain1 osc2 gain2 audioCtx gs =
  case gsLastPair gs of
    Nothing -> pure ()
    Just (PitchPair l u) -> do
      playNote l osc1 gain1 audioCtx
      playNote u osc2 gain2 audioCtx

playNote :: Pitch -> Osc.OscillatorNode -> Gain.GainNode -> Ctx.AudioContext -> JSM ()
playNote pitch osc gainNode audioCtx = do
  curTime <- double2Float <$> Ctx.getCurrentTime audioCtx
  gain <- Gain.getGain gainNode
  freq <- Osc.getFrequency osc

  Param.cancelScheduledValues gain 0
  Param.linearRampToValueAtTime gain 0.3 (curTime + 0.1)
  Param.setValue freq (pitchFreq pitch)
  Param.setTargetAtTime gain 0 (curTime + 1) 0.5

noteButtons :: DomBuilder t m => m (Event t Note)
noteButtons =
  let notes = [minBound ..]
      buttons = traverse (button . noteText) notes
      events = zipWith (<$) notes <$> buttons
   in leftmost <$> events
