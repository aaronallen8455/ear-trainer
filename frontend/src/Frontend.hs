{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend where

import           Control.Monad
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe (isJust)
import           GHC.Float
import           Language.Javascript.JSaddle (JSM, liftJSM)
import           Language.Javascript.JSaddle.Types (MonadJSM)
import qualified System.Random as Rand

import qualified GHCJS.DOM.AudioContext as Ctx
import qualified GHCJS.DOM.AudioNode as Node
import qualified GHCJS.DOM.AudioParam as Param
import qualified GHCJS.DOM.GainNode as Gain
import qualified GHCJS.DOM.OscillatorNode as Osc
import qualified GHCJS.DOM.PannerNode as Pan

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
      prerender_ blank ui
  }

ui :: (MonadJSM m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, MonadIO (Performable m), TriggerEvent t m, PerformEvent t m)
   => m ()
ui = do
  stdGen <- liftIO Rand.newStdGen

  audioCtx <- liftJSM Ctx.newAudioContext

  -- build the audio graph
  osc1 <- liftJSM $ Ctx.createOscillator audioCtx
  gain1 <- Ctx.createGain audioCtx
  pan1 <- Ctx.createPanner audioCtx
  Pan.setPosition pan1 (-1) 0 1
  flip Param.setValue 0 =<< Gain.getGain gain1
  osc2 <- liftJSM $ Ctx.createOscillator audioCtx
  gain2 <- Ctx.createGain audioCtx
  pan2 <- Ctx.createPanner audioCtx
  Pan.setPosition pan2 1 0 1
  flip Param.setValue 0 =<< Gain.getGain gain2
  dest <- liftJSM $ Ctx.getDestination audioCtx
  Node.connect osc1 gain1 Nothing Nothing
  Node.connect gain1 pan1 Nothing Nothing
  Node.connect pan1 dest Nothing Nothing
  Node.connect osc2 gain2 Nothing Nothing
  Node.connect gain2 pan2 Nothing Nothing
  Node.connect pan2 dest Nothing Nothing

  -- UI elements
  divClass "main-wrapper" $ mdo

    let initState = initGameState stdGen

        gameStateEv = leftmost [ Guess <$> noteEv
                               , gate (not . gsAudioInited <$> current gameStateDyn)
                                      (InitedAudio <$ audioEv)
                               ]

    startOfRoundEv <- -- have to delay this so that the state has time to update
      delay 0.1 . void $ ffilter gsStartOfRound (updated gameStateDyn)

    gameStateDyn <- foldDyn updateGameState initState gameStateEv

    noteEv <- divClass "note-buttons" noteButtons

    audioEv <- divClass "dashboard-wrapper" $ do
      playCurEv <- button "Play Current"
      _ <- widgetHold blank
         $ liftJSM . playCurPair osc1 gain1 osc2 gain2 audioCtx
             <$> current gameStateDyn <@ leftmost [startOfRoundEv, playCurEv]

      playPrevEv <- playPrevButton gameStateDyn
      _ <- widgetHold blank
         $ liftJSM . playPrevPair osc1 gain1 osc2 gain2 audioCtx
             <$> current gameStateDyn <@ playPrevEv

      displayPrevNotes gameStateDyn
      displayCurNotes gameStateDyn

      pure $ leftmost [ playCurEv
                      , playPrevEv
                      ]
    pure ()

playPrevButton :: (DomBuilder t m, PostBuild t m) => Dynamic t GameState -> m (Event t ())
playPrevButton gsDyn = do
  let attrMap = do
        gs <- gsDyn
        pure $ if isJust (gsLastPair gs)
           then mempty
           else "disabled" =: "true"
  (ele, _) <- elDynAttr' "button" attrMap $ text "Play Previous"
  pure $ domEvent Click ele

displayPrevNotes :: (PostBuild t m, DomBuilder t m, MonadHold t m) => Dynamic t GameState -> m ()
displayPrevNotes gsDyn =
  divClass "dash-field" . widgetHold_ blank . updated $ do
    gs <- gsDyn
    pure $ case gsLastPair gs of
      Nothing -> blank
      Just p -> do
        elClass "span" "dash-label" $ text "Previous Notes"
        text $ noteText (pitchNote $ lowerPitch p)
            <> ", "
            <> noteText (pitchNote $ upperPitch p)

displayCurNotes :: (PostBuild t m, DomBuilder t m) => Dynamic t GameState -> m ()
displayCurNotes gsDyn = divClass "dash-field" $ do
  let txt = do
        gs <- gsDyn
        pure $ case gsRoundState gs of
          RoundStart -> "?, ?"
          GuessedLower ->
            (noteText . pitchNote . lowerPitch . getCurrentPair) gs
              <> ", ?"

  elClass "span" "dash-label" $ text "Current Notes"
  dynText txt

initAudio :: Osc.OscillatorNode -> Osc.OscillatorNode -> GameState -> JSM ()
initAudio osc1 osc2 gs =
  unless (gsAudioInited gs) $ do
    Osc.start osc1 Nothing
    Osc.start osc2 Nothing

playCurPair :: Osc.OscillatorNode
            -> Gain.GainNode
            -> Osc.OscillatorNode
            -> Gain.GainNode
            -> Ctx.AudioContext
            -> GameState
            -> JSM ()
playCurPair osc1 gain1 osc2 gain2 audioCtx gs = do
  initAudio osc1 osc2 gs
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
playPrevPair osc1 gain1 osc2 gain2 audioCtx gs = do
  initAudio osc1 osc2 gs
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
  Param.linearRampToValueAtTime freq (pitchFreq pitch) (curTime + 0.2)
  Param.setTargetAtTime gain 0 (curTime + 1) 0.5

noteButtons :: DomBuilder t m => m (Event t Note)
noteButtons =
  let notes = [minBound ..]
      buttons = traverse (button . noteText) notes
      events = zipWith (<$) notes <$> buttons
   in leftmost <$> events
