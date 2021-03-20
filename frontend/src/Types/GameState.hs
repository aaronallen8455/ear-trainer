{-# LANGUAGE PatternGuards #-}
module Types.GameState
  ( RoundState(..)
  , GameState(..)
  , GameStateEvent(..)
  , initGameState
  , correctGuess
  , incorrectGuess
  , updateGameState
  , getCurrentPair
  ) where

import           System.Random

import           Types.Pitch

type Level = Word

data RoundState
  = RoundStart
  | GuessedLower
  deriving (Show, Eq)

data GameState =
  GameState
    { gsLevel        :: !Level
    , gsNumCorrect   :: !Word
    , gsNumWrong     :: !Word
    , gsLastPair     :: !(Maybe PitchPair)
    , gsPairStream   :: [PitchPair]
    , gsRoundState   :: !RoundState
    , gsLevelScore   :: !Word
    , gsStartOfRound :: !Bool
    , gsAudioInited  :: !Bool
    } deriving (Show)

initGameState :: StdGen -> GameState
initGameState gen =
  GameState
    { gsLevel        = 1
    , gsNumCorrect   = 0
    , gsNumWrong     = 0
    , gsLastPair     = Nothing
    , gsPairStream   = filter (not . dupePair) $ randoms gen
    , gsRoundState   = GuessedLower
    , gsLevelScore   = 0
    , gsStartOfRound = False
    , gsAudioInited  = False
    }

data GameStateEvent
  = Guess Note
  | InitedAudio

correctGuess :: GameState -> GameState
correctGuess gs =
  case gsRoundState gs of
    RoundStart ->
      gs { gsRoundState   = GuessedLower
         , gsNumCorrect   = gsNumCorrect gs + 1
         , gsLevelScore   = gsLevelScore gs + 1
         , gsStartOfRound = False
         }
    GuessedLower ->
      gs { gsLastPair     = Just . head $ gsPairStream gs
         , gsPairStream   = tail $ gsPairStream gs
         , gsNumCorrect   = gsNumCorrect gs + 1
         , gsLevelScore   = gsLevelScore gs + 1
         , gsRoundState   = RoundStart
         , gsStartOfRound = True
         }

incorrectGuess :: GameState -> GameState
incorrectGuess gs =
  gs { gsNumWrong     = gsNumWrong gs + 1
     , gsLevelScore   = max 0 $ gsLevelScore gs - 1
     , gsStartOfRound = False
     }

checkGuess :: Note -> GameState -> Bool
checkGuess guess gs
  | PitchPair l u : _ <- gsPairStream gs
  = case gsRoundState gs of
      RoundStart   -> pitchNote l == guess
      GuessedLower -> pitchNote u == guess
  | otherwise = False

updateGameState :: GameStateEvent -> GameState -> GameState
updateGameState (Guess guess) gs =
  if checkGuess guess gs
     then correctGuess gs
     else incorrectGuess gs
updateGameState InitedAudio gs =
  gs { gsAudioInited = True }

getCurrentPair :: GameState -> PitchPair
getCurrentPair = head . gsPairStream
