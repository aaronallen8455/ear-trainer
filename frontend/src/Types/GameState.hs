{-# LANGUAGE PatternGuards #-}
module Types.GameState
  ( RoundState(..)
  , GameState(..)
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
    { gsLevel            :: !Level
    , gsNumCorrect       :: !Word
    , gsNumWrong         :: !Word
    , gsLastPair         :: !(Maybe PitchPair)
    , gsPairStream       :: [PitchPair]
    , gsRoundState       :: !RoundState
    , gsLevelScore       :: !Word
    , gsLastGuessCorrect :: !Bool
    } deriving (Show, Eq)

initGameState :: StdGen -> GameState
initGameState gen =
  GameState
    { gsLevel            = 1
    , gsNumCorrect       = 0
    , gsNumWrong         = 0
    , gsLastPair         = Nothing
    , gsPairStream       = randoms gen
    , gsRoundState       = GuessedLower
    , gsLevelScore       = 0
    , gsLastGuessCorrect = False
    }

correctGuess :: GameState -> GameState
correctGuess gs =
  case gsRoundState gs of
    RoundStart ->
      gs { gsRoundState       = GuessedLower
         , gsNumCorrect       = gsNumCorrect gs + 1
         , gsLevelScore       = gsLevelScore gs + 1
         , gsLastGuessCorrect = True
         }
    GuessedLower ->
      gs { gsLastPair         = Just . head $ gsPairStream gs
         , gsPairStream       = tail $ gsPairStream gs
         , gsNumCorrect       = gsNumCorrect gs + 1
         , gsLevelScore       = gsLevelScore gs + 1
         , gsRoundState       = RoundStart
         , gsLastGuessCorrect = True
         }

incorrectGuess :: GameState -> GameState
incorrectGuess gs =
  gs { gsNumWrong         = gsNumWrong gs + 1
     , gsLevelScore       = max 0 $ gsLevelScore gs - 1
     , gsLastGuessCorrect = False
     }

checkGuess :: Note -> GameState -> Bool
checkGuess guess gs
  | PitchPair l u : _ <- gsPairStream gs
  = case gsRoundState gs of
      RoundStart   -> pitchNote l == guess
      GuessedLower -> pitchNote u == guess
  | otherwise = False

updateGameState :: Note -> GameState -> GameState
updateGameState guess gs =
  if checkGuess guess gs
     then correctGuess gs
     else incorrectGuess gs

getCurrentPair :: GameState -> PitchPair
getCurrentPair = head . gsPairStream
