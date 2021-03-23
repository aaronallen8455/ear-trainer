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
  , levelCompletePercent
  ) where

import           Data.List
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
    , gsLevelScore   :: !Int
    , gsGuessedWrong :: !Bool
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
    , gsPairStream   = removeRepeats . filter (not . dupePair) $ randoms gen
    , gsRoundState   = GuessedLower
    , gsLevelScore   = 0
    , gsGuessedWrong = False
    , gsStartOfRound = False
    , gsAudioInited  = False
    }

removeRepeats :: Eq a => [a] -> [a]
removeRepeats = concatMap (take 1) . group

data GameStateEvent
  = Guess Note
  | InitedAudio

correctGuess :: GameState -> GameState
correctGuess gs =
  case gsRoundState gs of
    RoundStart ->
      gs { gsRoundState   = GuessedLower
         , gsNumCorrect   = gsNumCorrect gs + 1
         , gsStartOfRound = False
         }
    GuessedLower
      | not $ gsGuessedWrong gs
      , gsLevelScore gs' == nxtLevelScore -- level up!
      -> gs' { gsLevelScore = 0
             , gsLevel      = gsLevel gs + 1
             }
      | otherwise -> gs'
      where
        nxtLevelScore = scoreForNextLevel (gsLevel gs)
        gs' = gs { gsLastPair     = Just . head $ gsPairStream gs
                 , gsPairStream   = tail $ gsPairStream gs
                 , gsNumCorrect   = gsNumCorrect gs + 1
                 , gsLevelScore   = if gsGuessedWrong gs
                                       then gsLevelScore gs
                                       else gsLevelScore gs + 1
                 , gsRoundState   = RoundStart
                 , gsStartOfRound = True
                 , gsGuessedWrong = False
                 }

incorrectGuess :: GameState -> GameState
incorrectGuess gs =
  gs { gsNumWrong     = gsNumWrong gs + 1
     , gsLevelScore   = if gsGuessedWrong gs
                           then gsLevelScore gs
                           else max 0 $ gsLevelScore gs
                                      - wrongGuessPenalty (gsLevel gs)
     , gsStartOfRound = False
     , gsGuessedWrong = True
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

scoreForNextLevel :: Level -> Int
scoreForNextLevel l = 4 + fromIntegral l

wrongGuessPenalty :: Level -> Int
wrongGuessPenalty l = ceiling $ fromIntegral l / (2 :: Double)

levelCompletePercent :: GameState -> Double
levelCompletePercent gs = fromIntegral (gsLevelScore gs)
                        / fromIntegral (scoreForNextLevel $ gsLevel gs)
                        * 100
