module Types.Pitch
  ( Note(..)
  , noteText
  , Pitch(..)
  , PitchPair(..)
  , pitchFreq
  ) where

import qualified Data.Text as T
import           System.Random

data Note
  = C | Db | D | Eb | E | F
  | Gb | G | Ab | A | Bb | B
  deriving (Show, Eq, Ord, Enum, Bounded)

noteText :: Note -> T.Text
noteText = \case
  C  -> "C"
  Db -> "D♭"
  D  -> "D"
  Eb -> "E♭"
  E  -> "E"
  F  -> "F"
  Gb -> "G♭"
  G  -> "G"
  Ab -> "A♭"
  A  -> "A"
  Bb -> "B♭"
  B  -> "B"

type Octave = Word

data Pitch =
  Pitch
    { pitchNote   :: Note
    , pitchOctave :: Octave
    } deriving (Show, Eq)

upperBound :: Pitch
upperBound = Pitch C 6

lowerBound :: Pitch
lowerBound = Pitch A 2

instance Random Pitch where
  randomR (Pitch n1 o1, Pitch n2 o2) g = (p, g')
    where
      (x, g') = randomR ( fromEnum n1 + 12 * fromIntegral o1
                        , fromEnum n2 + 12 * fromIntegral o2 )
                        g
      (d, m) = divMod x 12
      p = Pitch (toEnum m) (fromIntegral d)

  random = randomR (lowerBound, upperBound)

type Frequency = Float

pitchFreq :: Pitch -> Frequency
pitchFreq (Pitch note octave) =
  440 * 2 ** (noteIndex / 12 + fromIntegral octave - 5)
    where
      noteIndex = fromIntegral $ fromEnum note + 3 `mod` 12

data PitchPair =
  PitchPair
    { lowerPitch :: !Pitch
    , upperPitch :: !Pitch
    } deriving (Show, Eq)

instance Random PitchPair where
  randomR (PitchPair l1 _, PitchPair _ u2) g = (PitchPair rl ru, g'')
    where
      (rl, g')  = randomR (l1, u2) g
      (ru, g'') = randomR (rl, u2) g'

  random g = (PitchPair l u, g'') where
    (l, g')  = random g
    (u, g'') = randomR (l, upperBound) g'

