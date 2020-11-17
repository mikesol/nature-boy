module Klank.Wah where

import Prelude
import Data.Int (toNumber)
import Data.List (List(..), (:), length, mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import Data.Vec ((+>), empty)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, gain_, highpass_, makePeriodicWave, pannerMono_, periodicOsc_, runInBrowser, speaker')
import Foreign.Object as O
import Math (pow, (%))
import Type.Klank.Dev (Klank, klank)

conv440 :: Number -> Number
conv440 i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

toNel :: forall s. Semiring s => List s -> NonEmpty List s
toNel Nil = zero :| Nil

toNel (h : t) = h :| t

skewedTriangle01 :: Number -> Number -> Number -> Number
skewedTriangle01 os len = lcmap (_ % len) go
  where
  go time
    | time < (len * os) = (time / (len * os))
    | otherwise = (len - time) / (len * (1.0 - os))

triangle01 :: Number -> Number -> Number
triangle01 = skewedTriangle01 0.5

wah :: String -> String -> Number -> Int -> List Number -> Maybe Number -> Number -> Number -> Number -> Number -> Number -> AudioUnit D2
wah tag pwave len nwahs pitches filt gnStart gnEnd panStart panEnd time = pannerMono_ (tag <> "WahPanner") (panStart + ((panEnd - panStart) * time / len)) ((maybe identity (highpass_ (tag <> "WahHP") 1.0) filt) (gain_ (tag <> "WahGain") (if time >= len then 0.0 else ((gnStart + (gnEnd - gnStart) * time / len) * (triangle01 (len / (toNumber nwahs)) time) / (toNumber $ length pitches))) (toNel (mapWithIndex (\i p -> periodicOsc_ (tag <> show i) pwave (conv440 p)) pitches))))

scene :: Number -> Behavior (AudioUnit D2)
scene time = pure $ speaker' (wah "test" "smooth" 0.4 3 (60.0 : 64.0 : 67.0 : Nil) Nothing 0.2 0.9 0.5 0.5 time)

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , periodicWaves =
      \ctx _ res rej -> do
        smooth <-
          makePeriodicWave ctx
            (0.5 +> 0.25 +> -0.1 +> 0.07 +> 0.1 +> empty)
            (0.2 +> 0.1 +> 0.01 +> -0.03 +> -0.1 +> empty)
        rich <-
          makePeriodicWave ctx
            (0.1 +> 0.3 +> -0.1 +> 0.1 +> 0.2 +> 0.05 +> 0.1 +> 0.01 +> empty)
            (0.3 +> -0.5 +> -0.4 +> -0.03 +> -0.15 +> -0.2 +> -0.05 +> -0.02 +> empty)
        res $ O.fromFoldable [ Tuple "smooth" smooth, Tuple "rich" rich ]
    }
