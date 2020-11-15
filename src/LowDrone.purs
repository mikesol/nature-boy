module Klank.Cello where

import Prelude
import Control.Promise (toAffE)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, decodeAudioDataFromUri, evalPiecewise, gainT_', gain_, gain_', loopBuf_, lowpass_, pannerMono_, panner_, playBuf_, runInBrowser, speaker')
import Foreign.Object as O
import Math (cos, pi, sin, (%))
import Type.Klank.Dev (Buffers, Klank, affable, defaultEngineInfo, klank, makeBuffersKeepingCache)

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

wobbleRate :: Number -> Number
wobbleRate time
  | time < 1.0 = 8.0
  | time < 2.0 = 5.0
  | time < 3.0 = 11.0
  | time < 4.0 = 6.0
  | time < 5.0 = 3.0
  | otherwise = 0.2

bassDroneVol :: Number -> Number -> Number
bassDroneVol len = lcmap (_ % len) go
  where
  go time
    | time < (len / 2.0) = (time * 2.0 / len)
    | otherwise = (len - time) * 2.0 / len

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker'
        ( gain_ "C#DroneMasterFader" 1.0
            ( ( gainT_' "C#CelloLoopGain"
                  (epwf [ Tuple 0.0 0.0, Tuple 0.15 1.0, Tuple 10.0 1.0 ] time)
                  ( lowpass_
                      "C#CelloLoopLowpass"
                      (175.0 + (-100.0 * (cos ((wobbleRate time) * rad)))) -- 75.0 orig
                      10.0
                      (loopBuf_ "C#CelloLoop" "low-c#-cello-drone" 1.0 0.5 2.5)
                  )
              )
                :| (gain_' "C#BassGain" (1.0 * (bassDroneVol 4.0 time)) (loopBuf_ "C#BassLoop" "bass-c-sharp" 0.5 0.5 2.5))
                : Nil
            )
        )
  where
  rad = pi * time

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers =
      makeBuffersKeepingCache
        [ Tuple "low-c#-cello-drone" "https://freesound.org/data/previews/195/195278_3623377-hq.mp3"
        --, Tuple "handbell-c#" "https://freesound.org/data/previews/339/339808_5121236-hq.mp3"
        --, Tuple "guitar-8th-c#" "https://freesound.org/data/previews/372/372386_5968459-hq.mp3"
        --, Tuple "accordion-c#-aug" "https://freesound.org/data/previews/120/120692_649468-hq.mp3"
        --, Tuple "spoooooky-amb" "https://freesound.org/data/previews/277/277572_5338846-hq.mp3"
        -- rate 1.09 gets to C#
        --, Tuple "scary-c" "https://freesound.org/data/previews/277/277637_5338846-hq.mp3"
        --, Tuple "gong-c-sharp" "https://media.graphcms.com/LFgrdeImQICudFzE1ShR"
        --, Tuple "power-chord-c-sharp" "https://freesound.org/data/previews/49/49275_177850-hq.mp3"
        --, Tuple "bassoon-c-sharp" "https://freesound.org/data/previews/154/154330_2626346-hq.mp3"
        --, Tuple "loud-awful-scream" "https://freesound.org/data/previews/267/267395_5004228-hq.mp3"
        --, Tuple "real-human-scream" "https://freesound.org/data/previews/536/536486_11937282-hq.mp3"
        --, Tuple "flute-c-sharp" "https://freesound.org/data/previews/154/154208_2626346-hq.mp3"
        --, Tuple "pipe-c-sharp" "https://freesound.org/data/previews/345/345192_5622625-hq.mp3"
        --, Tuple "guitar-c-sharp" "https://freesound.org/data/previews/153/153957_2626346-hq.mp3"
        , Tuple "bass-c-sharp" "https://media.graphcms.com/iUoLXZ3S5e8uSq647NEd"
        --, Tuple "pizz-c-sharp" "https://freesound.org/data/previews/153/153642_2626346-hq.mp3"
        --, Tuple "pizz-e" "https://freesound.org/data/previews/153/153633_2626346-hq.mp3"
        --, Tuple "pizz-g-sharp" "https://freesound.org/data/previews/153/153637_2626346-hq.mp3"
        --, Tuple "bass-pizz-c-sharp" "https://freesound.org/data/previews/153/153805_2626346-lq.mp3"
        --, Tuple "guitar-high-c-sharp" "https://freesound.org/data/previews/153/153944_2626346-lq.mp3"
        --, Tuple "voice-like-c-sharp" "https://freesound.org/data/previews/315/315850_4557960-lq.mp3"
        --, Tuple "terrifying-air-raid-siren" "https://freesound.org/data/previews/271/271132_5004228-lq.mp3"
        --, Tuple "shruti-box" "https://media.graphcms.com/qwlr3QDKQHmrLjD9smOY"
        ]
    }
