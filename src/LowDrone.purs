module Klank.LowDrone where

import Prelude
import Data.Foldable (fold, foldl)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (lcmap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class Pos, D1, D2)
import Data.Vec ((+>), empty)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioParameter, AudioUnit, BrowserAudioBuffer, decodeAudioDataFromUri, evalPiecewise, g'add_, g'delay_, g'gain_, g'highpass_, gainT_', gain_, gain_', graph_, highpass_, loopBuf_, lowpass_, makePeriodicWave, pannerMono_, panner_, periodicOsc_, playBufWithOffsetT_, playBufWithOffset_, playBuf_, runInBrowser, speaker')
import Foreign.Object as O
import Math (cos, pi, pow, sin, (%))
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))
import Type.Klank.Dev (Klank, makeBuffersKeepingCache, defaultEngineInfo, klank)

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

loopT :: forall a. Number -> (Number -> a) -> (Number -> a)
loopT t = lcmap (_ % t)

boundPlayer :: forall a. Number -> (Number -> List a) -> Number -> List a
boundPlayer len a time = if (time) + kr >= 0.0 && time < (len) then a time else Nil

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

conv440 :: Number -> Number
conv440 i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

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

--------------------------------------------
---------
wobbleRate :: Number -> Number
wobbleRate time
  | time < 1.0 = 8.0
  | time < 2.0 = 5.0
  | time < 3.0 = 11.0
  | time < 4.0 = 6.0
  | time < 5.0 = 3.0
  ---
  | time < 5.6 = 9.0
  | time < 6.1 = 5.0
  | time < 7.0 = 3.0
  | time < 8.0 = 2.0
  | time < 9.0 = 1.0
  ---
  | otherwise = 0.2

bassDroneVol :: Number -> Number
bassDroneVol time
  | time < 2.7 = triangle01 4.0 (time - 0.0)
  | time < 4.6 = triangle01 0.4 (time - 2.7)
  | time < 5.6 = triangle01 1.0 (time - 4.6)
  | otherwise = triangle01 4.0 (time - 5.6)

gongBackwards2Atomic :: String -> Number -> Number -> List (AudioUnit D1)
gongBackwards2Atomic tag len =
  boundPlayer (len + 0.1)
    ( \t ->
        pure
          $ ( gainT_' ("GongBwAboveC#Gain" <> tag)
                (epwf [ Tuple 0.0 0.0, Tuple 0.1 1.0, Tuple (len - 0.2) 1.0, Tuple (len - 0.1) 0.05, Tuple len 0.0 ] t)
                ( playBufWithOffset_
                    ("GongBwAboveC#Buf" <> tag)
                    "gong-g-sharp-reversed"
                    1.0 --(min 1.0 (0.95 + (((t % 0.4) / 0.4) * 0.1)))
                    (6.7 - len)
                )
            )
    )

birds :: Number -> List (AudioUnit D2)
birds =
  boundPlayer (20.0)
    ( \t ->
        pure
          $ ( gain_' ("BirdsAboveC#Gain")
                (0.3 * (skewedTriangle01 0.5 10.0 t))
                ( playBuf_
                    ("BirdsAboveC#Buf")
                    "beautiful-birds"
                    1.0
                )
            )
    )

guitarSingleton :: String -> String -> Number -> Number -> List (AudioUnit D2)
guitarSingleton tag name gain =
  boundPlayer (20.0)
    ( \t ->
        pure
          $ ( gain_' ("GuitarGain" <> tag <> name)
                (gain)
                ( playBuf_
                    ("GuitarBuf" <> tag <> name)
                    name
                    1.0
                )
            )
    )

shriek :: Number -> List (AudioUnit D2)
shriek =
  boundPlayer (20.0)
    ( \t ->
        pure
          $ ( gain_' ("AirRaidSirenAboveC#Gain")
                (0.3)
                ( graph_
                    "AirRaidGrpah"
                    { aggregators:
                        { out: Tuple (g'add_ "AirRaidOut") (SLProxy :: SLProxy ("combine" :/ SNil))
                        , combine: Tuple (g'add_ "AirRaidCombine") (SLProxy :: SLProxy ("gain" :/ "chimez" :/ SNil))
                        , gain: Tuple (g'gain_ "AirRaidGraphGain" 0.5) (SLProxy :: SLProxy ("del" :/ SNil))
                        }
                    , processors:
                        { del: Tuple (g'delay_ "AirRaidGraphDelay" 0.4) (SProxy :: SProxy "combine")
                        }
                    , generators:
                        { chimez:
                            ( if t < 4.0 then
                                ( gainT_' "AirRaidSirenCarveGian"
                                    ( epwf
                                        [ Tuple 0.0 1.0
                                        , Tuple 0.2 1.0
                                        , Tuple 0.3 0.2
                                        , Tuple 0.5 0.7
                                        , Tuple 1.4 0.7
                                        , Tuple 1.5 0.1
                                        , Tuple 1.6 0.7
                                        , Tuple 1.7 0.1
                                        , Tuple 1.8 0.7
                                        , Tuple 1.9 0.1
                                        , Tuple 2.0 0.7
                                        , Tuple 2.1 0.1
                                        , Tuple 2.2 0.7
                                        , Tuple 2.3 0.1
                                        , Tuple 2.4 0.6
                                        , Tuple 2.5 0.1
                                        , Tuple 2.6 0.5
                                        , Tuple 2.7 0.1
                                        , Tuple 2.8 0.4
                                        , Tuple 2.9 0.1
                                        , Tuple 3.0 0.3
                                        , Tuple 3.1 0.1
                                        , Tuple 3.2 0.2
                                        , Tuple 3.3 0.1
                                        ]
                                        t
                                    )
                                    $ playBuf_
                                        ("AirRaidSirenAboveC#Buf")
                                        "terrifying-air-raid-siren"
                                        1.0
                                )
                              else
                                zero
                            )
                        }
                    }
                )
            )
    )

gongBackwards2 :: Array (Number -> List (AudioUnit D1))
gongBackwards2 = (foldl (\{ acc, t } a -> { acc: [ atT t $ gongBackwards2Atomic (show t) a ] <> acc, t: t + a }) { acc: [], t: 0.0 } [ 0.7, 0.7, 0.7, 0.3, 0.3, 0.3, 1.0, 1.0, 0.7, 0.4, 0.4, 0.4, 1.4, 0.5, 0.5, 0.7, 0.7, 1.0, 1.0 ]).acc

gongBackwards :: Number -> List (AudioUnit D2)
gongBackwards =
  boundPlayer 10.0
    ( \t ->
        pure
          $ pannerMono_ "GongAboveC#Pan" (2.0 * (skewedTriangle01 0.8 2.0 t) - 1.0)
              ( gain_ "GongBwAboveC#Gain"
                  (4.0 * (skewedTriangle01 0.2 10.0 t))
                  (toNel $ fold (map (\f -> f t) gongBackwards2))
              )
    )

chimez :: Number -> List (AudioUnit D2)
chimez =
  boundPlayer 10.0
    ( \t ->
        pure
          $ graph_
              "ChimezGrpah"
              { aggregators:
                  { out: Tuple (g'add_ "ChimezOut") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "ChimezCombine") (SLProxy :: SLProxy ("gain" :/ "chimez" :/ SNil))
                  , gain: Tuple (g'gain_ "ChimezGraphGain" 0.17) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "ChimezGraphDelay" 0.21) (SProxy :: SProxy "hpf")
                  , hpf: Tuple (g'highpass_ "ChimezGraphHpf" 4000.0 14.0) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { chimez:
                      ( gain_' "ChimezAboveC#Gain"
                          (max 0.0 (0.5 - (t * 0.05)))
                          ( highpass_ "ChimezAboveC#HP" 3000.0 5.0
                              ( loopBuf_
                                  "ChimezAboveC#Buf"
                                  "chimez-above-c-sharp-drone"
                                  1.0
                                  0.0
                                  0.0
                              )
                          )
                      )
                  }
              }
    )

synth :: Number -> List (AudioUnit D2)
synth =
  boundPlayer 20.0 \time ->
    let
      rad = pi * time
    in
      pure
        ( gain_ "SynthGain" (if time < 2.0 then time * 0.5 else 1.0)
            ( ( pannerMono_ "TonalDotsPan" (sin rad)
                  (gain_' "TonalDotsGain" (0.1 * (triangle01 0.35 time)) (periodicOsc_ "TonalDotsOsc" "smooth" (conv440 (68.0))))
              )
                :| ( pannerMono_ "TonalDotsPan" (cos rad)
                      (gain_' "TonalDotsGain" (0.08 * (triangle01 0.35 time)) (periodicOsc_ "TonalDotsOsc" "smooth" (conv440 (66.0))))
                  )
                : Nil
            )
        )

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker'
        ( gain_ "C#DroneMasterFader" 1.0
            ( toNel
                $ ( ( gainT_' "C#CelloLoopGain"
                        (epwf [ Tuple 0.0 0.0, Tuple 0.15 0.8, Tuple 10.0 0.8 ] time)
                        ( lowpass_
                            "C#CelloLoopLowpass"
                            (175.0 + (-100.0 * (cos ((wobbleRate time) * rad)))) -- 75.0 orig
                            10.0
                            (loopBuf_ "C#CelloLoop" "low-c#-cello-drone" 1.0 0.5 2.5)
                        )
                    )
                      : ( pannerMono_
                            "C#BassPan"
                            (2.0 * (skewedTriangle01 (0.94 - (min 0.44 (time * 0.1))) 2.0 time) - 1.0)
                            (gain_' "C#BassGain" (1.0 * (bassDroneVol time)) (loopBuf_ "C#BassLoop" "bass-c-sharp" 0.5 0.0 4.3))
                        )
                      : Nil
                      <> ( fold
                            $ ( map (\f -> f time)
                                  ( [ atT 3.0 chimez
                                    , atT 2.0 gongBackwards
                                    , atT 4.0 birds
                                    , atT 3.0 shriek
                                    , atT 5.2 (guitarSingleton "a" "middle-g-sharp-guitar" 0.5)
                                    , atT 6.7 (guitarSingleton "b" "e-guitar" 0.3)
                                    , atT 9.0 synth
                                    ]
                                  )
                              )
                        )
                  )
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
        -- , Tuple "gong-g-sharp" "https://media.graphcms.com/LFgrdeImQICudFzE1ShR"
        , Tuple "gong-g-sharp-reversed" "https://media.graphcms.com/pYrQiqMAT62OoVYQugg4"
        --, Tuple "power-chord-c-sharp" "https://freesound.org/data/previews/49/49275_177850-hq.mp3"
        --, Tuple "bassoon-c-sharp" "https://freesound.org/data/previews/154/154330_2626346-hq.mp3"
        --, Tuple "loud-awful-scream" "https://freesound.org/data/previews/267/267395_5004228-hq.mp3"
        --, Tuple "real-human-scream" "https://freesound.org/data/previews/536/536486_11937282-hq.mp3"
        --, Tuple "flute-c-sharp" "https://freesound.org/data/previews/154/154208_2626346-hq.mp3"
        --, Tuple "pipe-c-sharp" "https://freesound.org/data/previews/345/345192_5622625-hq.mp3"
        -- , Tuple "guitar-c-sharp" "https://freesound.org/data/previews/153/153957_2626346-hq.mp3"
        , Tuple "bass-c-sharp" "https://media.graphcms.com/0gp37YI7Q5mczsjAUiUH"
        --, Tuple "pizz-c-sharp" "https://freesound.org/data/previews/153/153642_2626346-hq.mp3"
        --, Tuple "pizz-e" "https://freesound.org/data/previews/153/153633_2626346-hq.mp3"
        --, Tuple "pizz-g-sharp" "https://freesound.org/data/previews/153/153637_2626346-hq.mp3"
        --, Tuple "bass-pizz-c-sharp" "https://freesound.org/data/previews/153/153805_2626346-hq.mp3"
        --, Tuple "guitar-high-c-sharp" "https://freesound.org/data/previews/153/153944_2626346-hq.mp3"
        --, Tuple "voice-like-c-sharp" "https://freesound.org/data/previews/315/315850_4557960-hq.mp3"
        , Tuple "terrifying-air-raid-siren" "https://freesound.org/data/previews/271/271132_5004228-hq.mp3"
        --, Tuple "shruti-box" "https://media.graphcms.com/qwlr3QDKQHmrLjD9smOY"
        , Tuple "chimez-above-c-sharp-drone" "https://media.graphcms.com/3Z0DXRxRtOTymo51DGev"
        , Tuple "middle-g-sharp-guitar" "https://freesound.org/data/previews/154/154013_2626346-hq.mp3"
        , Tuple "high-g-sharp-guitar" "https://freesound.org/data/previews/153/153984_2626346-hq.mp3"
        , Tuple "e-guitar" "https://freesound.org/data/previews/153/153980_2626346-hq.mp3"
        , Tuple "beautiful-birds" "https://freesound.org/data/previews/528/528661_1576553-lq.mp3"
        ]
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
