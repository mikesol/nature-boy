module Klank.DelaySketch where

import Prelude
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, delay, dup1, evalPiecewise, gain', gainT', microphone, runInBrowser, speaker)
import Math ((%))
import Type.Klank.Dev (Klank, defaultEngineInfo, klank)

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

myDel :: AudioUnit D1 -> Number -> Number -> Number -> AudioUnit D1
myDel m l g t =
  gainT'
    ( epwf
        [ Tuple 0.0 0.0
        , Tuple 0.06 0.0
        , Tuple 0.5 1.0
        , Tuple 0.94 0.0
        , Tuple 1.0 0.0
        ]
        (t % 1.0)
    )
    (delay ((l + 0.1) - l * (t % 1.0)) (gain' g m))
    + delay 0.5
        ( gainT'
            ( epwf
                [ Tuple 0.0 0.0
                , Tuple 0.06 0.0
                , Tuple 0.5 1.0
                , Tuple 0.94 0.0
                , Tuple 1.0 0.0
                ]
                (t % 1.0)
            )
            (delay ((l + 0.1) - l * (t % 1.0)) (gain' g m))
        )

scene :: Number -> Behavior (AudioUnit D1)
scene t =
  pure
    ( dup1 microphone \m ->
        ( speaker
            ( myDel m 0.3 0.5 t
                :| myDel m 0.4 0.3 t
                : myDel m 0.62 0.6 t
                : m
                : Nil
            )
        )
    )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , enableMicrophone = true
    }
