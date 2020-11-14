module Klank.Cello where

import Prelude
import Control.Promise (toAffE)
import Data.Int (toNumber)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, decodeAudioDataFromUri, evalPiecewise, gainT_', lowpass_, playBuf_, runInBrowser, speaker')
import Foreign.Object as O
import Math (pi)
import Type.Klank.Dev (Buffers, Klank, affable, defaultEngineInfo, klank)

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  let
    rad = pi * time
  in
    pure
      $ speaker'
          ( gainT_' "CelloPlayer"
              (epwf [ Tuple 0.0 0.0, Tuple 0.5 1.0, Tuple 2.5 1.0, Tuple 3.0 0.0 ] time)
              ( lowpass_
                  "CelloPlayer"
                  (75.0)
                  10.0
                  (playBuf_ "CelloPlayer" "scratch" 1.0)
              )
          )

buffers :: Buffers
buffers ctx _ =
  affable
    $ sequence
        ( O.singleton "scratch"
            $ toAffE (decodeAudioDataFromUri ctx "https://freesound.org/data/previews/195/195285_3623377-hq.mp3")
        )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers = buffers
    }
