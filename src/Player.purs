module Klank.Player where

import Prelude
import Control.Promise (toAffE)
import Data.Traversable (sequence)
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, bandpass, decodeAudioDataFromUri, gain', loopBuf, runInBrowser, speaker')
import Foreign.Object as O
import Math (sin, pi)
import Type.Klank.Dev (Klank, Buffers, affable, klank)

scene :: Number -> Behavior (AudioUnit D1)
scene time = pure $ speaker' (gain' 1.0 (bandpass (1000.0 + 400.0 * sin (pi * time * 0.2)) (3.0 + 2.5 * sin (pi * time * 0.3)) $ loopBuf "scratch" 1.0 0.0 0.0))

buffers :: Buffers
buffers ctx _ =
  affable
    $ sequence
        ( O.singleton "scratch"
            $ toAffE (decodeAudioDataFromUri ctx "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/aLittleShyAndSadOfEyeButVeryWiseWasHe.ogg")
        )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers = buffers
    }
