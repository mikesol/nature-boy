module Klank.Player where

import Prelude
import Control.Promise (toAffE)
import Data.Traversable (sequence)
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, decodeAudioDataFromUri, gain', loopBuf, runInBrowser, speaker')
import Foreign.Object as O
import Math (pi, sin)
import Type.Klank.Dev (Klank, Buffers, affable, klank)

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  let
    rad = pi * time
  in
    pure $ speaker' (gain' 0.2 (loopBuf "scratch" 1.0 0.0 0.0))

buffers :: Buffers
buffers ctx _ =
  affable
    $ sequence
        ( O.singleton "scratch"
            $ toAffE (decodeAudioDataFromUri ctx "https://freesound.org/data/previews/277/277637_5338846-lq.mp3")
        )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers = buffers
    }
