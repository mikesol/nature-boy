module Klank.Player where

import Prelude
import Control.Promise (toAffE)
import Data.Traversable (sequence)
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, decodeAudioDataFromUri, gain', loopBuf, runInBrowser, speaker')
import Foreign.Object as O
import Type.Klank.Dev (Klank, Buffers, affable, klank)

scene :: Number -> Behavior (AudioUnit D1)
scene time = pure $ speaker' (gain' 1.0 (loopBuf "scratch" 0.7 0.1 0.3))

buffers :: Buffers
buffers ctx _ =
  affable
    $ sequence
        ( O.singleton "scratch"
            $ toAffE (decodeAudioDataFromUri ctx "https://freesound.org/data/previews/119/119059_181941-hq.mp3")
        )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers = buffers
    }
