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
    pure $ speaker' (gain' 0.2 (loopBuf "scratch" (2.0 + sin rad) 0.0 0.0))

buffers :: Buffers
buffers ctx _ =
  affable
    $ sequence
        ( O.singleton "scratch"
            $ toAffE (decodeAudioDataFromUri ctx "https://media.graphcms.com/Ts3c1reQQr2Cs5D2e4A0")
        )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers = buffers
    }
