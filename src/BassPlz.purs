module Klank.BassPlz where

import Prelude
import Data.Int (toNumber)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, evalPiecewise, g'add_, g'delay_, g'gain_, gainT_', gain_', graph_, lowpass_, playBufWithOffset_, runInBrowser, speaker')
import Math (pow)
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))
import Type.Klank.Dev (Klank, defaultEngineInfo, makeBuffersKeepingCache, klank)

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

conv1 :: Number -> Number
conv1 i = 1.0 * (2.0 `pow` ((i - 1.0) / 12.0))

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  pure
    $ speaker'
        ( gain_' (tag <> "bassPlzFader") globalGain
            $ graph_ (tag <> "bassPlzGraph")
                { aggregators:
                    { out: Tuple (g'add_ (tag <> "bassPlzOut")) (SLProxy :: SLProxy ("combine" :/ SNil))
                    , combine: Tuple (g'add_ (tag <> "bassPlzCombine")) (SLProxy :: SLProxy ("gain" :/ "bass" :/ SNil))
                    , gain: Tuple (g'gain_ (tag <> "bassPlzGain") gn) (SLProxy :: SLProxy ("del" :/ SNil))
                    }
                , processors:
                    { del: Tuple (g'delay_ (tag <> "bassPlzDelay") del) (SProxy :: SProxy "combine")
                    }
                , generators:
                    { bass:
                        (gainT_' (tag <> "bassPlzImpulse") (if backwards then (epwf [ Tuple 0.0 0.0, Tuple (1.0 - kink) 0.1, Tuple 1.0 1.0, Tuple 1.06 0.0 ] time) else (epwf [ Tuple 0.0 1.0, Tuple kink 0.1, Tuple 1.0 0.0 ] time)) $ filt (playBufWithOffset_ (tag <> "bassPlzBuf") "bassplz" (rate time) os))
                    }
                }
        )
  where
  globalGain = 1.0

  tag = "x"

  del = 0.23

  gn = 0.7

  os = 0.0

  filt = (lowpass_ "lpf" 50.0 1.0)

  kink = 0.3

  backwards = false

  --rate = const $ conv1 (-5.0)
  rate = \t ->
    let
      st = conv1 (-6.2)

      ed = conv1 (-5.0)
    in
      min ed (st + (ed - st) * t * 6.0)

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers = makeBuffersKeepingCache [ Tuple "bassplz" "https://freesound.org/data/previews/119/119059_181941-hq.mp3" ]
    }
