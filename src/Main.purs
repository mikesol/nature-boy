module Klank.NatureBoy where

import Prelude
import Color (Color, rgb)
import Control.Parallel (parallel, sequential)
import Control.Promise (toAffE)
import Data.Array (catMaybes, filter, head, index, length, mapWithIndex, range)
import Data.Either (either)
import Data.Foldable (fold, foldl, traverse_)
import Data.Int (floor, toNumber)
import Data.Lens (_2, over, traversed)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (lcmap)
import Data.String (Pattern(..), indexOf)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1, D2)
import Data.Vec ((+>), empty)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, try)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Audio (AV(..), AudioContext, AudioParameter, AudioUnit, BrowserAudioBuffer, CanvasInfo(..), Oversample(..), allpass_, audioWorkletProcessor_, bandpass_, convolver_, decodeAudioDataFromUri, defaultExporter, defaultParam, delay_, dup2, dup2_, evalPiecewise, g'add_, g'delay_, g'gain_, g'highpass_, gainT_, gainT_', gain_, gain_', graph_, highpassT_, highpass_, iirFilter_, loopBuf_, lowpass_, lowshelf_, makeFloatArray, makePeriodicWave, microphone_, mul_, notch_, pannerMono_, peaking_, periodicOsc_, playBufWithOffset_, playBuf_, runInBrowser_, sinOsc_, speaker, waveShaper_)
import FRP.Event (Event, makeEvent, subscribe)
import Foreign.Object as O
import Graphics.Canvas (Rectangle)
import Graphics.Drawing (Drawing, Point, fillColor, filled, rectangle, text)
import Graphics.Drawing.Font (FontOptions, bold, font, italic, sansSerif)
import Math (abs, cos, pi, pow, sin, (%))
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))
import Type.Klank.Dev (Klank', affable, defaultEngineInfo, klank)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Navigator (userAgent)
import Web.HTML.Window (navigator, toEventTarget)
import Web.TouchEvent.Touch as T
import Web.TouchEvent.TouchEvent (TouchEvent, changedTouches, fromEvent)
import Web.TouchEvent.TouchList as TL
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

-- There
-- Was   (was)    (was)    (was)
-- A     (aaaaaaaaaaaaa)
-- Boy (boy) (boy) (boy) (boy) (boy) (boy)
-- A very strange enchanted boy [[chords]]  (on ve-  start A drone) (chan = E) (boy = F#)
-- They (they they they)  (+ G# drone)
-- say he wandered
-- Ve (C#)
-- And then one day starts to have sawtooth delay
boundByCue_ :: forall acc a. Monoid a => Marker -> Marker -> (acc -> Marker -> Number -> Tuple acc a) -> acc -> Marker -> Number -> Tuple acc a
boundByCue_ st ed f ac m n = if m >= st && m <= ed then f ac m n else Tuple ac mempty

boundByCueWithOnset :: forall a. Monoid a => Marker -> Marker -> (NatureBoyAccumulator -> Number -> Marker -> Number -> a) -> NatureBoyAccumulator -> Marker -> Number -> Tuple NatureBoyAccumulator a
boundByCueWithOnset st ed f =
  boundByCue_ st ed
    ( \ac m t ->
        Tuple ac
          ( maybe
              mempty
              (\onset -> f ac onset m t)
              (M.lookup st ac.markerOnsets)
          )
    )

boundByCue :: forall acc a. Monoid a => Marker -> Marker -> (Marker -> Number -> a) -> acc -> Marker -> Number -> Tuple acc a
boundByCue st ed f = boundByCue_ st ed (\a m n -> Tuple a $ f m n)

boundByCue' :: forall acc a. Monoid a => Marker -> Marker -> (Number -> a) -> acc -> Marker -> Number -> Tuple acc a
boundByCue' st ed f = boundByCue_ st ed (\a m n -> Tuple a $ f n)

boundByCue'' :: forall acc a. Monoid a => Marker -> Marker -> a -> acc -> Marker -> Number -> Tuple acc a
boundByCue'' st ed f = boundByCue_ st ed (\a m n -> Tuple a f)

boundByCueNac_ :: forall a. Monoid a => Marker -> Marker -> (Marker -> Number -> a) -> Marker -> Number -> a
boundByCueNac_ st ed f m n = if m >= st && m <= ed then f m n else mempty

boundByCueNac :: forall a. Monoid a => Marker -> Marker -> (Marker -> Number -> a) -> Marker -> Number -> a
boundByCueNac st ed f = boundByCueNac_ st ed (\m n -> f m n)

boundByCueNac' :: forall a. Monoid a => Marker -> Marker -> (Number -> a) -> Marker -> Number -> a
boundByCueNac' st ed f = boundByCueNac_ st ed (\_ n -> f n)

boundByCueNac'' :: forall a. Monoid a => Marker -> Marker -> a -> Marker -> Number -> a
boundByCueNac'' st ed f = boundByCueNac_ st ed (\_ _ -> f)

boundByCueNac''' :: forall a. Monoid a => Marker -> Marker -> a -> Marker -> a
boundByCueNac''' st ed f m = boundByCueNac_ st ed (\_ _ -> f) m 0.0

conv440 :: Number -> Number
conv440 i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

conv1 :: Number -> Number
conv1 i = 1.0 * (2.0 `pow` ((i - 0.0) / 12.0))

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

loopT :: forall a. Number -> (Number -> a) -> (Number -> a)
loopT t = lcmap (_ % t)

boundPlayer :: forall a. Number -> (Number -> List a) -> Number -> List a
boundPlayer len a time = if (time) + kr >= 0.0 && time < (len) then a time else Nil

overZeroPlayer :: forall a. (Number -> List a) -> Number -> List a
overZeroPlayer = boundPlayer 100000.0 -- large enough...

skewedTriangle01 :: Number -> Number -> Number -> Number
skewedTriangle01 os len = lcmap (_ % len) go
  where
  go time
    | time < (len * os) = (time / (len * os))
    | otherwise = (len - time) / (len * (1.0 - os))

triangle01 :: Number -> Number -> Number
triangle01 = skewedTriangle01 0.5

----
wah :: forall m. Applicative m => String -> String -> Number -> Int -> List Number -> Maybe Number -> Number -> Number -> Number -> Number -> Number -> m (AudioUnit D2)
wah tag pwave len nwahs pitches filt gnStart gnEnd panStart panEnd time = pure (pannerMono_ (tag <> "WahPanner") (panStart + ((panEnd - panStart) * time / len)) ((maybe identity (highpass_ (tag <> "WahHP") 1.0) filt) (gain_ (tag <> "WahGain") (if time >= len then 0.0 else ((gnStart + (gnEnd - gnStart) * time / len) * (triangle01 (len / (toNumber nwahs)) time) / (toNumber $ L.length pitches))) (toNel (L.mapWithIndex (\i p -> periodicOsc_ (tag <> show i) pwave (conv440 p)) pitches)))))

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

toNel :: forall s. Semiring s => List s -> NonEmpty List s
toNel Nil = zero :| Nil

toNel (h : t) = h :| t

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

mic = microphone_ :: String -> AudioUnit D1

pmic :: String -> AudioUnit D2
pmic s = pannerMono_ ("voicePanner" <> s) 0.0 (mic s)

t1c440 :: Number -> Tuple Number Number
t1c440 = Tuple 1.0 <<< conv440

loopDownload :: AudioContext -> String -> Aff BrowserAudioBuffer
loopDownload ctx str =
  res
    >>= either
        ( \e -> do
            delay (Milliseconds 20.0)
            loopDownload ctx str
        )
        pure
  where
  res = try $ toAffE (decodeAudioDataFromUri ctx str)

makeBuffersUsingCache :: (O.Object BrowserAudioBuffer -> Tuple (Array (Tuple String String)) (O.Object BrowserAudioBuffer)) -> AudioContext -> O.Object BrowserAudioBuffer -> (O.Object BrowserAudioBuffer -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
makeBuffersUsingCache bf ctx prev' =
  affable do
    sequential
      ( O.union <$> (pure prev)
          <*> ( sequence
                $ O.fromFoldable
                    ( map
                        ( over _2
                            (parallel <<< loopDownload ctx)
                        )
                        (filter (not <<< flip O.member prev <<< fst) newB)
                    )
            )
      )
  where
  (Tuple newB prev) = bf prev'

makeBuffersKeepingCache :: Array (Tuple String String) -> AudioContext -> O.Object BrowserAudioBuffer -> (O.Object BrowserAudioBuffer -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
makeBuffersKeepingCache = makeBuffersUsingCache <<< Tuple

-----------------------
------------------
-----------
------
--
type SigAU
  = NatureBoyAccumulator -> Marker -> Number -> Tuple NatureBoyAccumulator (List (AudioUnit D2))

harmDel :: String -> Number -> Number -> Number -> AudioUnit D2 -> AudioUnit D2
harmDel tag l g t m =
  gain_ (tag <> "harmDelGlobal") 1.0
    ( ( gainT_' (tag <> "harmDelG0")
          ( epwf
              [ Tuple 0.0 0.0
              , Tuple 0.06 0.0
              , Tuple 0.5 1.0
              , Tuple 0.94 0.0
              , Tuple 1.0 0.0
              ]
              (t % 1.0)
          )
          (delay_ (tag <> "harmDelD0") ((l + 0.1) - l * (t % 1.0)) (gain_' (tag <> "harmDelGInner0") g m))
      )
        :| ( delay_ (tag <> "harmDelD1") 0.5
              ( gainT_' (tag <> "harmDelG1")
                  ( epwf
                      [ Tuple 0.0 0.0
                      , Tuple 0.06 0.0
                      , Tuple 0.5 1.0
                      , Tuple 0.94 0.0
                      , Tuple 1.0 0.0
                      ]
                      (t % 1.0)
                  )
                  (delay_ (tag <> "harmDelD2") ((l + 0.1) - l * (t % 1.0)) (gain_' (tag <> "harmDelGInner1") g m))
              )
          )
        : Nil
    )

there0 :: SigAU
there0 =
  boundByCue There0 There0
    (\m t -> pure (pmic "There0Mic"))

andPt2filt :: Number -> AudioUnit D2 -> AudioUnit D2
andPt2filt t = go (t % (ival * 4.0))
  where
  ival = 0.32

  go x
    | x < ival = (lowpass_ "andP2filtLP" 200.0 4.0)
    | x < 2.0 * ival = identity
    | x < 3.0 * ival = (highpass_ "andP2filtLP" 1500.0 4.0)
    | otherwise = identity

andPt2Voice :: SigAU
andPt2Voice =
  boundByCueWithOnset And7 And7
    (\ac onset m t -> pure ((andPt2filt $ (t - onset)) (pmic "And7Mic")))

genericFB :: String -> Number -> Number -> AudioUnit D2 -> AudioUnit D2
genericFB tag del gn u =
  graph_ (tag <> "GenericFBGraph")
    { aggregators:
        { out: Tuple (g'add_ (tag <> "GenericFBOut")) (SLProxy :: SLProxy ("combine" :/ SNil))
        , combine: Tuple (g'add_ (tag <> "GenericFBCombine")) (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
        , gain: Tuple (g'gain_ (tag <> "GenericFBGain") gn) (SLProxy :: SLProxy ("del" :/ SNil))
        }
    , processors:
        { del: Tuple (g'delay_ (tag <> "GenericFBDelay") del) (SProxy :: SProxy "combine")
        }
    , generators:
        { mic: u
        }
    }

was0 :: SigAU
was0 =
  boundByCue Was0 Boy0
    ( \m t ->
        pure
          $ graph_ "Was0Graph"
              { aggregators:
                  { out: Tuple (g'add_ "Was0Out") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "Was0Combine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "Was0Gain" 0.7) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "Was0Delay" 0.4) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: boundByCueNac''' Was0 Was0 (pmic "Was0Mic") m
                  }
              }
    )

theGreatestThingYoullEverLearnIsJustToLove :: SigAU
theGreatestThingYoullEverLearnIsJustToLove =
  boundByCue The12 Love13
    ( \m t ->
        pure
          $ graph_ "theGreatestThingYoullEverLearnIsJustToLoveGraph"
              { aggregators:
                  { out: Tuple (g'add_ "theGreatestThingYoullEverLearnIsJustToLoveOut") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "theGreatestThingYoullEverLearnIsJustToLoveCombine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "theGreatestThingYoullEverLearnIsJustToLoveGain" 0.65) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "theGreatestThingYoullEverLearnIsJustToLoveDelay" 0.31) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: (pmic "theGreatestThingYoullEverLearnIsJustToLoveMic")
                  }
              }
    )

beLovedInReturn :: SigAU
beLovedInReturn =
  boundByCue Be13 Turn13
    ( \m t ->
        pure
          $ graph_ "beLovedInReturnGraph"
              { aggregators:
                  { out: Tuple (g'add_ "beLovedInReturnOut") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "beLovedInReturnCombine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "beLovedInReturnGain" 0.48) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "beLovedInReturnDelay" 0.43) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: (pmic "beLovedInReturnMic")
                  }
              }
    )

meBeforeGreatest :: SigAU
meBeforeGreatest =
  boundByCue Me11 The12
    ( \m t ->
        pure
          $ graph_ "Me11Graph"
              { aggregators:
                  { out: Tuple (g'add_ "Me11Out") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "Me11Combine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "Me11Gain" 0.9) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "Me11Delay" 0.23) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: boundByCueNac''' Me11 Me11 (pmic "Me11Mic") m
                  }
              }
    )

showM2n = show <<< m2n :: Marker -> String

veRyVoice :: Marker -> Marker -> Marker -> SigAU
veRyVoice ve ry far =
  boundByCue ve far
    ( \m t ->
        pure
          $ graph_ ("Ve3Ry3Graph" <> showM2n ve)
              { aggregators:
                  { out: Tuple (g'add_ ("Ve3Ry3Out" <> showM2n ve)) (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ ("Ve3Ry3Combine" <> showM2n ve)) (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ ("Ve3Ry3Gain" <> showM2n ve) 0.7) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ ("Ve3Ry3Delay" <> showM2n ve) 0.4) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: boundByCueNac''' ve ry (pmic ("Ve3Ry3Mic" <> showM2n ve)) m
                  }
              }
    )

farVoice :: Marker -> SigAU
farVoice far' =
  boundByCue far' far'
    ( \m t ->
        pure
          $ graph_ ("Far3Graph" <> showM2n far')
              { aggregators:
                  { out: Tuple (g'add_ ("Far3Out" <> showM2n far')) (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ ("Far3Combine" <> showM2n far')) (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ ("Far3Gain" <> showM2n far') 0.35) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ ("Far3Delay" <> showM2n far') 0.2) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: boundByCueNac''' far' far' (pmic ("Far3Mic" <> showM2n far')) m
                  }
              }
    )

veRy2 = veRyVoice Ve2 Ry2 Far2 :: SigAU

far2 = farVoice Far2 :: SigAU

veRy3 = veRyVoice Ve3 Ry3 Far3 :: SigAU

far3 = farVoice Far3 :: SigAU

overLandAnd :: SigAU
overLandAnd =
  boundByCue O4 Sea4
    ( \m t ->
        pure
          $ graph_ "overLandAndGraph"
              { aggregators:
                  { out: Tuple (g'add_ "overLandAndOut") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "overLandAndCombine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "overLandAndGain" 0.3) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "overLandAndDelay" 0.5) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: boundByCueNac''' O4 And4 (pmic "overLandAndMic") m
                  }
              }
    )

seaVoice :: SigAU
seaVoice =
  boundByCue Sea4 Sea4
    ( \m t ->
        pure
          $ graph_ "seaGraph"
              { aggregators:
                  { out: Tuple (g'add_ "seaOut") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "seaCombine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "seaGain" 0.3) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "seaDelay" 0.1) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: pmic "seaMic"
                  }
              }
    )

aVoicePedal :: SigAU
aVoicePedal =
  boundByCue A5 Of5
    ( \m t ->
        pure
          $ graph_ "aVoicePedalGraph"
              { aggregators:
                  { out: Tuple (g'add_ "aVoicePedalOut") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "aVoicePedalCombine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "aVoicePedalGain" 0.94) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "aVoicePedalDelay" 0.1) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: boundByCueNac''' A5 A5 (pmic "aVoicePedalMic") m
                  }
              }
    )

littleShyVoice :: SigAU
littleShyVoice =
  boundByCue Lit5 Shy5
    ( \m t ->
        pure
          $ graph_ "littleShyVoiceGraph"
              { aggregators:
                  { out: Tuple (g'add_ "littleShyVoiceOut") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "littleShyVoiceCombine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "littleShyVoiceGain" 0.3) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "littleShyVoiceDelay" 0.45) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: pmic "littleShyVoiceMic"
                  }
              }
    )

andVoice :: SigAU
andVoice =
  boundByCueWithOnset And5 And5
    ( \ac onset m t ->
        pure
          $ graph_ "andVoiceGraph"
              { aggregators:
                  { out: Tuple (g'add_ "andVoiceOut") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "andVoiceCombine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "andVoiceGain" 0.26) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "andVoiceDelay" 0.21) (SProxy :: SProxy "hpf")
                  , hpf: Tuple (g'highpass_ "andVoiceMic" (500.0 + (3500.0 * (t - onset))) 14.0) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: pmic "andVoiceMic"
                  }
              }
    )

sadOfEyeVoice :: SigAU
sadOfEyeVoice =
  boundByCue Sad5 Eye5
    ( \m t ->
        pure
          $ graph_ "sadOfEyeVoiceGraph"
              { aggregators:
                  { out: Tuple (g'add_ "sadOfEyeVoiceOut") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "sadOfEyeVoiceCombine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "sadOfEyeVoiceGain" 0.3) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "sadOfEyeVoiceDelay" 0.45) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: pmic "sadOfEyeVoiceMic"
                  }
              }
    )

butPedalVoice :: SigAU
butPedalVoice =
  boundByCue But6 Was6
    ( \m t ->
        pure
          $ graph_ "butPedalVoiceGraph"
              { aggregators:
                  { out: Tuple (g'add_ "butPedalVoiceOut") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "butPedalVoiceCombine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "butPedalVoiceGain" 0.94) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "butPedalVoiceDelay" 0.1) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: boundByCueNac''' But6 But6 (pmic "butPedalVoiceMic") m
                  }
              }
    )

veryWiseWasHeVoice :: SigAU
veryWiseWasHeVoice =
  boundByCueWithOnset Ve6 He6
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure $ dup2 (pmic "butVeryWiseWasHePmic") \d -> (gainT_' "butVeryWiseWasHeDry" (epwf [ Tuple 0.0 1.0, Tuple 4.0 0.2, Tuple 6.0 1.0 ] time) d + (gainT_' "butVeryWiseWasHeWet" (epwf [ Tuple 0.0 0.0, Tuple 4.0 0.8, Tuple 6.0 0.0 ] time) $ convolver_ "veryWiseWasHeVoiceConvolver" "matrix-verb-3" d))
    )

alsAccomp :: String -> Marker -> Marker -> SigAU
alsAccomp tag st ed =
  boundByCueWithOnset st ed
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure (bandpass_ (tag <> "ALittleShyBandpass") (1000.0 + 400.0 * sin (pi * time * 0.2)) (3.0 + 2.5 * sin (pi * time * 0.3)) $ loopBuf_ (tag <> "ALittleShyBuf") tag 1.0 (0.6 + 0.6 * sin (time * pi)) (3.248 + 0.6 * sin (time * pi)))
    )

preALittleShyAccomp = alsAccomp "pre-a-little-shy" Sea4 Sea4 :: SigAU

aLittleShyAccomp = alsAccomp "a-little-shy" A5 Shy5 :: SigAU

andSadOfEyeAccomp = alsAccomp "and-sad-of-eye" And5 Eye5 :: SigAU

butVeryWiseWasAccomp = alsAccomp "but-very-wise-was" But6 Was6 :: SigAU

heAccomp = alsAccomp "he" He6 He6 :: SigAU

heRichSwell :: SigAU
heRichSwell =
  boundByCueWithOnset Ve6 Was6
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure (pannerMono_ "heRichSwellPan" 0.0 (gain_ "heRichSwellFade" 1.0 ((gain_' "heRichSwellGain0" (max 0.3 $ time * 0.4) (periodicOsc_ "heRichSwellOsc0" "rich" (conv440 32.0))) :| (gain_' "heRichSwellGain1" (max 0.2 $ time * 0.5) (periodicOsc_ "heRichSwellOsc1" "rich" (conv440 44.0))) : Nil)))
    )

-- (wah "test" "smooth" 0.4 3 (60.0 : 64.0 : 67.0 : Nil) Nothing 0.2 0.9 0.5 0.5 time)
bpWah :: Number -> String -> String -> Number -> Int -> List Number -> Maybe Number -> Number -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
bpWah t tag pwave len nwahs pitches filt gnStart gnEnd panStart panEnd = atT t (boundPlayer (len + 0.3) (wah tag pwave len nwahs pitches filt gnStart gnEnd panStart panEnd))

veryWiseWahs =
  [ bpWah 1.0 "f#" "smooth" 0.3 1 (56.0 : Nil) Nothing 0.3 0.3 0.5 0.5
  , bpWah 1.4 "e" "rich" 0.3 1 (64.0 : Nil) (Just 3500.0) 0.3 0.3 (-0.5) (-0.5)
  , bpWah 1.8 "b" "smooth" 0.3 1 (71.0 : Nil) Nothing 0.4 0.4 (0.0) (0.0)
  , bpWah 2.2 "d#" "rich" 0.2 1 (75.0 : Nil) (Just 3500.0) 0.25 0.6 (-0.3) (0.4)
  , bpWah 2.6 "f#" "smooth" 0.5 3 (32.0 : Nil) Nothing 0.3 0.3 0.0 0.0
  , bpWah 3.1 "f#-2" "smooth" 0.3 1 (44.0 : Nil) Nothing 0.2 0.6 0.0 0.0
  , bpWah 3.5 "e" "rich" 0.3 1 (61.0 : 64.0 : Nil) (Just 3500.0) 0.3 0.3 (-0.5) (-0.5)
  , bpWah 3.9 "b" "smooth" 0.3 2 (68.0 : 71.0 : Nil) Nothing 0.4 0.4 (0.0) (0.0)
  , bpWah 4.3 "d#" "rich" 0.2 3 (75.0 : Nil) (Just 3500.0) 0.25 0.6 (-0.3) (0.4)
  , bpWah 4.7 "f#" "smooth" 0.5 3 (32.0 : Nil) Nothing 0.3 0.3 0.0 0.0
  , bpWah 5.1 "f#-2" "smooth" 0.3 1 (44.0 : Nil) Nothing 0.2 0.6 0.0 0.0
  , bpWah 5.5 "d#" "rich" 0.2 3 (75.0 : Nil) (Just 3500.0) 0.25 0.6 (-0.3) (0.4)
  , bpWah 5.9 "a#" "rich" 0.2 3 (82.0 : Nil) (Just 4500.0) 0.2 0.2 (0.3) (-0.4)
  ] ::
    Array (Number -> List (AudioUnit D2))

veryWiseWasHeWahs :: SigAU
veryWiseWasHeWahs =
  boundByCueWithOnset Ve6 Was6
    ( \ac onset m t ->
        let
          time = t - onset
        in
          fold
            ( map (\f -> f time)
                veryWiseWahs
            )
    )

veryWiseWasBassoon :: SigAU
veryWiseWasBassoon =
  boundByCueWithOnset Ve6 Was6
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure (gain_' "bassoonVeryWiseGain" (max 0.0 (1.0 - 0.25 * (t - onset))) (lowpass_ "bassoonVeryWiseLowpass" (50.0) 1.0 $ loopBuf_ "bassoonVeryWiseLoop" "bassoon-low-d" 1.0 0.7 1.9))
    )

veryWiseWasSkiddaw :: SigAU
veryWiseWasSkiddaw =
  boundByCueWithOnset Ve6 Was6
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure (gain_' "skiddawVeryWiseGain" (1.0) (playBuf_ "skiddawVeryWiseBuf" "skiddaw-low-d" 1.0))
    )

wiseWasHeAndClock :: SigAU
wiseWasHeAndClock =
  boundByCueWithOnset Wise6 And7
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure (gain_' "wiseWasHeAndClockGain" (min 1.0 (0.1 * time)) (loopBuf_ "wiseWasHeAndClockBuf" "wall-clock" 1.0 0.0 0.0))
    )

planeLandingEnv = [ Tuple 0.0 0.0, Tuple 1.0 1.0, Tuple 1.64 1.0, Tuple 1.7 0.0, Tuple 1.8 1.0, Tuple 2.24 1.0, Tuple 2.3 0.0, Tuple 2.4 1.0, Tuple 2.6 1.0, Tuple 2.65 0.0, Tuple 2.72 1.0, Tuple 2.9 1.0, Tuple 2.94 0.0, Tuple 3.0 1.0, Tuple 3.4 1.0, Tuple 3.42 0.0, Tuple 3.5 1.0, Tuple 3.6 1.0, Tuple 3.62 0.0, Tuple 3.7 1.0 ] :: Array (Tuple Number Number)

planeLanding :: SigAU
planeLanding =
  boundByCueWithOnset He6 He6
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure (gainT_' "planeLandingGain" (epwf planeLandingEnv time) (playBuf_ "planeLandingBuf" "plane-landing" 1.0))
    )

scratchySwellHe :: SigAU
scratchySwellHe =
  boundByCueWithOnset He6 He6
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure (highpass_ "scratchySwellHeHP" (4500.0 - ((skewedTriangle01 0.8 1.5 time) * 2500.0)) 3.0 (loopBuf_ "scratchySwellHeBuf" "scratchy-swell" 1.0 0.0 0.0))
    )

data WasHeBuffer
  = WHWarble
  | WHToTheHeavens
  | WHShakyScratchy

makeWHBuffer :: WasHeBuffer -> Number -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
makeWHBuffer whs when os len hpf =
  atT when
    $ boundPlayer (len + 0.06)
        ( \t ->
            pure
              ( gainT_' (show t <> "whGain") (epwf [ Tuple 0.0 0.0, Tuple (len / 2.0) 0.1, Tuple (4.0 * len / 5.0) 0.2, Tuple len 1.0, Tuple (len + 0.022) 0.0 ] t)
                  ( highpass_ (show t <> "whHighPass") hpf 1.0
                      ( playBufWithOffset_ (show t <> "whBuf")
                          ( case whs of
                              WHWarble -> "warble"
                              WHToTheHeavens -> "to-the-heavens"
                              WHShakyScratchy -> "shaky-scratchy"
                          )
                          1.0
                          ( max os
                              ( case whs of
                                  WHWarble -> 18.0
                                  WHToTheHeavens -> 24.0
                                  WHShakyScratchy -> 12.0
                              )
                          )
                      )
                  )
              )
        )

whbufs :: Array (Number -> List (AudioUnit D2))
whbufs =
  [ makeWHBuffer WHWarble 0.1 4.3 1.3 3500.0
  , makeWHBuffer WHToTheHeavens 0.9 2.3 0.6 2000.0
  , makeWHBuffer WHShakyScratchy 1.5 1.3 0.4 4000.0
  , makeWHBuffer WHWarble 1.9 0.4 0.4 2000.0
  , makeWHBuffer WHToTheHeavens 2.2 0.7 0.48 1500.0
  , makeWHBuffer WHShakyScratchy 2.3 0.1 0.47 1000.0
  , makeWHBuffer WHToTheHeavens 2.45 5.2 0.42 500.0
  , makeWHBuffer WHShakyScratchy 2.55 4.3 0.41 3000.0
  , makeWHBuffer WHShakyScratchy 2.65 3.8 0.35 3500.0
  , makeWHBuffer WHToTheHeavens 2.8 1.2 0.48 2000.0
  , makeWHBuffer WHShakyScratchy 2.9 1.7 0.52 1500.0
  , makeWHBuffer WHWarble 3.1 0.1 0.8 1200.0
  , makeWHBuffer WHShakyScratchy 3.35 0.0 0.12 1100.0
  , makeWHBuffer WHToTheHeavens 3.45 0.82 0.15 900.0
  , makeWHBuffer WHShakyScratchy 3.5 3.2 0.45 1900.0
  , makeWHBuffer WHWarble 3.62 2.2 0.22 2000.0
  , makeWHBuffer WHToTheHeavens 3.7 4.3 0.23 200.0
  , makeWHBuffer WHToTheHeavens 3.85 2.1 0.34 3500.0
  , makeWHBuffer WHShakyScratchy 3.9 5.3 0.56 2000.0
  , makeWHBuffer WHWarble 4.05 3.3 0.75 1200.0
  , makeWHBuffer WHShakyScratchy 4.15 1.3 0.235 1000.0
  , makeWHBuffer WHToTheHeavens 4.2 0.85 0.2 700.0
  , makeWHBuffer WHToTheHeavens 4.4 7.3 0.234 1900.0
  , makeWHBuffer WHWarble 4.45 8.1 0.6 2100.0
  , makeWHBuffer WHShakyScratchy 4.5 5.6 0.1 1400.0
  , makeWHBuffer WHShakyScratchy 5.0 10.2 0.234 1300.0
  , makeWHBuffer WHWarble 5.1 4.2 0.88 1200.0
  , makeWHBuffer WHToTheHeavens 5.12 0.7 0.54 1100.0
  , makeWHBuffer WHShakyScratchy 5.23 6.8 0.235 1200.0
  , makeWHBuffer WHShakyScratchy 5.25 4.43 0.32 1300.0
  , makeWHBuffer WHWarble 5.3 7.1 0.4 1400.0
  , makeWHBuffer WHWarble 5.45 1.7 0.3 1500.0
  , makeWHBuffer WHToTheHeavens 5.5 0.01 0.55 1600.0
  , makeWHBuffer WHShakyScratchy 5.53 0.56 0.3 1700.0
  , makeWHBuffer WHShakyScratchy 5.6 2.45 0.2 800.0
  , makeWHBuffer WHWarble 5.61 6.4 0.19 900.0
  , makeWHBuffer WHToTheHeavens 5.69 9.9 0.25 1000.0
  , makeWHBuffer WHShakyScratchy 5.96 8.32 0.24 1100.0
  , makeWHBuffer WHToTheHeavens 6.1 8.1 0.23 1200.0
  ]

wasHeGlitches :: SigAU
wasHeGlitches = boundByCueWithOnset Was6 He6 \_ onset _ t -> fold (map (\f -> f (t - onset)) whbufs)

littleShyHigh :: SigAU
littleShyHigh =
  boundByCueWithOnset Lit5 Shy5
    ( \ac onset m t ->
        let
          time = t - onset
        in
          atT 0.4 (overZeroPlayer (const $ pure (gain_' "aLittleShyHighGain" (min (time * 0.3) 1.0) (playBuf_ "aLittleShyHighBuf" "g-sharp-a-sharp-high" 1.0)))) time
    )

sadOfEyeHigh :: SigAU
sadOfEyeHigh =
  boundByCueWithOnset Sad5 Eye5
    ( \ac onset m t ->
        let
          time = t - onset
        in
          atT 0.3 (overZeroPlayer (const $ pure (gain_' "sadOfEyeHighGain" (min (time * 0.25) 1.0) (playBuf_ "sadOfEyeHighGain" "c-sharp-d-sharp-high" 1.0)))) time
    )

veryWiseWasHeHigh :: SigAU
veryWiseWasHeHigh =
  boundByCueWithOnset Ve6 Was6
    ( \ac onset m t ->
        let
          time = t - onset
        in
          atT 0.1 (overZeroPlayer (const $ pure (gain_' "veryWiseWasHeGain" (min (time * 0.2) 1.0) (playBuf_ "veryWiseWasHeGain" "g-a-high" 1.0)))) time
    )

a0 :: SigAU
a0 =
  boundByCue A0 A0
    (\m t -> pure (pmic "A0Mic"))

boy0 :: SigAU
boy0 =
  boundByCue_ Boy0 A1
    ( \ac m t ->
        Tuple ac
          ( pure
              $ dup2
                  ( graph_ "Boy0Graph"
                      { aggregators:
                          { out: Tuple (g'add_ "Boy0Out") (SLProxy :: SLProxy ("combine" :/ SNil))
                          , combine: Tuple (g'add_ "Boy0Combine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                          , gain: Tuple (g'gain_ "Boy0Gain" 0.78) (SLProxy :: SLProxy ("del" :/ SNil))
                          }
                      , processors:
                          { del: Tuple (g'delay_ "Boy0Delay" 0.26) (SProxy :: SProxy "combine")
                          }
                      , generators:
                          { mic: boundByCueNac''' Boy0 Boy0 (pmic "Boy0Mic") m
                          }
                      }
                  ) \d ->
                  -- adds a small octave descant
                  ( gain_ "Boy0Comb" 1.0
                      ( d
                          :| ( maybe Nil
                                ( \onset ->
                                    ( gain_' "Boy0RampUpOsc" 3.5
                                        $ mul_ "Boy0Mul"
                                            ( ( pannerMono_ "Boy0OctavePan" (0.3 * sin (0.8 * pi * t))
                                                  $ playBufWithOffset_ "Boy0Octave" "flute" 1.0 3.8
                                              )
                                                :| ( audioWorkletProcessor_ "Boy0OctaveGate"
                                                      "klank-amplitude"
                                                      O.empty
                                                      d
                                                  )
                                                : Nil
                                            )
                                    )
                                      : Nil
                                )
                                (M.lookup m ac.markerOnsets)
                            )
                      )
                  )
          )
    )

boyDupedOnset :: Number -> AudioUnit D2 -> List (AudioUnit D2)
boyDupedOnset t d =
  pure
    ( mul_ "Boy1Mul"
        ( ( pannerMono_ "Boy1OctavePan" (0.3 * sin (0.8 * pi * t))
              $ gain_ "Boy1Combined" (3.5 * (max 0.0 $ 0.5 * (1.0 - t)))
                  ( (periodicOsc_ "Boy1Octave" "smooth" (conv440 61.0))
                      :| (periodicOsc_ "Boy1Octavem3" "smooth" (conv440 64.0))
                      : Nil
                  )
          )
            :| ( audioWorkletProcessor_ "Boy1OctaveGate"
                  "klank-amplitude"
                  O.empty
                  d
              )
            : Nil
        )
    )

boy1 :: SigAU
boy1 =
  boundByCueWithOnset Boy1 Boy1 \ac onset m t ->
    pure
      $ dup2
          (pmic "Boy1Mic") \d ->
          (gain_ "Boy1Comb" 1.0 (d :| (boyDupedOnset (t - onset) d)))

they2 :: SigAU
they2 =
  boundByCueWithOnset They2 Dered2
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure
            $ graph_ "They2Graph"
                { aggregators:
                    { out: Tuple (g'add_ "They2Out") (SLProxy :: SLProxy ("combine" :/ SNil))
                    , combine: Tuple (g'add_ "They2Combine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                    , gain: Tuple (g'gain_ "They2Gain" (min 0.7 $ 2.0 * time)) (SLProxy :: SLProxy ("del" :/ SNil))
                    }
                , processors:
                    { del: Tuple (g'delay_ "They2Delay" 0.5) (SProxy :: SProxy "combine")
                    }
                , generators:
                    { mic: boundByCueNac''' They2 They2 (pmic "They2Mic") m
                    }
                }
    )

sayHeWandered :: SigAU
sayHeWandered =
  boundByCue Say2 Dered2
    (\m t -> pure (pmic "SayHeWanderedMic"))

a1 :: SigAU
a1 =
  boundByCue A1 A1
    (\m t -> pure (pmic "A1Mic"))

singleLowGSharpCello :: String -> Number -> AudioUnit D2
singleLowGSharpCello s time =
  ( gainT_' (s <> "CelloPlayer")
      (epwf [ Tuple 0.0 0.0, Tuple 0.5 1.0, Tuple 2.5 1.0, Tuple 3.0 0.0 ] time)
      ( lowpass_
          (s <> "CelloPlayer")
          (75.0)
          10.0
          (playBuf_ (s <> "CelloPlayer") "low-g#" 1.0)
      )
  )

tshwvfGong :: String -> Marker -> Marker -> Number -> SigAU
tshwvfGong buf st ed loc = boundByCueWithOnset st ed \ac onset m t -> let time = t - onset in (atT loc $ overZeroPlayer (const $ pure (playBuf_ (buf <> "GongPlayer") buf 1.0))) time

theyGong = tshwvfGong "kettle-g-sharp-3" They2 Wan2 0.5 :: SigAU

sayGong = tshwvfGong "kettle-a-3" Say2 Wan2 0.4 :: SigAU

heGong = tshwvfGong "kettle-c-4" He2 Dered2 0.4 :: SigAU

wanGong = tshwvfGong "kettle-e-flat-4" Wan2 Ve3 0.3 :: SigAU

deredGong = tshwvfGong "kettle-f-sharp-4" Dered2 Ry3 0.2 :: SigAU

--
tshwvfPad :: String -> Marker -> Marker -> SigAU
tshwvfPad buf st ed = boundByCueWithOnset st ed \ac onset m t -> let time = t - onset in pure (gainT_' (buf <> "PadGain") (epwf [ Tuple 0.0 0.0, Tuple 0.25 1.0, Tuple 1.2 0.0 ] time) (playBuf_ (buf <> "PadPlayer") buf 1.0))

sayPad = tshwvfPad "say-pad" Say2 Ve3 :: SigAU

hePad = tshwvfPad "he-pad" He2 Ve3 :: SigAU

wanPad = tshwvfPad "wan-pad" Wan2 Ve3 :: SigAU

deredPad = tshwvfPad "dered-pad" Dered2 Ry3 :: SigAU

veRyPad = tshwvfPad "very-pad" Ve3 Far3 :: SigAU

-- at offset length freq q
data AOLFQ
  = AOLFQ Number Number Number Number Number

theySayHeWanderedCymbalFragment :: Marker -> Marker -> Array AOLFQ -> SigAU
theySayHeWanderedCymbalFragment st ed olfq =
  boundByCueWithOnset st ed
    ( \ac onset m t ->
        let
          time = t - onset
        in
          fold
            ( map
                ( \(AOLFQ a o l f q) ->
                    ( atT a
                        $ boundPlayer (l + 0.06)
                            ( \tm ->
                                pure
                                  ( gainT_' (show a <> m2s st <> "theySayGain") (epwf [ Tuple 0.0 0.0, Tuple 1.0 l ] tm)
                                      (highpass_ (show a <> m2s st <> "theySayHP") f q $ playBufWithOffset_ (show a <> m2s st <> "theySayBuf") "revcym" 1.0 o)
                                  )
                            )
                    )
                      time
                )
                olfq
            )
    )

theySayHeWanderedBuildup =
  [ theySayHeWanderedCymbalFragment They2 Say2
      [ AOLFQ 0.0 0.8 0.8 1000.0 3.0
      , AOLFQ 0.6 1.4 0.5 500.0 2.0
      , AOLFQ 0.9 1.5 0.6 1500.0 6.0
      , AOLFQ 1.3 1.0 0.3 700.0 4.0
      ]
  , theySayHeWanderedCymbalFragment Say2 He2
      [ AOLFQ 0.0 1.2 0.8 1000.0 3.0
      , AOLFQ 0.5 1.4 0.5 500.0 2.0
      , AOLFQ 0.9 2.0 0.6 1500.0 6.0
      , AOLFQ 1.4 1.7 1.0 700.0 4.0
      ]
  , theySayHeWanderedCymbalFragment He2 Wan2
      [ AOLFQ 0.0 1.6 0.8 1000.0 3.0
      , AOLFQ 0.4 2.3 0.5 500.0 2.0
      , AOLFQ 0.8 2.6 0.7 1500.0 6.0
      , AOLFQ 1.2 2.5 0.5 700.0 4.0
      ]
  , theySayHeWanderedCymbalFragment Wan2 Dered2
      [ AOLFQ 0.0 2.3 0.8 1000.0 3.0
      , AOLFQ 0.35 3.3 0.5 500.0 2.0
      , AOLFQ 0.7 4.2 0.6 1500.0 6.0
      , AOLFQ 1.05 3.1 0.3 2800.0 8.0
      , AOLFQ 1.2 4.5 0.4 700.0 4.0
      ]
  , theySayHeWanderedCymbalFragment Dered2 Ve2
      [ AOLFQ 0.0 4.8 0.8 1600.0 7.0
      , AOLFQ 0.3 5.1 0.5 500.0 2.0
      , AOLFQ 0.6 5.9 0.15 3000.0 6.0
      , AOLFQ 0.9 5.5 0.5 300.0 2.0
      , AOLFQ 1.2 5.8 0.3 2000.0 4.0
      ]
  , theySayHeWanderedCymbalFragment Ve2 Ry2
      [ AOLFQ 0.0 4.8 0.8 1600.0 7.0
      , AOLFQ 0.3 4.7 0.5 500.0 2.0
      , AOLFQ 0.6 4.2 0.15 3000.0 6.0
      , AOLFQ 0.9 4.8 0.5 300.0 2.0
      , AOLFQ 1.2 3.9 0.3 1500.0 4.0
      ]
  , theySayHeWanderedCymbalFragment Ry2 Far2
      [ AOLFQ 0.0 2.3 0.8 1000.0 3.0
      , AOLFQ 0.6 3.3 0.5 500.0 2.0
      , AOLFQ 0.9 4.2 0.6 1500.0 6.0
      , AOLFQ 1.2 3.1 0.3 700.0 4.0
      ]
  , theySayHeWanderedCymbalFragment Far2 Ve3
      [ AOLFQ 0.0 1.2 0.8 1000.0 3.0
      , AOLFQ 0.6 1.4 0.5 500.0 2.0
      , AOLFQ 0.9 2.0 0.6 1500.0 6.0
      , AOLFQ 1.2 1.7 0.3 700.0 4.0
      ]
  ] ::
    Array SigAU

boundLowGSharpCello :: String -> Number -> List (AudioUnit D2)
boundLowGSharpCello s = boundPlayer 3.1 (map pure (singleLowGSharpCello s))

-- creates a pad of low g# cello
celloLowGSharpRack :: Number -> List (AudioUnit D2)
celloLowGSharpRack time =
  fold
    ( mapWithIndex (\i f -> f time)
        [ atT 0.0 $ boundLowGSharpCello "a"
        , atT 2.5 $ boundLowGSharpCello "b"
        , atT 5.0 $ boundLowGSharpCello "c"
        , atT 7.5 $ boundLowGSharpCello "d"
        , atT 10.0 $ boundLowGSharpCello "e"
        , atT 12.5 $ boundLowGSharpCello "f"
        , atT 15.0 $ boundLowGSharpCello "g"
        , atT 17.5 $ boundLowGSharpCello "h"
        , atT 20.0 $ boundLowGSharpCello "i"
        , atT 22.5 $ boundLowGSharpCello "j"
        ]
    )

celloVeryStrangeEnchantedDrone :: SigAU
celloVeryStrangeEnchantedDrone =
  boundByCueWithOnset A1 Boy1
    ( \ac onset m t ->
        let
          now = t - onset
        in
          pure
            ( gain_
                "CelloFadeInOut"
                ( if m < Chan1 then
                    -- fade in if below <= En1
                    (min (now * 0.5) 1.0)
                  else
                    -- fade out if > Chan1
                    ( maybe
                        1.0
                        (\chan -> max 0.0 (1.0 - ((t - chan) * 0.2)))
                        (M.lookup Chan1 ac.markerOnsets)
                    )
                )
                (toNel (celloLowGSharpRack now))
            )
    )

veRyStrangeEn :: SigAU
veRyStrangeEn =
  boundByCueWithOnset Ve1 Boy1
    ( \ac onset m t ->
        graph_ "VeRyStrangeEnGraph"
          { aggregators:
              { out: Tuple (g'add_ "VeRyStrangeEnOut") (SLProxy :: SLProxy ("combine" :/ SNil))
              , combine: Tuple (g'add_ "VeRyStrangeEnCombine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
              , gain: Tuple (g'gain_ "VeRyStrangeEnGain" $ min 0.7 (0.7 * 0.5 * (t - onset))) (SLProxy :: SLProxy ("del" :/ SNil))
              }
          , processors:
              { del: Tuple (g'delay_ "VeRyStrangeEnDelay" 0.2) (SProxy :: SProxy "combine")
              }
          , generators:
              { mic: boundByCueNac''' Ve1 En1 (pmic "VeRyStrangeEnMic") m
              }
          }
          : Nil
    )

chanTed :: SigAU
chanTed =
  boundByCue Chan1 Boy1
    ( \m t ->
        pure
          $ graph_ "Chan1Graph"
              { aggregators:
                  { out: Tuple (g'add_ "Chan1Out") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "Chan1Combine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "Chan1Gain" 0.3) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "Chan1Delay" 0.25) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: boundByCueNac''' Chan1 Ted1 (pmic "Chan1Mic") m
                  }
              }
    )

data Harm0
  = Harm0'A
  | Harm0'120
  | Harm0'110
  | Harm0'90
  | Harm0'70
  | Harm0'40

derive instance harm0Eq :: Eq Harm0

harm0Factory :: Number -> Harm0
harm0Factory t
  | t < 1.0 = Harm0'A
  | t < 1.4 = Harm0'120
  | t < 2.5 = Harm0'70
  | t < 3.25 = Harm0'A
  | t < 5.2 = Harm0'40
  | t < 7.0 = Harm0'110
  | t < 7.6 = Harm0'A
  | t < 9.2 = Harm0'120
  | t < 10.1 = Harm0'40
  | t < 13.6 = Harm0'110
  | otherwise = Harm0'A

harm0Gain :: Number -> Harm0 -> Number
harm0Gain t h = if harm0Factory t == h then 1.0 else 0.0

harm0 :: SigAU
harm0 =
  boundByCueWithOnset Ve2 And4
    ( \ac onset m t'' ->
        let
          t' = t'' - onset
        in
          atT 0.6
            ( overZeroPlayer
                ( \t ->
                    ( pure
                        ( gainT_ "Harm0Gain"
                            ( epwf
                                [ Tuple 0.0 0.0
                                , Tuple 1.0 0.0
                                , Tuple 2.0 0.1
                                , Tuple 2.2 0.7
                                , Tuple 2.4 0.1
                                , Tuple 3.0 0.7
                                , Tuple 3.1 0.1
                                , Tuple 3.2 0.6
                                , Tuple 10.0 0.3
                                , Tuple 20.0 0.0
                                ]
                                t
                            )
                            ( ( gain_' "Harm0--Gain"
                                  (harm0Gain t Harm0'A)
                                  $ playBuf_
                                      "Harm0--Play"
                                      "harm-0"
                                      1.0
                              )
                                :| ( gain_' "Harm0-120Gain"
                                      (harm0Gain t Harm0'120)
                                      $ playBuf_
                                          "Harm0-120Play"
                                          "harm-0-120"
                                          1.0
                                  )
                                : ( gain_' "Harm0-110Gain"
                                      (harm0Gain t Harm0'110)
                                      $ playBuf_
                                          "Harm0-110Play"
                                          "harm-0-110"
                                          1.0
                                  )
                                : ( gain_' "Harm0-90Gain"
                                      (harm0Gain t Harm0'90)
                                      $ playBuf_
                                          "Harm0-90Play"
                                          "harm-0-90"
                                          1.0
                                  )
                                : ( gain_' "Harm0-70Gain"
                                      (harm0Gain t Harm0'70)
                                      $ playBuf_
                                          "Harm0-70Play"
                                          "harm-0-70"
                                          1.0
                                  )
                                : ( gain_' "Harm0-40Gain"
                                      (harm0Gain t Harm0'40)
                                      $ playBuf_
                                          "Harm0-40Play"
                                          "harm-0-40"
                                          1.0
                                  )
                                : Nil
                            )
                        )
                    )
                )
            )
            t'
    )

--- harm1
data Harm1
  = Harm1'A
  | Harm1'120
  | Harm1'50
  | Harm1'90

derive instance harm1Eq :: Eq Harm1

harm1Factory :: Number -> Harm1
harm1Factory t
  | t < 1.2 = Harm1'120
  | t < 2.0 = Harm1'50
  | t < 3.5 = Harm1'A
  | t < 3.7 = Harm1'120
  | t < 4.6 = Harm1'A
  | t < 4.7 = Harm1'120
  | t < 5.25 = Harm1'50
  | t < 5.25 = Harm1'90
  | otherwise = Harm1'A

harm1Gain :: Number -> Harm1 -> Number
harm1Gain t h = if harm1Factory t == h then 1.0 else 0.0

harm1 :: SigAU
harm1 =
  boundByCueWithOnset Ve3 And4
    ( \ac onset m t'' ->
        let
          t' = t'' - onset
        in
          atT 0.5
            ( overZeroPlayer
                ( \t ->
                    pure
                      ( gainT_ "Harm1Gain"
                          ( epwf
                              [ Tuple 0.0 0.0
                              , Tuple 1.0 0.8
                              , Tuple 20.0 0.0
                              ]
                              t
                          )
                          ( ( gain_' "Harm1--Gain"
                                (harm1Gain t Harm1'A)
                                $ playBuf_
                                    "Harm1--Play"
                                    "harm-1"
                                    1.0
                            )
                              :| ( gain_' "Harm1-120Gain"
                                    (harm1Gain t Harm1'120)
                                    $ playBuf_
                                        "Harm1-120Play"
                                        "harm-1-120"
                                        1.0
                                )
                              : ( gain_' "Harm1-90Gain"
                                    (harm1Gain t Harm1'90)
                                    $ playBuf_
                                        "Harm1-90Play"
                                        "harm-1-90"
                                        1.0
                                )
                              : ( gain_' "Harm1-50Gain"
                                    (harm1Gain t Harm1'50)
                                    $ playBuf_
                                        "Harm1-50Play"
                                        "harm-1-50"
                                        1.0
                                )
                              : Nil
                          )
                      )
                )
            )
            t'
    )

harm2 :: SigAU
harm2 =
  boundByCueWithOnset O4 Shy5
    ( \ac onset m t'' ->
        let
          t' = t'' - onset
        in
          atT 0.6
            ( boundPlayer 21.0 \t ->
                pure
                  ( gainT_ "Harm2Gain"
                      ( epwf
                          [ Tuple 0.0 0.0
                          , Tuple 1.0 0.6
                          , Tuple 20.0 0.0
                          ]
                          t
                      )
                      ( ( gain_' "Harm2--Gain"
                            1.0
                            $ playBuf_
                                "Harm2--Play"
                                "harm-2"
                                1.0
                        )
                          :| Nil
                      )
                  )
            )
            t'
    )

landEggTimer :: SigAU
landEggTimer =
  boundByCueWithOnset Land4 And4
    ( \ac onset m t ->
        pure
          $ ( gainT_ "LandEggGain"
                ( epwf
                    [ Tuple 0.0 0.0
                    , Tuple 3.0 1.0
                    ]
                    t
                )
                ( ( gain_' "LandEgg--Gain"
                      1.0
                      $ loopBuf_
                          "LandEgg--Play"
                          "egg-timer-ticking"
                          1.0
                          0.0
                          (0.15 + 0.1 * sin ((t - onset) * pi))
                  )
                    :| Nil
                )
            )
    )

veryFarDrones :: SigAU
veryFarDrones =
  boundByCueWithOnset Ve2 And4
    ( \ac onset m t ->
        let
          time = t - onset

          rad = time * pi
        in
          ( gainT_' "C#CelloLoopGain"
              (if m /= And4 then (epwf [ Tuple 0.0 0.0, Tuple 0.15 0.8, Tuple 10.0 0.8 ] time) else (maybe defaultParam (\andOnset -> (epwf [ Tuple 0.0 0.8, Tuple 0.1 0.1, Tuple 0.2 0.8, Tuple 0.3 0.1, Tuple 0.4 0.8, Tuple 0.5 0.1, Tuple 0.6 0.8, Tuple 0.7 0.1, Tuple 0.8 0.8, Tuple 0.9 0.1, Tuple 1.0 0.8 ] (t - andOnset))) (M.lookup And4 ac.markerOnsets)))
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
                  (gain_' "C#BassGain" (((if m /= And4 then 1.0 else (maybe 1.0 (\andOnset -> max 0.0 (t - andOnset)) (M.lookup And4 ac.markerOnsets))) * (bassDroneVol time))) (loopBuf_ "C#BassLoop" "bass-c-sharp" 0.5 0.0 4.3))
              )
            : Nil
    )

farChimes :: SigAU
farChimes =
  boundByCueWithOnset Far2 And4
    ( \ac onset m t ->
        let
          time = t - onset
        in
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
                            (max 0.0 (0.5 - (time * 0.05)))
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

gongBackwards2 :: Array (Number -> List (AudioUnit D1))
gongBackwards2 = (foldl (\{ acc, t } a -> { acc: [ atT t $ gongBackwards2Atomic (show t) a ] <> acc, t: t + a }) { acc: [], t: 0.0 } [ 0.7, 0.7, 0.7, 0.3, 0.3, 0.3, 1.0, 1.0, 0.7, 0.4, 0.4, 0.4, 1.4, 0.5, 0.5, 0.7, 0.7, 1.0, 1.0 ]).acc

ryGongBackwards :: SigAU
ryGongBackwards =
  boundByCueWithOnset Ry2 And4
    ( \ac onset m t' ->
        let
          t = t' - onset
        in
          pure
            $ pannerMono_ "GongAboveC#Pan" (2.0 * (skewedTriangle01 0.8 2.0 t) - 1.0)
                ( gain_ "GongBwAboveC#Gain"
                    (4.0 * (skewedTriangle01 0.2 10.0 t))
                    (toNel $ fold (map (\f -> f t) gongBackwards2))
                )
    )

airRaidPWF =
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
  ] ::
    Array (Tuple Number Number)

farShriek :: SigAU
farShriek =
  boundByCueWithOnset Far2 And4
    ( \ac onset m t' ->
        let
          t = t' - onset
        in
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
                                      (epwf airRaidPWF t)
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

farBirds :: SigAU
farBirds =
  boundByCueWithOnset Far2 And4
    ( \ac onset m t' ->
        let
          time = t' - onset - 1.0 -- delay by one secod
        in
          boundPlayer (20.0)
            ( \t ->
                pure
                  $ ( gain_' ("BirdsAboveC#Gain")
                        (0.3 * (skewedTriangle01 0.5 10.0 t))
                        ( loopBuf_
                            ("BirdsAboveC#Buf")
                            "beautiful-birds"
                            1.0
                            0.0
                            0.0
                        )
                    )
            )
            time
    )

guitarSingleton :: String -> String -> Number -> Marker -> SigAU
guitarSingleton tag name gain st =
  boundByCueWithOnset st Far3
    ( \ac onset m t' ->
        let
          t = t' - onset
        in
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

snare :: SigAU
snare =
  boundByCue Ver4 And4
    ( \m t ->
        pure
          $ graph_
              "SnareGrpah"
              { aggregators:
                  { out: Tuple (g'add_ "SnareOut") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "SnareCombine") (SLProxy :: SLProxy ("gain" :/ "snare" :/ SNil))
                  , gain: Tuple (g'gain_ "SnareGraphGain" 0.93) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "SnareGraphDelay" (0.31 + 0.02 * sin (pi * t))) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { snare:
                      ( gain_' "SnareGain"
                          0.6
                          ( playBuf_
                              "SnareBuf"
                              "snare-hit"
                              1.0
                          )
                      )
                  }
              }
    )

bassLick :: String -> Number -> Number -> SigAU
bassLick tag os p = bassplz Kings10 This11 os tag 1.0 0.2 0.1 5.0 (lowpass_ "kings-bp" 250.0 10.0) 0.3 false (const $ conv1 p)

thisHeSaidToMeLicks :: String -> Int -> Marker -> Marker -> Array SigAU
thisHeSaidToMeLicks tag n st ed =
  map
    ( \i ->
        let tni = toNumber i in bassplz st ed (tni * 1.6 / tnn) (show i <> tag) (if i < 4 then 0.7 + tni * 0.1 else 1.3 - tni * 0.1) 0.2 0.3 (4.0 + tni * 0.4) identity 0.3 false (const $ conv1 (-4.0) + (conv1 11.0 - conv1 (-4.0)) * tni / (tnn - 1.0))
    )
    (range 0 (n - 1))
  where
  tnn = toNumber n

-- start with c#
secondPartBP =
  [ bassplz Then7 Day7 0.0 "first-C#" 1.0 0.37 0.7 0.0 identity 0.3 false (const $ conv1 0.0)
  -- glitch of c#, smaller
  , bassplz One7 One7 0.1 "C#-echo" 0.5 0.04 0.3 4.0 (highpass_ "glitch-c#" 300.0 1.0) 0.1 false (const $ conv1 0.0)
  -- g#
  , bassplz Day7 One8 0.1 "G#" 1.0 0.27 0.7 0.0 identity 0.3 false (const $ conv1 (-5.0))
  -- e
  , bassplz Day7 One8 0.7 "E" 1.0 0.34 0.7 0.0 identity 0.3 false (const $ conv1 (3.0))
  , bassplz Ma8 Gic8 0.0 "B" 1.0 0.37 0.7 0.0 identity 0.3 false (const $ conv1 (-2.0))
  , bassplz Gic8 Day8 0.0 "C#" 1.0 0.11 0.5 0.0 identity 0.3 false (const $ conv1 (0.0))
  , bassplz Day8 He8 0.0 "D#" 1.0 0.30 0.7 0.0 (lowpass_ "lpD#" 400.0 1.0) 0.3 false (const $ conv1 (2.0))
  -- d# glitch
  , bassplz He8 He8 0.0 "D#-glitch" 0.4 0.06 0.3 0.0 (highpass_ "lpD#" 1000.0 1.0) 0.06 false (const $ conv1 (2.0))
  , bassplz Passed8 Way8 0.0 "D#-bold" 1.0 0.4 0.9 6.0 identity 0.35 false (const $ conv1 (2.0))
  , bassplz My8 My8 0.0 "D#-bkwrd" 0.2 0.1 0.3 6.0 identity 0.35 true (const $ conv1 (2.0))
  , bassplz Way8 Way8 0.0 "A-start" 1.0 0.34 0.5 0.0 identity 0.3 false (const $ conv1 (-4.0))
  , bassplz Way8 Way8 3.2 "D#-wink" 0.7 0.1 0.5 7.0 identity 0.12 false (const $ conv1 (2.0))
  , bassplz Way8 Way8 3.4 "A-wink" 0.7 0.08 0.5 8.0 identity 0.1 true (const $ conv1 (-4.0))
  , bassplz And9 While9 0.0 "C#-awwso" 1.0 0.3 0.7 0.0 identity 0.3 false (const $ conv1 (0.0))
  , bassplz While9 We9 0.0 "D#-awwso" 1.0 0.25 0.6 0.0 identity 0.3 false (const $ conv1 (2.0))
  , bassplz We9 Spoke9 0.0 "A-awwso" 1.0 0.21 0.55 0.0 identity 0.3 false (const $ conv1 (-4.0))
  , bassplz Spoke9 Of9 0.0 "G#-awwso" 1.0 0.43 0.7 0.0 identity 0.3 false (const $ conv1 (-5.0))
  , bassplz Of9 Ma9 0.0 "F#-awwso" 1.0 0.32 0.7 3.45 identity 0.3 false (const $ conv1 (-7.0))
  , bassplz Ma9 Ny9 0.0 "E-mnt" 1.0 0.36 0.7 1.2 identity 0.3 false (const $ conv1 (-9.0))
  , bassplz Ny9 Things9 0.0 "G#-mnt" 1.0 0.45 0.8 5.1 identity 0.3 false \t ->
      let
        st = conv1 (-6.2)

        ed = conv1 (-5.0)
      in
        min ed (st + (ed - st) * t * 3.0)
  , bassplz Things9 Fools10 0.0 "C#-mnt" 1.0 0.6 0.9 8.3 identity 0.3 false \t ->
      let
        st = conv1 (-0.8)

        ed = conv1 (0.0)
      in
        min ed (st + (ed - st) * t * 1.5)
  , bassplz Things9 To11 2.0 "C#-pedal" 2.0 0.9 0.9 0.0 (lowpass_ "many-things-pedal" 300.0 1.0) 0.4 false (const $ conv1 (-12.0))
  , bassplz Fools10 And10 0.0 "fools-F#" 1.0 0.37 0.7 9.0 (bandpass_ "fools-bp" (conv440 44.0) 3.0) 0.3 false (const $ conv1 (-7.0))
  , bassplz And10 Kings10 0.0 "and-C#" 1.0 0.37 0.7 10.0 (bandpass_ "and-bp" (conv440 51.0) 3.0) 0.3 false (const $ conv1 0.0)
  , bassplz Kings10 This11 0.0 "kings-F#" 1.0 0.43 0.7 5.0 (bandpass_ "kings-bp" (conv440 56.0) 3.0) 0.3 false (const $ conv1 5.0)
  -----
  , bassLick "lick-0" 2.7 (-9.0)
  , bassLick "lick-1" 2.9 (-4.0)
  , bassLick "lick-2" 3.1 (-5.0)
  , bassLick "lick-3" 3.3 (1.0)
  , bassLick "lick-4" 3.47 (-6.0)
  , bassLick "lick-5" 3.64 (-7.0)
  , bassLick "lick-6" 3.59 (-1.0)
  , bassLick "lick-7" 3.71 (-2.0)
  , bassLick "lick-8" 3.8 (-8.0)
  ]
    <> thisHeSaidToMeLicks "ladder-0" 4 This11 He11
    <> thisHeSaidToMeLicks "ladder-1-" 5 He11 Said11
    <> thisHeSaidToMeLicks "ladder-2-" 7 Said11 To11
    <> thisHeSaidToMeLicks "ladder-3-" 11 To11 To11 ::
    Array SigAU

bassplz :: Marker -> Marker -> Number -> String -> Number -> Number -> Number -> Number -> (AudioUnit D2 -> AudioUnit D2) -> Number -> Boolean -> (Number -> Number) -> SigAU
bassplz st ed startsAt tag globalGain del gn os filt kink backwards rate =
  boundByCueWithOnset st ed \ac onset m t ->
    let
      time' = t - onset - startsAt
    in
      overZeroPlayer
        ( \time ->
            ( pure $ gain_' (tag <> "bassPlzFader") globalGain
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
        )
        time'

bassGlitch1 = bassGlitch He8 "bg-1" 7.0 :: SigAU

bassGlitch2 = bassGlitch We9 "bg-2" 30.0 :: SigAU

bassGlitch :: Marker -> String -> Number -> SigAU
bassGlitch mk tag os = boundByCue'' mk mk (pure (gain_' (tag <> "bassPlzImpulse") 1.0 $ (playBufWithOffset_ (tag <> "bassPlzBuf") "nasty-bass" 1.0 os)))

bassManyThings :: SigAU
bassManyThings =
  boundByCueWithOnset Ma9 Me11
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure
            ( gain_' "gain-bass-many-things" (if m == Me11 then (maybe 1.0 (\x -> max 0.0 (1.0 - 0.1 * (t - x))) (M.lookup Me11 ac.markerOnsets)) else 1.0)
                ( ( case m of
                      Ma9 -> identity
                      Ny9 -> iirFilter_ "iir-ny9" (0.0050662636 +> 0.0101325272 +> 0.0050662636 +> empty) (1.0632762845 +> -1.9197349456 +> 0.9367237155 +> empty)
                      Things9 -> bandpass_ "bandpass-ma9" (maybe 100.0 (\x -> 100.0 + 500.0 * (t - x)) (M.lookup Things9 ac.markerOnsets)) 5.0
                      Fools10 -> identity
                      And10 -> highpass_ "highpass-and10" 1000.0 3.0
                      Kings10 -> bandpass_ "lowpass-kings10" 140.0 (maybe 0.5 (\x -> max 0.5 $ 12.0 - 3.0 * (t - x)) (M.lookup Kings10 ac.markerOnsets))
                      This11 -> identity
                      He11 -> lowpass_ "lp-he11" 300.0 4.0
                      Said11 -> highpass_ "hp-said11" 1500.0 3.0
                      To11 -> identity
                      Me11 -> lowpass_ "mtf-me11" 80.0 10.0
                      _ -> identity
                  )
                    (playBuf_ "many-things-bass" "nasty-bass" 1.0)
                )
            )
    )

improGlitch :: SigAU
improGlitch =
  boundByCueWithOnset Day7 Turn13
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure (gainT_' "improGlitchGain" (epwf [ Tuple 0.0 1.0, Tuple 5.0 1.0, Tuple 5.04 0.07, Tuple 10.0 0.07, Tuple 13.0 1.0, Tuple 15.0 1.0, Tuple 15.4 0.07, Tuple 20.0 0.07, Tuple 26.0 1.0, Tuple 31.0 1.0, Tuple 31.02 0.07, Tuple 33.0 0.07, Tuple 33.05 1.0, Tuple 33.38 1.0, Tuple 33.44 0.13, Tuple 36.0 0.07, Tuple 40.0 0.13, Tuple 48.1 0.13, Tuple 50.0 0.7, Tuple 54.3 0.7, Tuple 55.0 0.1, Tuple 57.0 0.0, Tuple 63.0 0.0, Tuple 66.0 0.1, Tuple 71.0 0.1, Tuple 71.2 0.0, Tuple 74.0 0.0, Tuple 74.2 0.6, Tuple 74.8 0.2, Tuple 90.0 0.0 ] time) (playBuf_ "improGlitchBuf" "impro-glitch" 1.0))
    )

improWobble :: SigAU
improWobble =
  boundByCueWithOnset Passed8 Turn13
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure (highpassT_ "improWobbleHpf" (epwf [ Tuple 0.0 3000.0, Tuple 5.0 3000.0, Tuple 2000.0 6.0, Tuple 2000.0 11.0, Tuple 1000.0 12.0, Tuple 2000.0 15.0, Tuple 2000.0 21.0, Tuple 2000.0 29.0, Tuple 300.0 30.0, Tuple 2500.0 39.0, Tuple 2500.0 46.0, Tuple 1000.0 49.0, Tuple 3000.0 55.0, Tuple 3000.0 57.0, Tuple 400.0 59.8, Tuple 4000.0 75.0 ] time) (defaultParam { param = 1.0 }) (playBuf_ "improWobbleBuf" "impro-wobbly" 1.0))
    )

improFiligree :: SigAU
improFiligree =
  boundByCueWithOnset Me11 Turn13
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure (gainT_' "improFiligreeGain" (epwf [ Tuple 0.0 0.0, Tuple 1.0 1.0 ] time) (playBufWithOffset_ "improFiligreeBuf" "impro-filigree" 1.0 3.0))
    )

------------------------
compVeryStrangeEnchantedBoy :: Marker -> List (Tuple Number Number)
compVeryStrangeEnchantedBoy Ve1 = t1c440 <$> 54.0 : 58.0 : 61.0 : Nil

compVeryStrangeEnchantedBoy Ry1 = t1c440 <$> 56.0 : 58.0 : 60.0 : Nil

compVeryStrangeEnchantedBoy Strange1 = t1c440 <$> 58.0 : 61.0 : 63.0 : Nil

compVeryStrangeEnchantedBoy En1 = t1c440 <$> 62.0 : 65.0 : 68.0 : 71.0 : Nil

compVeryStrangeEnchantedBoy Chan1 = t1c440 <$> 61.0 : 63.0 : 66.0 : 70.0 : Nil

compVeryStrangeEnchantedBoy Ted1 = t1c440 <$> 60.0 : 62.0 : 66.0 : 69.0 : Nil

-- compVeryStrangeEnchantedBoy Boy1 = t1c440 <$> 59.0 : 63.0 : 65.0 : 68.0 : Nil
compVeryStrangeEnchantedBoy _ = Nil

simpleOsc :: (String -> Number -> AudioUnit D1) -> String -> List (Tuple Number Number) -> AudioUnit D2
simpleOsc f s Nil = zero

simpleOsc f s (h : t) =
  pannerMono_ (s <> "_pmono") 0.0
    ( gain_ (s <> "_gain") 1.0
        ( gain_'
            (s <> "_firstgn")
            (fst h)
            (f (s <> "_firstosc") (snd h))
            :| L.mapWithIndex (\i n -> gain_' (s <> "osc" <> show i) (fst n) $ f (s <> "osc" <> show i) (snd n)) t
        )
    )

sSinOsc :: String -> List (Tuple Number Number) -> AudioUnit D2
sSinOsc = simpleOsc sinOsc_

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

data Marker
  = There0
  | Was0
  | A0
  | Boy0
  | A1
  | Ve1
  | Ry1
  | Strange1
  | En1
  | Chan1
  | Ted1
  | Boy1
  | They2
  | Say2
  | He2
  | Wan2
  | Dered2
  | Ve2
  | Ry2
  | Far2
  | Ve3
  | Ry3
  | Far3
  | O4
  | Ver4
  | Land4
  | And4
  | Sea4
  | A5
  | Lit5
  | Tle5
  | Shy5
  | And5
  | Sad5
  | Of5
  | Eye5
  | But6
  | Ve6
  | Ry6
  | Wise6
  | Was6
  | He6
  | And7
  | Then7
  | One7
  | Day7
  | One8
  | Ma8
  | Gic8
  | Day8
  | He8
  | Passed8
  | My8
  | Way8
  | And9
  | While9
  | We9
  | Spoke9
  | Of9
  | Ma9
  | Ny9
  | Things9
  | Fools10
  | And10
  | Kings10
  | This11
  | He11
  | Said11
  | To11
  | Me11
  | The12
  | Great12
  | Est12
  | Thing12
  | You'll12
  | E12
  | Ver12
  | Learn12
  | Is13
  | Just13
  | To13
  | Love13
  | And13
  | Be13
  | Loved13
  | In13
  | Re13
  | Turn13

m2n :: Marker -> Number
m2n There0 = 0.000000

m2n Was0 = 1.000000

m2n A0 = 2.000000

m2n Boy0 = 3.000000

m2n A1 = 4.000000

m2n Ve1 = 5.000000

m2n Ry1 = 6.000000

m2n Strange1 = 7.000000

m2n En1 = 8.000000

m2n Chan1 = 9.000000

m2n Ted1 = 10.000000

m2n Boy1 = 11.000000

m2n They2 = 12.000000

m2n Say2 = 13.000000

m2n He2 = 14.000000

m2n Wan2 = 15.000000

m2n Dered2 = 16.000000

m2n Ve2 = 17.000000

m2n Ry2 = 18.000000

m2n Far2 = 19.000000

m2n Ve3 = 20.000000

m2n Ry3 = 21.000000

m2n Far3 = 22.000000

m2n O4 = 23.000000

m2n Ver4 = 24.000000

m2n Land4 = 25.000000

m2n And4 = 26.000000

m2n Sea4 = 27.000000

m2n A5 = 28.000000

m2n Lit5 = 29.000000

m2n Tle5 = 30.000000

m2n Shy5 = 31.000000

m2n And5 = 32.000000

m2n Sad5 = 33.000000

m2n Of5 = 34.000000

m2n Eye5 = 35.000000

m2n But6 = 36.000000

m2n Ve6 = 37.000000

m2n Ry6 = 38.000000

m2n Wise6 = 39.000000

m2n Was6 = 40.000000

m2n He6 = 41.000000

m2n And7 = 42.000000

m2n Then7 = 43.000000

m2n One7 = 44.000000

m2n Day7 = 45.000000

m2n One8 = 46.000000

m2n Ma8 = 47.000000

m2n Gic8 = 48.000000

m2n Day8 = 49.000000

m2n He8 = 50.000000

m2n Passed8 = 51.000000

m2n My8 = 52.000000

m2n Way8 = 53.000000

m2n And9 = 54.000000

m2n While9 = 55.000000

m2n We9 = 56.000000

m2n Spoke9 = 57.000000

m2n Of9 = 58.000000

m2n Ma9 = 59.000000

m2n Ny9 = 60.000000

m2n Things9 = 61.000000

m2n Fools10 = 62.000000

m2n And10 = 63.000000

m2n Kings10 = 64.000000

m2n This11 = 65.000000

m2n He11 = 66.000000

m2n Said11 = 67.000000

m2n To11 = 68.000000

m2n Me11 = 69.000000

m2n The12 = 70.000000

m2n Great12 = 71.000000

m2n Est12 = 72.000000

m2n Thing12 = 73.000000

m2n You'll12 = 74.000000

m2n E12 = 75.000000

m2n Ver12 = 76.000000

m2n Learn12 = 77.000000

m2n Is13 = 78.000000

m2n Just13 = 79.000000

m2n To13 = 80.000000

m2n Love13 = 81.000000

m2n And13 = 82.000000

m2n Be13 = 83.000000

m2n Loved13 = 84.000000

m2n In13 = 85.000000

m2n Re13 = 86.000000

m2n Turn13 = 87.000000

m2s :: Marker -> String
m2s There0 = "There"

m2s Was0 = "was"

m2s A0 = "a"

m2s Boy0 = "boy."

m2s A1 = "A"

m2s Ve1 = "ve-"

m2s Ry1 = "-ry"

m2s Strange1 = "strange"

m2s En1 = "en-"

m2s Chan1 = "-chan-"

m2s Ted1 = "-ted"

m2s Boy1 = "boy."

m2s They2 = "They"

m2s Say2 = "say"

m2s He2 = "he"

m2s Wan2 = "wan-"

m2s Dered2 = "-dered"

m2s Ve2 = "ve-"

m2s Ry2 = "-ry"

m2s Far2 = "far..."

m2s Ve3 = "Ve-"

m2s Ry3 = "-ry"

m2s Far3 = "far..."

m2s O4 = "O-"

m2s Ver4 = "-ver"

m2s Land4 = "land"

m2s And4 = "and"

m2s Sea4 = "sea."

m2s A5 = "A"

m2s Lit5 = "lit-"

m2s Tle5 = "-tle"

m2s Shy5 = "shy,"

m2s And5 = "and"

m2s Sad5 = "sad"

m2s Of5 = "of"

m2s Eye5 = "eye,"

m2s But6 = "but"

m2s Ve6 = "ve-"

m2s Ry6 = "-ry"

m2s Wise6 = "wise"

m2s Was6 = "was"

m2s He6 = "he."

m2s And7 = "And"

m2s Then7 = "then"

m2s One7 = "one"

m2s Day7 = "day."

m2s One8 = "One"

m2s Ma8 = "ma-"

m2s Gic8 = "-gic"

m2s Day8 = "day"

m2s He8 = "he"

m2s Passed8 = "passed"

m2s My8 = "my"

m2s Way8 = "way."

m2s And9 = "And"

m2s While9 = "while"

m2s We9 = "we"

m2s Spoke9 = "spoke"

m2s Of9 = "of"

m2s Ma9 = "ma-"

m2s Ny9 = "-ny"

m2s Things9 = "things..."

m2s Fools10 = "Fools"

m2s And10 = "and"

m2s Kings10 = "kings..."

m2s This11 = "This"

m2s He11 = "he"

m2s Said11 = "said"

m2s To11 = "to"

m2s Me11 = "me."

m2s The12 = "The"

m2s Great12 = "great-"

m2s Est12 = "-est"

m2s Thing12 = "thing"

m2s You'll12 = "you'll"

m2s E12 = "e-"

m2s Ver12 = "-ver"

m2s Learn12 = "learn"

m2s Is13 = "is"

m2s Just13 = "just"

m2s To13 = "to"

m2s Love13 = "love"

m2s And13 = "and"

m2s Be13 = "be"

m2s Loved13 = "loved"

m2s In13 = "in"

m2s Re13 = "re-"

m2s Turn13 = "-turn."

backFrom :: Screen -> NatureBoyAccumulator -> NatureBoyAccumulator
backFrom ThereWasABoy = identity

backFrom AVeryStrangeEnchantedBoy = _ { currentScreen = ThereWasABoy }

backFrom TheySayHeWanderedVeryFar = _ { currentScreen = AVeryStrangeEnchantedBoy }

backFrom VeryFarOverLandAndSea = _ { currentScreen = TheySayHeWanderedVeryFar }

backFrom ALittleShyAndSadOfEye = _ { currentScreen = VeryFarOverLandAndSea }

backFrom ButVeryWiseWasHe = _ { currentScreen = ALittleShyAndSadOfEye }

backFrom AndThenOneDay = _ { currentScreen = ButVeryWiseWasHe }

backFrom OneMagicDayHePassedMyWay = _ { currentScreen = AndThenOneDay }

backFrom AndWhileWeSpokeOfManyThings = _ { currentScreen = OneMagicDayHePassedMyWay }

backFrom FoolsAndKingsThisHeSaidToMe = _ { currentScreen = AndWhileWeSpokeOfManyThings }

backFrom TheGreatestGiftYou'llEverLearn = _ { currentScreen = FoolsAndKingsThisHeSaidToMe }

backFrom IsJustToLove = _ { currentScreen = TheGreatestGiftYou'llEverLearn }

backFrom AndBeLovedInReturn = _ { currentScreen = IsJustToLove }

forwardFrom :: Screen -> NatureBoyAccumulator -> NatureBoyAccumulator
forwardFrom ThereWasABoy = _ { currentScreen = AVeryStrangeEnchantedBoy }

forwardFrom AVeryStrangeEnchantedBoy = _ { currentScreen = TheySayHeWanderedVeryFar }

forwardFrom TheySayHeWanderedVeryFar = _ { currentScreen = VeryFarOverLandAndSea }

forwardFrom VeryFarOverLandAndSea = _ { currentScreen = ALittleShyAndSadOfEye }

forwardFrom ALittleShyAndSadOfEye = _ { currentScreen = ButVeryWiseWasHe }

forwardFrom ButVeryWiseWasHe = _ { currentScreen = AndThenOneDay }

forwardFrom AndThenOneDay = _ { currentScreen = OneMagicDayHePassedMyWay }

forwardFrom OneMagicDayHePassedMyWay = _ { currentScreen = AndWhileWeSpokeOfManyThings }

forwardFrom AndWhileWeSpokeOfManyThings = _ { currentScreen = FoolsAndKingsThisHeSaidToMe }

forwardFrom FoolsAndKingsThisHeSaidToMe = _ { currentScreen = TheGreatestGiftYou'llEverLearn }

forwardFrom TheGreatestGiftYou'llEverLearn = _ { currentScreen = IsJustToLove }

forwardFrom IsJustToLove = _ { currentScreen = AndBeLovedInReturn }

forwardFrom AndBeLovedInReturn = identity

data Screen
  = ThereWasABoy
  | AVeryStrangeEnchantedBoy
  | TheySayHeWanderedVeryFar
  | VeryFarOverLandAndSea
  | ALittleShyAndSadOfEye
  | ButVeryWiseWasHe
  | AndThenOneDay
  | OneMagicDayHePassedMyWay
  | AndWhileWeSpokeOfManyThings
  | FoolsAndKingsThisHeSaidToMe
  | TheGreatestGiftYou'llEverLearn
  | IsJustToLove
  | AndBeLovedInReturn

instance eqMarker :: Eq Marker where
  eq a b = eq (m2n a) (m2n b)

instance ordMarker :: Ord Marker where
  compare x y = compare (m2n x) (m2n y)

type NatureBoyAccumulator
  = { initiatedClick :: Boolean
    , curClickId :: Maybe Int
    , mousePosition :: Maybe { x :: Number, y :: Number }
    , currentMarker :: Maybe Marker
    , currentScreen :: Screen
    , markerOnsets :: M.Map Marker Number
    }

inRect :: Point -> Rectangle -> Boolean
inRect p r = p.x >= r.x && p.y >= r.y && p.x <= (r.x + r.width) && p.y <= (r.y + r.height)

doAction :: NatureBoyAccumulator -> Rectangle -> Boolean
doAction acc r = acc.initiatedClick && (maybe false (flip inRect r) acc.mousePosition)

i2rgb :: Int -> Color
i2rgb 0 = rgb 217 131 150

i2rgb 1 = rgb 120 166 164

i2rgb 2 = rgb 116 89 116

i2rgb 3 = rgb 95 94 88

i2rgb 4 = rgb 153 150 165

i2rgb 5 = rgb 216 223 203

i2rgb 6 = rgb 242 215 198

i2rgb 7 = rgb 109 125 123

i2rgb 8 = rgb 224 187 182

i2rgb 9 = rgb 73 166 166

i2rgb _ = rgb 217 131 150

boldItalic :: FontOptions
boldItalic = bold <> italic

screen2markerAccf :: Screen -> Array MarkerAccf
screen2markerAccf screen =
  ( \x ->
      Tuple x
        ( \t ac ->
            ac
              { currentMarker = Just x
              , markerOnsets = (M.insert x t ac.markerOnsets)
              }
        )
  )
    <$> go screen
  where
  go ThereWasABoy = [ There0, Was0, A0, Boy0 ]

  go AVeryStrangeEnchantedBoy =
    [ A1
    , Ve1
    , Ry1
    , Strange1
    , En1
    , Chan1
    , Ted1
    , Boy1
    ]

  go TheySayHeWanderedVeryFar =
    [ They2
    , Say2
    , He2
    , Wan2
    , Dered2
    , Ve2
    , Ry2
    , Far2
    ]

  go VeryFarOverLandAndSea =
    [ Ve3
    , Ry3
    , Far3
    , O4
    , Ver4
    , Land4
    , And4
    , Sea4
    ]

  go ALittleShyAndSadOfEye =
    [ A5
    , Lit5
    , Tle5
    , Shy5
    , And5
    , Sad5
    , Of5
    , Eye5
    ]

  go ButVeryWiseWasHe =
    [ But6
    , Ve6
    , Ry6
    , Wise6
    , Was6
    , He6
    ]

  go AndThenOneDay =
    [ And7
    , Then7
    , One7
    , Day7
    ]

  go OneMagicDayHePassedMyWay =
    [ One8
    , Ma8
    , Gic8
    , Day8
    , He8
    , Passed8
    , My8
    , Way8
    ]

  go AndWhileWeSpokeOfManyThings =
    [ And9
    , While9
    , We9
    , Spoke9
    , Of9
    , Ma9
    , Ny9
    , Things9
    ]

  go FoolsAndKingsThisHeSaidToMe =
    [ Fools10
    , And10
    , Kings10
    , This11
    , He11
    , Said11
    , To11
    , Me11
    ]

  go TheGreatestGiftYou'llEverLearn =
    [ The12
    , Great12
    , Est12
    , Thing12
    , You'll12
    , E12
    , Ver12
    , Learn12
    ]

  go IsJustToLove =
    [ Is13
    , Just13
    , To13
    , Love13
    ]

  go AndBeLovedInReturn =
    [ And13
    , Be13
    , Loved13
    , In13
    , Re13
    , Turn13
    ]

type MarkerAccf
  = Tuple Marker (Number -> NatureBoyAccumulator -> NatureBoyAccumulator)

makeCanvas :: CanvasInfo -> Number -> Array MarkerAccf -> (NatureBoyAccumulator -> NatureBoyAccumulator) -> (NatureBoyAccumulator -> NatureBoyAccumulator) -> NatureBoyAccumulator -> Tuple NatureBoyAccumulator Drawing
makeCanvas (CanvasInfo ci) time pads backAction forwardAction acc =
  let
    lpads = length pads

    nrows = (lpads + 1) `div` 2

    nrowsFloat = toNumber nrows

    squares =
      mapWithIndex
        ( \i a ->
            Tuple
              { x: if i `mod` 2 == 0 then 0.0 else ci.w / 2.0
              , y: (toNumber (i `div` 2)) * 3.0 * ci.h / (nrowsFloat * 4.0)
              , height: 3.0 * ci.h / (4.0 * nrowsFloat)
              , width: if i == (lpads - 1) && i `mod` 2 == 1 then ci.w else ci.w / 2.0
              }
              a
        )
        pads

    backRect = { x: 0.0, y: 3.0 * ci.h / 4.0, height: ci.h / 4.0, width: ci.w / 4.0 }

    forwardRect = { x: ci.w / 4.0, y: 3.0 * ci.h / 4.0, height: ci.h / 4.0, width: 3.0 * ci.w / 4.0 }
  in
    Tuple
      ( foldl (\newAcc tpl -> if doAction newAcc (fst tpl) then ((snd <<< snd) tpl time newAcc) else newAcc)
          ( if doAction acc backRect then
              backAction acc
            else
              if doAction acc forwardRect then forwardAction acc else acc
          )
          squares
      )
      ( ( fold
            $ mapWithIndex
                ( \i s ->
                    let
                      coords = fst s

                      mkr = ((fst <<< snd) s)
                    in
                      filled (fillColor (if acc.currentMarker == Just mkr then (rgb 240 240 240) else i2rgb i)) (rectangle coords.x coords.y coords.width coords.height)
                        <> ( text
                              (font sansSerif 16 (if mkr >= The12 then boldItalic else bold))
                              (coords.x + 10.0)
                              (coords.y + (coords.height / 2.0))
                              (fillColor (rgb 0 0 0))
                              (m2s mkr)
                          )
                )
                squares
        )
          <> filled (fillColor (rgb 48 55 46)) (rectangle backRect.x backRect.y backRect.width backRect.height)
          <> filled (fillColor (rgb 1 17 2)) (rectangle forwardRect.x forwardRect.y forwardRect.width forwardRect.height)
      )

veryStrangeEnchantedBoyComp :: SigAU
veryStrangeEnchantedBoyComp ac cm _ =
  Tuple ac
    ( pure
        $ ( simpleOsc (\s n -> periodicOsc_ ("oscVeryStrangeEnchantedBoyComp" <> s) "smooth" n) (m2s cm) (compVeryStrangeEnchantedBoy cm)
              * audioWorkletProcessor_ "gateVeryStrangeEnchantedBoyComp"
                  "klank-amplitude"
                  O.empty
                  (pmic "gatingSignalVeryStrangeEnchantedBoyComp")
          )
    )

type Filt
  = Number -> AudioUnit D2 -> AudioUnit D2

secondPartVocalRig :: String -> Marker -> Marker -> Filt -> SigAU
secondPartVocalRig tag st ed filt =
  boundByCueWithOnset st ed
    ( \ac onset m t ->
        pure
          $ graph_ (tag <> "SecondPartVocalsGraph")
              { aggregators:
                  { out: Tuple (g'add_ (tag <> "SecondPartVocalsOut")) (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ (tag <> "SecondPartVocalsCombine")) (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ (tag <> "SecondPartVocalsGain") 0.2) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ (tag <> "SecondPartVocalsDelay") 0.4) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: boundByCueNac''' st st ((filt $ t - onset) (pmic (tag <> "SecondPartVocalsMic"))) m
                  }
              }
    )

filtLtoFiltF :: Number -> Array (AudioUnit D2 -> AudioUnit D2) -> Filt
filtLtoFiltF len arr t = fromMaybe identity (arr `index` floor ((t % (len * ((toNumber <<< length) arr))) / len))

then7Filt :: Filt
then7Filt t =
  filtLtoFiltF 0.27
    [ lowshelf_ "lowshelfThen7" 818.0 7.0
    , bandpass_ "bandpassThen7" 320.0 0.0
    , notch_ "notchThen7" 320.0 0.0
    , highpass_ "highpassThen7" 2000.0 7.0
    , lowpass_ "lowpassThen7" 100.0 7.0
    , allpass_ "allpassThen7" (200.0 + ((t % 0.27) * 3000.0 / 0.27)) 6.0
    ]
    t

passed8Filt :: Filt
passed8Filt =
  filtLtoFiltF 0.195
    [ bandpass_ "bandpassPassed8" 900.0 0.0
    , highpass_ "highpassPassed8" 3000.0 7.0
    , notch_ "notchPassed8" 120.0 0.0
    , lowpass_ "lowpassPassed8" 100.0 7.0
    , lowshelf_ "lowshelfPassed8" 1818.0 7.0
    ]

and13Filt :: Filt
and13Filt =
  filtLtoFiltF 0.14
    [ bandpass_ "bandpassPassed8" 900.0 0.0
    , highpass_ "highpassPassed8" 3000.0 7.0
    , lowpass_ "lowpassPassed8" 100.0 7.0
    ]

foolsFilt :: Filt
foolsFilt =
  filtLtoFiltF 0.195
    [ bandpass_ "fools-0-filt" 200.0 7.0
    , bandpass_ "fools-1-filt" 600.0 7.0
    , bandpass_ "fools-2-filt" 1200.0 7.0
    , bandpass_ "fools-3-filt" 1500.0 7.0
    ]

foolsVoice :: SigAU
foolsVoice = boundByCueWithOnset Fools10 Fools10 \_ onset _ t -> let time = t - onset in pure (foolsFilt time (pmic "foolsMic"))

and13Voice :: SigAU
and13Voice = boundByCueWithOnset And13 And13 \_ onset _ t -> let time = t - onset in pure (and13Filt time (pmic "and13Mic"))

secondPartVocalsUsingRig =
  [ secondPartVocalRig "then-7-voice" Then7 One7 then7Filt
  , secondPartVocalRig "one-7-voice" One7 Day7
      ( const
          $ genericFB "one-7-outter" 0.07 0.3
          <<< genericFB "one-7-middle" 0.1 0.3
          <<< genericFB "one-7-inner" 0.05 0.13
      )
  , secondPartVocalRig "day-7-voice" Day7 One8 (\t -> genericFB "day-7-outter" 0.4 0.6 <<< waveShaper_ "wave-shaper-day" "wicked" (if t % 1.0 < 0.5 then TwoX else FourX))
  , secondPartVocalRig "one-8-voice" One8 Ma8
      ( const
          $ genericFB "one-8-outter" 0.2 0.4
          <<< genericFB "one-9-middle" 0.14 0.3
      )
  , secondPartVocalRig "ma-8-voice" Ma8 Gic8 (\t -> gain_' "ma-8-gain" (0.55 + 0.4 * sin (pi * t * 7.0)))
  , secondPartVocalRig "gic-8-voice" Gic8 Day8 (\t -> highpass_ "gic-8-hp" (max 300.0 $ 4000.0 - (t * pi * 4000.0)) 10.4)
  , secondPartVocalRig "day-8-voice" Day8 He8 (\t -> lowpass_ "gic-8-hp" (400.0 + (-350.0) * cos (t * pi * 3.0)) 10.4)
  , secondPartVocalRig "he-8-voice" He8 Passed8 (const identity)
  , secondPartVocalRig "passed-7-voice" Passed8 My8 passed8Filt
  , secondPartVocalRig "my-8-voice" My8 Way8
      ( const
          $ genericFB "my-8-outter" 0.07 0.3
          <<< genericFB "my-8-middle" 0.1 0.3
          <<< genericFB "my-8-inner" 0.05 0.13
      )
  , secondPartVocalRig "way-8-voice" Way8 And9 (\t -> genericFB "way-8-outter" 0.4 0.6 <<< waveShaper_ "wave-shaper-way" "wicked" (if t % 0.36 < 0.18 then TwoX else FourX))
  , secondPartVocalRig "and-9-voice" And9 While9 (const $ genericFB "and-9-outter" 0.26 0.7)
  , secondPartVocalRig "while-9-voice" While9 We9 (const $ genericFB "while-9-outter" 0.21 0.6)
  , secondPartVocalRig "we-9-voice" We9 Spoke9 (const $ genericFB "we-9-outter" 0.15 0.5)
  , secondPartVocalRig "spoke-9-voice" Spoke9 Of9 (const $ genericFB "spoke-9-outter" 0.1 0.4)
  , secondPartVocalRig "of-9-voice" Of9 Ma9 (const $ genericFB "of-9-outter" 0.06 0.3)
  ] ::
    Array SigAU

maVoice :: SigAU
maVoice =
  boundByCueWithOnset Ma9 Ny9 \_ onset _ t' ->
    let
      t = t' - onset
    in
      pure
        $ ( dup2_ "pmicdupMaVoice" (pmic "maVoiceBase") \m ->
              gain_ "maHarmDelAdder" 1.0
                ( harmDel "ma-0" (conv1 3.0 - 1.0) 0.5 t m
                    :| harmDel "ma-1" (conv1 5.0 - 1.0) 0.4 t m
                    : harmDel "ma-2" (conv1 8.0 - 1.0) 0.45 t m
                    : m
                    : Nil
                )
          )

nyOrAndVoice :: String -> Marker -> Marker -> SigAU
nyOrAndVoice tag st ed =
  boundByCue st ed
    ( \m t ->
        pure
          $ graph_ (tag <> "NyOrAndGraph")
              { aggregators:
                  { out: Tuple (g'add_ (tag <> "NyOrAndOut")) (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ (tag <> "NyOrAndCombine")) (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ (tag <> "NyOrAndGain") 0.7) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ (tag <> "NyOrAndDelay") 0.29) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: boundByCueNac''' st st (pmic (tag <> "NyOrAndMic")) m
                  }
              }
    )

ny9Voice = nyOrAndVoice "ny9" Ny9 Things9 :: SigAU

and10Voice = nyOrAndVoice "and10" And10 Kings10 :: SigAU

thingsPwfVoice =
  [ Tuple 0.0 1.0
  , Tuple 1.0 1.0
  , Tuple 1.06 0.0
  , Tuple 1.20 0.0
  , Tuple 1.25 1.0
  , Tuple 2.1 1.0
  , Tuple 2.16 0.0
  , Tuple 2.35 0.0
  , Tuple 2.41 1.0
  , Tuple 3.05 1.0
  , Tuple 3.12 0.0
  , Tuple 3.2 0.0
  , Tuple 3.24 1.0
  ] ::
    Array (Tuple Number Number)

thingsPwfOsc = over (traversed <<< _2) (\n -> 0.3 * (1.0 - n)) thingsPwfVoice :: Array (Tuple Number Number)

thingsVoice :: SigAU
thingsVoice =
  boundByCueWithOnset Things9 Fools10
    ( \_ onset m t ->
        let
          time = t - onset
        in
          pure
            $ ( gain_ "ThingsGlobalFader" 1.0
                  ( graph_ "Things9Graph"
                      { aggregators:
                          { out: Tuple (g'add_ "Things9Out") (SLProxy :: SLProxy ("combine" :/ SNil))
                          , combine: Tuple (g'add_ "Things9Combine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                          , gain: Tuple (g'gain_ "Things9Gain" 0.7) (SLProxy :: SLProxy ("del" :/ SNil))
                          }
                      , processors:
                          { del: Tuple (g'delay_ "Things9Delay" 0.45) (SProxy :: SProxy "combine")
                          }
                      , generators:
                          { mic:
                              boundByCueNac''' Things9 Things9
                                ( gainT_' ("Things9Voilent")
                                    (epwf thingsPwfVoice time)
                                    (pmic "Things9Mic")
                                )
                                m
                          }
                      }
                      :| ( pannerMono_ "poscThingsPan" 0.0
                            $ gainT_' ("poscThingsGain")
                                (epwf thingsPwfOsc time)
                                (periodicOsc_ "thingsPosc" "rich" (conv440 60.0))
                        )
                      : Nil
                  )
              )
    )

peakingKings :: String -> Number -> Number -> Number -> AudioUnit D2 -> AudioUnit D2
peakingKings tag freq rate t = peaking_ (tag <> "peakingKings") freq (5.2 + (-5.0) * cos (rate * pi * t)) 0.0

peakingKingsList :: List (Tuple Number Number)
peakingKingsList =
  Tuple (conv440 (-2.0)) 1.0
    : Tuple (conv440 10.0) 1.1
    : Tuple (conv440 17.0) 1.2
    : Tuple (conv440 22.0) 1.3
    : Tuple (conv440 36.0) 1.4
    : Nil

kingsVoice :: SigAU
kingsVoice =
  secondPartVocalRig "kings-10-voice" Kings10 This11
    ( \t u ->
        dup2_ "kings-dup" u
          ( \m ->
              gain_ "kings-dup-gain" 0.2
                $ toNel
                    ( map (\(Tuple f r) -> peakingKings (show f) f r t m)
                        peakingKingsList
                    )
          )
    )

thisHeSaidTo :: SigAU
thisHeSaidTo =
  boundByCueWithOnset This11 To11
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure $ dup2 (pmic "thisHeSaidToPmic") \d -> (gainT_' "thisHeSaidToDry" (epwf [ Tuple 0.0 0.0, Tuple 4.0 1.0 ] time) d + (gainT_' "thisHeSaidToWeT" (epwf [ Tuple 0.0 1.0, Tuple 4.0 0.0 ] time) $ convolver_ "veryWiseWasHeVoiceConvolver" "matrix-verb-5" d))
    )

slowDrumSecondPart :: SigAU
slowDrumSecondPart =
  boundByCueWithOnset Then7 Passed8
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure (gain_' "slowDrumGain" (max 0.0 (0.8 - (time * 0.1))) (lowpass_ "then-one-day-lp" 170.0 5.0 (playBuf_ "slowDrumBuff" "slow-drum-pattern" (0.8 - ((time * 0.2) % 0.1)))))
    )

thenOneDayOneMagicDayHePassedMyWayDrums :: SigAU
thenOneDayOneMagicDayHePassedMyWayDrums =
  boundByCueWithOnset Then7 Of9
    ( \ac onset m t ->
        let
          time = t - onset

          left = 30.0 + 30.0 * sin ((if m >= While9 then 10.0 else 0.2) * time * pi)

          right = min 78.0 ((if m >= While9 then 0.5 else 4.4) + (if m >= While9 then 0.3 else 4.0) * sin (if m >= While9 then 12.0 else 0.35 * time * pi) + left)
        in
          pure (gain_' "glitchDrumGain" (min 1.0 (time * 0.15)) (loopBuf_ "glitchDrumBuf" "drumz-cat-55" 1.0 left right))
    )

theGreatestThingDrumOutro :: SigAU
theGreatestThingDrumOutro =
  boundByCueWithOnset The12 Re13
    ( \ac onset m t ->
        let
          time = t - onset
        in
          pure (gain_' "glitchDrumGain" (max 0.0 (1.0 - time * 0.1)) (lowpass_ "lpglitchdrum" 70.0 5.0 (waveShaper_ "ws-drum" "wicked" FourX (loopBuf_ "glitchDrumBuf" "drumz-cat-55" 1.0 0.0 0.0))))
    )

data ManyThingsDrumMachine
  = Cat55
  | Cat80
  | Cat100
  | Cat110
  | Cat160

-- offset l dm
data TOD
  = TOD Number Number (Maybe (Tuple ManyThingsDrumMachine (AudioUnit D2 -> AudioUnit D2)))

makeDrumMachine :: Marker -> Array TOD -> SigAU
makeDrumMachine mk atod' =
  boundByCueWithOnset mk mk
    ( \ac onset m t ->
        let
          time = t - onset
        in
          fold (map (\f -> f time) bps)
    )
  where
  atod = (foldl (\{ acc, clen } (TOD offset len dm) -> { acc: acc <> [ Tuple (len + clen) (TOD offset len dm) ], clen: len + clen }) { acc: [], clen: 0.0 } atod').acc

  bps = mapWithIndex (\i (Tuple loc (TOD offset len dm')) -> (maybe (const Nil) \(Tuple dm hpf) -> let preface = m2s mk <> mt2s dm <> show i in atT loc $ boundPlayer (len + 0.06) (\tnow -> pure $ gainT_' (preface <> "mdm-gain") (epwf [ Tuple 0.0 1.0, Tuple len 1.0, Tuple (len + 0.03) 0.0 ] tnow) (hpf $ playBufWithOffset_ (preface <> "mdm-buf") (mt2s dm) 1.0 offset))) dm') atod

mt2s :: ManyThingsDrumMachine -> String
mt2s Cat55 = "drumz-cat-55"

mt2s Cat80 = "drumz-cat-80"

mt2s Cat100 = "drumz-cat-100"

mt2s Cat110 = "drumz-cat-110"

mt2s Cat160 = "drumz-cat-160"

maDrumz =
  makeDrumMachine Ma9
    [ TOD 20.0 0.41 (Just $ Tuple Cat55 identity)
    , TOD 0.0 3.0 (Just $ Tuple Cat160 identity) -- will spill over
    ] ::
    SigAU

nyDrumz =
  makeDrumMachine Ny9
    [ TOD 10.0 0.35 (Just $ Tuple Cat80 identity)
    , TOD 8.0 3.0 (Just $ Tuple Cat100 identity) -- will spill over
    ] ::
    SigAU

thingsDrumz =
  makeDrumMachine Things9
    [ TOD 20.0 0.41 (Just $ Tuple Cat55 identity)
    , TOD 0.0 0.2 (Just $ Tuple Cat160 identity)
    , TOD 1.0 0.4 (Just $ Tuple Cat160 identity)
    , TOD 3.0 0.15 (Just $ Tuple Cat160 identity)
    , TOD 3.0 0.35 (Just $ Tuple Cat55 identity)
    , TOD 3.0 0.3 (Just $ Tuple Cat100 identity)
    , TOD 3.0 0.26 (Just $ Tuple Cat110 identity)
    , TOD 6.0 0.55 (Just $ Tuple Cat100 identity)
    , TOD 0.0 0.2 (Just $ Tuple Cat160 identity)
    , TOD 10.0 0.9 (Nothing)
    , TOD 5.0 0.35 (Just $ Tuple Cat110 identity)
    , TOD 7.0 1.5 (Just $ Tuple Cat80 identity)
    , TOD 3.0 0.4 (Just $ Tuple Cat110 identity)
    , TOD 7.0 0.3 (Just $ Tuple Cat80 identity)
    , TOD 0.0 0.3 (Just $ Tuple Cat160 identity)
    , TOD 20.0 0.56 (Just $ Tuple Cat55 identity)
    , TOD 0.0 0.2 (Nothing)
    , TOD 20.0 0.42 (Just $ Tuple Cat55 identity)
    , TOD 0.0 0.1 (Just $ Tuple Cat110 identity)
    , TOD 20.0 0.42 (Nothing)
    , TOD 7.0 4.0 (Just $ Tuple Cat80 identity)
    ] ::
    SigAU

foolsDrumz =
  makeDrumMachine Fools10
    [ TOD 20.0 0.9 (Nothing)
    , TOD 0.0 0.15 (Just $ Tuple Cat160 identity)
    , TOD 1.0 4.0 (Nothing)
    ] ::
    SigAU

andDrumz =
  makeDrumMachine And10
    [ TOD 0.0 0.2 (Just $ Tuple Cat55 identity)
    , TOD 0.0 0.2 (Just $ Tuple Cat80 identity)
    , TOD 0.0 0.2 (Just $ Tuple Cat100 identity)
    , TOD 0.0 0.2 (Just $ Tuple Cat110 identity)
    , TOD 0.0 3.0 (Just $ Tuple Cat160 identity)
    ] ::
    SigAU

kingsDrumz =
  makeDrumMachine Kings10
    [ TOD 0.0 1.2 (Just $ Tuple Cat80 identity)
    , TOD 0.0 0.4 (Just $ Tuple Cat80 identity)
    , TOD 0.0 0.6 (Just $ Tuple Cat80 identity)
    , TOD 0.0 2.3 (Just $ Tuple Cat80 identity)
    , TOD 0.0 0.4 (Just $ Tuple Cat80 identity)
    , TOD 0.0 10.0 (Just $ Tuple Cat55 identity)
    ] ::
    SigAU

thisDrumz =
  makeDrumMachine This11
    [ TOD 0.0 10.0 (Just $ Tuple Cat55 (highpass_ "thisDrumzHpf" 700.0 8.0))
    ] ::
    SigAU

heDrumz =
  makeDrumMachine He11
    [ TOD 20.0 10.0 (Just $ Tuple Cat55 (highpass_ "heDrumzHpf" 1400.0 8.0))
    ] ::
    SigAU

saidDrumz =
  makeDrumMachine Said11
    [ TOD 15.0 10.0 (Just $ Tuple Cat55 (highpass_ "saidDrumzHpf" 2100.0 8.0))
    ] ::
    SigAU

toDrumz =
  makeDrumMachine To11
    [ TOD 12.0 10.0 (Just $ Tuple Cat110 (highpass_ "toDrumzHpf" 3000.0 8.0))
    ] ::
    SigAU

manyThingsFoolsAndKingsThisHeSaidToMeDrumz :: Array SigAU
manyThingsFoolsAndKingsThisHeSaidToMeDrumz = [ maDrumz, nyDrumz, thingsDrumz, foolsDrumz, andDrumz, kingsDrumz, thisDrumz, heDrumz, saidDrumz, toDrumz ]

natureBoy =
  [ there0
  , was0
  , a0
  , boy0
  , a1
  , celloVeryStrangeEnchantedDrone
  , veryStrangeEnchantedBoyComp
  , veRyStrangeEn
  , chanTed
  , boy1
  , they2
  , sayHeWandered
  , theyGong
  , sayGong
  , sayPad
  , heGong
  , hePad
  , wanGong
  , wanPad
  , deredGong
  , deredPad
  , veRyPad
  , veRy2
  , far2
  , veryFarDrones
  , ryGongBackwards
  , farChimes
  , farShriek
  , farBirds
  , harm0
  , veRy3
  , far3
  , harm1
  , guitarSingleton "a" "middle-g-sharp-guitar" 0.5 Ve3
  , guitarSingleton "b" "e-guitar" 0.3 Ry3
  , harm2
  , overLandAnd
  , snare
  , landEggTimer
  , seaVoice
  , preALittleShyAccomp
  , aLittleShyAccomp
  , andSadOfEyeAccomp
  , butVeryWiseWasAccomp
  , heAccomp
  , littleShyHigh
  , aVoicePedal
  , littleShyVoice
  , andVoice
  , sadOfEyeVoice
  , sadOfEyeHigh
  , butPedalVoice
  , veryWiseWasHeVoice
  , veryWiseWasHeHigh
  , veryWiseWasHeWahs
  , veryWiseWasBassoon
  , veryWiseWasSkiddaw
  , wasHeGlitches
  , planeLanding
  , heRichSwell
  , scratchySwellHe
  , wiseWasHeAndClock
  ----------- pt 2
  , andPt2Voice
  , slowDrumSecondPart
  , thenOneDayOneMagicDayHePassedMyWayDrums
  , bassGlitch1
  , bassGlitch2
  , bassManyThings
  , improGlitch
  , improWobble
  , improFiligree
  , maVoice
  , ny9Voice
  , thingsVoice
  , foolsVoice
  , and10Voice
  , kingsVoice
  , thisHeSaidTo
  , meBeforeGreatest
  , theGreatestThingDrumOutro
  , theGreatestThingYoullEverLearnIsJustToLove
  , and13Voice
  , beLovedInReturn
  ]
    <> theySayHeWanderedBuildup
    <> secondPartBP
    <> secondPartVocalsUsingRig
    <> manyThingsFoolsAndKingsThisHeSaidToMeDrumz ::
    Array SigAU

scene :: Interactions -> NatureBoyAccumulator -> CanvasInfo -> Number -> Behavior (AV D2 NatureBoyAccumulator)
scene inter acc' ci'@(CanvasInfo ci) time = go <$> (interactionLog inter)
  where
  go p =
    AV
      ( Just
          ( speaker (toNel players.aus)
          )
      )
      (Just (snd cvs))
      players.audAcc
    where
    acc =
      acc'
        { mousePosition =
          ( \{ x, y } ->
              { x: x - ci.boundingClientRect.x, y: y - ci.boundingClientRect.y
              }
          )
            <$> head p
        , initiatedClick = (_.id <$> head p) /= acc'.curClickId
        , curClickId = _.id <$> head p
        }

    curScreen = screen2markerAccf acc.currentScreen

    cvs = makeCanvas ci' time curScreen (backFrom acc.currentScreen) (forwardFrom acc.currentScreen) acc

    vizAcc = fst cvs

    initialV = { aus: Nil, audAcc: vizAcc }

    players =
      maybe initialV
        ( \mk ->
            foldl (\{ aus, audAcc } f -> let (Tuple ak au) = f audAcc mk time in { audAcc: ak, aus: au <> aus }) initialV natureBoy
        )
        acc.currentMarker

makeDistortionCurve :: Number -> Array Number
makeDistortionCurve k =
  map
    ( \i ->
        let
          x = (toNumber i * 2.0 / toNumber n_samples) - 1.0
        in
          (3.0 + k) * x * 20.0 * deg / (pi + (k * abs x))
    )
    (range 0 $ n_samples - 1)
  where
  n_samples = 44100

  deg = pi / 180.0

main :: Klank' NatureBoyAccumulator
main =
  klank
    { run =
      runInBrowser_ do
        inter <- getInteractivity
        pure $ scene inter
    , accumulator =
      \res _ ->
        res
          { initiatedClick: false
          , curClickId: Nothing
          , mousePosition: Nothing
          , currentMarker: Nothing
          , currentScreen: ThereWasABoy
          , markerOnsets: M.empty
          }
    , exporter = defaultExporter
    , enableMicrophone = true
    , worklets =
      \_ res rej ->
        res
          [ "https://klank-share.s3.eu-west-1.amazonaws.com/K16050057737584320.js" -- smoothing amplitude tracker
          ]
    , floatArrays =
      const
        $ affable
            ( do
                wicked <- liftEffect $ makeFloatArray (makeDistortionCurve 400.0)
                pure $ O.singleton "wicked" wicked
            )
    -- All sounds from freesound.org are used with the CC attribution license.
    -- For attribution information, see the links of the files themselves, all of which link to information about the file and author.
    , buffers =
      makeBuffersKeepingCache
        -- drones
        [ Tuple "low-g#" "https://freesound.org/data/previews/195/195285_3623377-hq.mp3"
        -- impros
        , Tuple "flute" "https://media.graphcms.com/eiKfSNIbSaiomCZQzXGA"
        -- siren
        , Tuple "siren" "https://freesound.org/data/previews/534/534550_11837619-hq.mp3"
        -- revcym
        , Tuple "revcym" " https://freesound.org/data/previews/240/240712_3552082-hq.mp3"
        -- drumz
        , Tuple "slow-drum-pattern" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/stonerRock.ogg"
        , Tuple "drumz-cat-55" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/drumz-catCrabs55.ogg"
        , Tuple "drumz-cat-80" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/drumz-catCrabs80.ogg"
        , Tuple "drumz-cat-100" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/drumz-catCrabs100.ogg"
        , Tuple "drumz-cat-110" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/drumz-catCrabs110.ogg"
        , Tuple "drumz-cat-160" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/drumz-catCrabs160.ogg"
        -- gamelan
        , Tuple "kettle-g-sharp-3" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/kettleGSharp3.ogg"
        , Tuple "kettle-a-3" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/kettleA3.ogg"
        , Tuple "kettle-c-4" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/kettleC4.ogg"
        , Tuple "kettle-e-flat-4" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/kettleEFlat4.ogg"
        , Tuple "kettle-f-sharp-4" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/kettleFSharp4.ogg"
        -- impulses
        , Tuple "matrix-verb-3" "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/Impulses/matrix-reverb3.wav"
        , Tuple "matrix-verb-3" "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/Impulses/matrix-reverb3.wav"
        , Tuple "matrix-verb-5" "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/Impulses/matrix-reverb5.wav"
        -- harmonic stuff
        ------- interj
        , Tuple "g-sharp-a-sharp-high" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/gSharpASharpInterjection.ogg"
        , Tuple "c-sharp-d-sharp-high" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/cSharpDSharpInterjection.ogg"
        , Tuple "g-a-high" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/gAInterjection.ogg"
        --------------- low d
        , Tuple "bassoon-low-d" "https://freesound.org/data/previews/154/154331_2626346-hq.mp3"
        , Tuple "low-guitar-d" "https://freesound.org/data/previews/117/117675_646701-hq.mp3"
        , Tuple "skiddaw-low-d" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/skiddawLowD.ogg"
        --------------- harm
        , Tuple "harm-0" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/cSharpMinorPad.ogg"
        , Tuple "harm-0-120" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/cSharpMinorPad120.ogg"
        , Tuple "harm-0-110" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/cSharpMinorPad110.ogg"
        , Tuple "harm-0-90" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/cSharpMinorPad90.ogg"
        , Tuple "harm-0-70" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/cSharpMinorPad70.ogg"
        , Tuple "harm-0-40" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/cSharpMinorPad40.ogg"
        , Tuple "harm-1" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/fSharpDiadPad.ogg"
        , Tuple "harm-1-120" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/fSharpDiadPad120.ogg"
        , Tuple "harm-1-50" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/fSharpDiadPad50.ogg"
        , Tuple "harm-1-90" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/fSharpDiadPad90.ogg"
        , Tuple "harm-2" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/cSharpDSharpDiadPad100.ogg"
        , Tuple "pre-a-little-shy" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/preALittleShy.ogg"
        , Tuple "a-little-shy" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/aLittleShy.ogg"
        , Tuple "and-sad-of-eye" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/andSadOfEye.ogg"
        , Tuple "but-very-wise-was" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/butVeryWiseWas.ogg"
        , Tuple "he" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/he.ogg"
        -- bass
        , Tuple "nasty-bass" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/nastyBass.ogg"
        -- snare
        , Tuple "snare-hit" "https://freesound.org/data/previews/100/100393_377011-hq.mp3"
        -- foghorns
        -- low, grave
        -- , Tuple "distant-low-blast" "https://freesound.org/data/previews/500/500146_401348-hq.mp3"
        -- nasty, clippy, beautiful
        -- , Tuple "nasty-rich-low" "https://freesound.org/data/previews/234/234681_1708550-hq.mp3"
        -- bold, straight
        -- , Tuple "bold-straight-pvc" "https://freesound.org/data/previews/507/507472_2977885-hq.mp3"
        -- single low-ish blast
        -- , Tuple "single-clear-blast" "https://freesound.org/data/previews/81/81874_1285056-hq.mp3"
        -- higher
        -- , Tuple "higher-fog-horn" "https://freesound.org/data/previews/92/92911_37876-hq.mp3"
        --------------------------------
        -- harmonic rising, glitchy
        -- , Tuple "rising-harm" "https://freesound.org/data/previews/430/430865_45576-hq.mp3"
        -- , Tuple "sitar" "https://freesound.org/data/previews/37/37715_347704-hq.mp3"
        ----------- pads
        -- , Tuple "twisty-pad" "https://freesound.org/data/previews/33/33183_250881-hq.mp3"
        --, Tuple "evolving" "https://freesound.org/data/previews/484/484850_16058-hq.mp3"
        -- yesHe
        , Tuple "warble" "https://freesound.org/data/previews/110/110212_1751865-hq.mp3"
        -- yesHe
        , Tuple "to-the-heavens" "https://freesound.org/data/previews/110/110211_1751865-hq.mp3"
        --, Tuple "low-energized" "https://freesound.org/data/previews/33/33182_250881-hq.mp3"
        --, Tuple "ethereal" "https://freesound.org/data/previews/352/352944_6523136-hq.mp3"
        ------------------------- THIS IS OUR BASS SOUND
        ------------------------- FOR THE SECOND PART
        , Tuple "bassplz" "https://freesound.org/data/previews/119/119059_181941-hq.mp3"
        , Tuple "scratchy-swell" "https://freesound.org/data/previews/417/417416_1453392-hq.mp3"
        --, Tuple "low-deep" "https://freesound.org/data/previews/350/350660_1676145-hq.mp3"
        --, Tuple "knock-pad" "https://freesound.org/data/previews/7/7402_1629-hq.mp3"
        --, Tuple "gnarly-feedback" "https://freesound.org/data/previews/213/213906_862453-hq.mp3"
        --, Tuple "low-drone" "https://freesound.org/data/previews/353/353549_6493436-hq.mp3"
        -- yesHe
        , Tuple "shaky-scratchy" "https://freesound.org/data/previews/277/277172_93137-hq.mp3"
        --, Tuple "flag-banging" "https://freesound.org/data/previews/169/169798_1661766-hq.mp3"
        -- Ambiance
        --, Tuple "costal-ambiance" "https://freesound.org/data/previews/207/207553_285997-hq.mp3"
        --, Tuple "beautiful-birds" "https://freesound.org/data/previews/528/528661_1576553-hq.mp3"
        --, Tuple "robin" "https://freesound.org/data/previews/416/416529_5121236-hq.mp3"
        ------------- shredders
        -- second half good... nice and gear-y
        --, Tuple "indoor-shredder" "https://freesound.org/data/previews/82/82435_1276308-hq.mp3"
        --, Tuple "high-shrill-terrifying-shredder" "https://freesound.org/data/previews/181/181143_3374466-hq.mp3"
        --, Tuple "mechanical-clicking-shredder" "https://freesound.org/data/previews/78/78521_1218676-hq.mp3"
        --, Tuple "single-shred" "https://freesound.org/data/previews/26/26389_186469-hq.mp3"
        --, Tuple "nice-high-shred" "https://freesound.org/data/previews/21/21755_29541-hq.mp3"
        -------------------- egg timer
        --, Tuple "egg-timer-wind-plus-ring" "https://freesound.org/data/previews/14/14263_31076-hq.mp3"
        , Tuple "egg-timer-ticking" "https://freesound.org/data/previews/468/468081_2247456-hq.mp3"
        ------------------ gamelan
        --, Tuple "gamelan-bali" "https://freesound.org/data/previews/257/257625_3932570-hq.mp3"
        , Tuple "low-c#-cello-drone" "https://freesound.org/data/previews/195/195278_3623377-hq.mp3"
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
        , Tuple "beautiful-birds" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/birds.ogg"
        , Tuple "plane-landing" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/planeLanding.ogg"
        , Tuple "wall-clock" "https://freesound.org/data/previews/188/188615_3330286-lq.mp3"
        -- impros
        , Tuple "impro-glitch" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/improGlitch.ogg"
        , Tuple "impro-wobbly" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/improWobblyGlitch.ogg"
        , Tuple "impro-filigree" "https://klank-share.s3-eu-west-1.amazonaws.com/nature-boy/improFiligree.ogg"
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

newtype Interactions
  = Interactions
  { interactions :: Ref.Ref (InteractionOnsets)
  , dispose :: Effect Unit
  }

type InteractionOnsets
  = Array
      { id :: Int
      , x :: Number
      , y :: Number
      }

handleTE :: Int -> Ref.Ref (InteractionOnsets) -> TouchEvent -> Effect Unit
handleTE i ref te = do
  let
    ts = changedTouches te
  let
    l = TL.length ts
  let
    tlist = map (\t -> { id: i, x: toNumber $ T.clientX t, y: toNumber $ T.clientY t }) (catMaybes $ map (\x -> TL.item x ts) (range 0 (l - 1)))
  void $ Ref.modify (\ipt -> tlist <> ipt) ref

handleME :: Int -> Ref.Ref (InteractionOnsets) -> MouseEvent -> Effect Unit
handleME id ref me = do
  let
    x = toNumber $ ME.clientX me
  let
    y = toNumber $ ME.clientY me
  void $ Ref.modify (\ipt -> [ { id, x, y } ] <> ipt) ref

getInteractivity :: Effect Interactions
getInteractivity = do
  w <- window
  nav <- navigator w
  ua <- userAgent nav
  let
    mobile = isJust (indexOf (Pattern "iPhone") ua) || isJust (indexOf (Pattern "iPad") ua) || isJust (indexOf (Pattern "Android") ua)
  nInteractions <- Ref.new 0
  interactions <- Ref.new []
  target <- toEventTarget <$> window
  touchStartListener <-
    eventListener \e -> do
      fromEvent e
        # traverse_ \me -> do
            nt <- Ref.modify (_ + 1) nInteractions
            handleTE nt interactions me
  mouseStartListener <-
    eventListener \e -> do
      ME.fromEvent e
        # traverse_ \me -> do
            nt <- Ref.modify (_ + 1) nInteractions
            handleME nt interactions me
  if mobile then addEventListener (wrap "touchstart") touchStartListener false target else addEventListener (wrap "mousedown") mouseStartListener false target
  let
    dispose =
      if mobile then do
        removeEventListener (wrap "touchstart") touchStartListener false target
      else do
        removeEventListener (wrap "mousedown") mouseStartListener false target
  pure (Interactions { interactions, dispose })

withInteractions ::
  forall a.
  Interactions ->
  Event a ->
  Event { value :: a, interactions :: InteractionOnsets }
withInteractions (Interactions { interactions }) e =
  makeEvent \k ->
    e
      `subscribe`
        \value -> do
          interactionsValue <- Ref.read interactions
          k { value, interactions: interactionsValue }

interactionLog :: Interactions -> Behavior (InteractionOnsets)
interactionLog m = behavior \e -> map (\{ value, interactions: bs } -> value bs) (withInteractions m e)
