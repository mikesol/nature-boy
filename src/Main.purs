module Klank.Dev where

import Prelude
import Color (Color, rgb)
import Data.Array (catMaybes, head, length, mapWithIndex, range)
import Data.Foldable (fold, foldl, traverse_)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (lcmap)
import Data.String (Pattern(..), indexOf)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1, D2)
import Data.Vec ((+>), empty)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Audio (AV(..), AudioParameter, AudioUnit, CanvasInfo(..), audioWorkletProcessor_, defaultExporter, dup2, evalPiecewise, g'add_, g'delay_, g'gain_, gain_, gain_', graph_, makePeriodicWave, microphone_, mul_, pannerMono_, periodicOsc_, runInBrowser_, sinOsc_, speaker)
import FRP.Event (Event, makeEvent, subscribe)
import Foreign.Object as O
import Graphics.Canvas (Rectangle)
import Graphics.Drawing (Drawing, Point, fillColor, filled, rectangle, text)
import Graphics.Drawing.Font (FontOptions, bold, font, italic, sansSerif)
import Math (pi, pow, sin, (%))
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))
import Type.Klank.Dev (Klank', defaultEngineInfo, klank)
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

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

loopT :: forall a. Number -> (Number -> a) -> (Number -> a)
loopT t = lcmap (_ % t)

boundPlayer :: forall a. Number -> (Number -> List a) -> Number -> List a
boundPlayer len a time = if (time) + kr >= 0.0 && time < (len) then a time else Nil

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

mic = microphone_ :: String -> AudioUnit D1

pmic :: String -> AudioUnit D2
pmic s = pannerMono_ ("voicePanner" <> s) 0.0 (mic s)

t1c440 :: Number -> Tuple Number Number
t1c440 = Tuple 1.0 <<< conv440

-----------------------
------------------
-----------
------
--
type SigAU
  = NatureBoyAccumulator -> Marker -> Number -> Tuple NatureBoyAccumulator (List (AudioUnit D2))

there0 :: SigAU
there0 =
  boundByCue There0 There0
    (\m t -> pure (pmic "Was0Mic"))

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

a0 :: SigAU
a0 =
  boundByCue A0 Boy0
    ( \m t ->
        pure
          $ graph_ "A0Graph"
              { aggregators:
                  { out: Tuple (g'add_ "A0Out") (SLProxy :: SLProxy ("combine" :/ SNil))
                  , combine: Tuple (g'add_ "A0Combine") (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
                  , gain: Tuple (g'gain_ "A0Gain" 0.4) (SLProxy :: SLProxy ("del" :/ SNil))
                  }
              , processors:
                  { del: Tuple (g'delay_ "A0Delay" 0.1) (SProxy :: SProxy "combine")
                  }
              , generators:
                  { mic: boundByCueNac''' A0 A0 (pmic "A0Mic") m
                  }
              }
    )

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
                          , gain: Tuple (g'gain_ "Boy0Gain" 0.6) (SLProxy :: SLProxy ("del" :/ SNil))
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
                                    ( gain_' "Boy0RampUpOsc" (min 1.0 (0.5 * (t - onset)))
                                        $ mul_ "Boy0Mul"
                                            ( ( pannerMono_ "Boy0OctavePan" (0.3 * sin (0.8 * pi * t))
                                                  $ periodicOsc_ "Boy0Octave" "smooth" (conv440 61.0)
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

------------------------
comp :: Marker -> List (Tuple Number Number)
comp There0 = Nil

comp Was0 = t1c440 <$> 60.0 : 63.0 : 64.0 : 68.0 : Nil

comp A0 = t1c440 <$> 56.0 : 59.0 : 61.0 : 64.0 : Nil

comp Boy0 = t1c440 <$> 54.0 : 56.0 : 57.0 : 61.0 : Nil

comp A1 = Nil

comp Ve1 = t1c440 <$> 54.0 : 58.0 : 61.0 : Nil

comp Ry1 = t1c440 <$> 56.0 : 58.0 : 60.0 : Nil

comp Strange1 = t1c440 <$> 58.0 : 61.0 : 63.0 : Nil

comp En1 = t1c440 <$> 62.0 : 65.0 : 68.0 : 71.0 : Nil

comp Chan1 = t1c440 <$> 61.0 : 63.0 : 66.0 : 70.0 : Nil

comp Ted1 = t1c440 <$> 60.0 : 62.0 : 66.0 : 69.0 : Nil

comp Boy1 = t1c440 <$> 59.0 : 63.0 : 65.0 : 68.0 : Nil

comp They2 = t1c440 <$> 58.0 : 61.0 : 64.0 : 67.0 : Nil

comp Say2 = t1c440 <$> 57.0 : 61.0 : 63.0 : 66.0 : Nil

comp He2 = t1c440 <$> 55.0 : 58.0 : 62.0 : 65.0 : Nil

comp Wan2 = t1c440 <$> 57.0 : 61.0 : 64.0 : 68.0 : Nil

comp Dered2 = t1c440 <$> 60.0 : 63.0 : 66.0 : 67.0 : 71.0 : Nil

comp Ve2 = t1c440 <$> 59.0 : 63.0 : 66.0 : 70.0 : Nil

comp Ry2 = t1c440 <$> 60.0 : 63.0 : 66.0 : 69.0 : Nil

comp Far2 = t1c440 <$> 61.0 : 62.0 : 66.0 : 68.0 : Nil

comp Ve3 = t1c440 <$> 60.0 : 63.0 : 67.0 : 71.0 : Nil

comp Ry3 = t1c440 <$> 61.0 : 63.0 : 65.0 : 70.0 : Nil

comp Far3 = t1c440 <$> 60.0 : 62.0 : 64.0 : 68.0 : Nil

comp O3 = t1c440 <$> 60.0 : 63.0 : 64.0 : 68.0 : Nil

comp Ver4 = t1c440 <$> 59.0 : 62.0 : 66.0 : Nil

comp Land4 = t1c440 <$> 61.0 : 63.0 : 67.0 : 70.0 : Nil

comp And4 = t1c440 <$> 57.0 : 59.0 : 62.0 : 66.0 : Nil

comp Sea4 = t1c440 <$> 56.0 : 60.0 : 61.0 : 65.0 : Nil

comp A5 = t1c440 <$> 57.0 : Nil

comp Lit5 = t1c440 <$> 58.0 : 61.0 : 65.0 : 69.0 : Nil

comp Tle5 = t1c440 <$> 60.0 : 63.0 : 68.0 : 70.0 : Nil

comp Shy5 = t1c440 <$> 57.0 : 63.0 : 65.0 : 68.0 : Nil

comp And5 = t1c440 <$> 57.0 : 61.0 : 64.0 : Nil

comp Sad5 = t1c440 <$> 60.0 : 63.0 : 70.0 : Nil

comp Of5 = t1c440 <$> 60.0 : 63.0 : 67.0 : 69.0 : Nil

comp Eye5 = t1c440 <$> 59.0 : 63.0 : 66.0 : 68.0 : Nil

comp But6 = t1c440 <$> 58.0 : 59.0 : Nil

comp Ve6 = t1c440 <$> 54.0 : 56.0 : 60.0 : Nil

comp Ry6 = t1c440 <$> 55.0 : 58.0 : 61.0 : Nil

comp Wise6 = t1c440 <$> 58.0 : 61.0 : 64.0 : Nil

comp Was6 = t1c440 <$> 58.0 : 62.0 : 65.0 : Nil

comp He6 = t1c440 <$> 57.0 : 60.0 : 63.0 : Nil

comp And7 = Nil

comp Then7 = Nil

comp One7 = Nil

comp Day7 = Nil

comp One8 = Nil

comp Ma8 = Nil

comp Gic8 = Nil

comp Day8 = Nil

comp He8 = Nil

comp Passed8 = Nil

comp My8 = Nil

comp Way8 = Nil

comp And9 = Nil

comp While9 = Nil

comp We9 = Nil

comp Spoke9 = Nil

comp Of9 = Nil

comp Ma9 = Nil

comp Ny9 = Nil

comp Things9 = Nil

comp Fools10 = Nil

comp And10 = Nil

comp Kings10 = Nil

comp This11 = Nil

comp He11 = Nil

comp Said11 = Nil

comp To11 = Nil

comp Me11 = Nil

comp The12 = Nil

comp Great12 = Nil

comp Est12 = Nil

comp Thing12 = Nil

comp You'll12 = Nil

comp E12 = Nil

comp Ver12 = Nil

comp Learn12 = Nil

comp Is13 = Nil

comp Just13 = Nil

comp To13 = Nil

comp Love13 = Nil

comp And13 = Nil

comp Be13 = Nil

comp Loved13 = Nil

comp In13 = Nil

comp Re13 = Nil

comp Turn13 = Nil

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
  | O3
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

m2n O3 = 23.000000

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

m2s O3 = "O-"

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
    , O3
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

toNel :: List (AudioUnit D2) -> NonEmpty List (AudioUnit D2)
toNel Nil = zero :| Nil

toNel (h : t) = h :| t

{-
( maybe Nil
  ( \cm ->
      ( simpleOsc (\s n -> periodicOsc_ ("po_" <> s) "smooth" n) (m2s cm) (comp cm)
          * audioWorkletProcessor_ "compGate"
              "klank-amplitude"
              O.empty
              (pmic "gatingSignal")
      )
        : Nil
  )
  retAcc.currentMarker
)
-}
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

    players = maybe initialV (\mk -> foldl (\{ aus, audAcc } f -> let (Tuple ak au) = f audAcc mk time in { audAcc: ak, aus: au <> aus }) initialV [ there0, was0, a0, boy0 ]) acc.currentMarker

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
    , periodicWaves =
      \ctx _ res rej -> do
        pw <-
          makePeriodicWave ctx
            (0.5 +> 0.25 +> -0.1 +> 0.07 +> 0.1 +> empty)
            (0.2 +> 0.1 +> 0.01 +> -0.03 +> -0.1 +> empty)
        res $ O.singleton "smooth" pw
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
