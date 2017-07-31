module Main where

import Prelude
import Control.Monad.Eff
import Data.List
import Data.Maybe
import Prim as P
import Math as Math

import Mathbox.Classes as C
import Mathbox.Field
import Mathbox.Mathbox
import Mathbox.Types

-- Mathbox properties can either be static (using Val)
-- or dynamic (using Fun or Sig)
-- Try replacing some Val's below by a Fun

camPos = Val $ mkVec3 0 0 4
--camPos = Fun $ (\t -> [ 0.5 * Math.cos(2.0 * t)
--                      , 0.4 * Math.cos(t)
--                      , 0.5 * Math.sin(t) + 4.0])


axisWidth = Val 4.0
--axisWidth = Fun $ \t -> 2.0 * Math.cos(t) + 4.0

gridWidth = Val 2.0
--gridWidth = Fun $ \t -> 1.5 * Math.sin(t) + 2.0

mathbox :: List MathboxPrimitive
mathbox =
  (Camera $ C.mkCamera { proxy = Val true, position = Just camPos }) :
  (Cartesian $ C.mkCartesian { 
    range = Val [mkVec2 (-2) 2, mkVec2 (-1) 1, mkVec2 (-1) 1], 
    scale = Val (mkVec3 2 1 1) }) (
    (Axis $ C.mkAxis { width = axisWidth }) :
    (Axis $ C.mkAxis { width = axisWidth, axis = Val axis2 }) :
    (Grid $ C.mkGrid { width = gridWidth, divideX = Val 20.0, divideY = Val 10.0 }) :
    Nil
  ) :
  Nil

main = do
  mkMathbox { plugins: ["core", "controls", "cursor"]
            , controls: { klass: trackballControls }
            , fullscreen: true
            } >>=
  applyOnThree (setThreeClearColor colorWhite 1.0) >>=
  set { focus: Just 3.0, scale: Just 720.0 } >>=
  addAll (map toJs mathbox)
