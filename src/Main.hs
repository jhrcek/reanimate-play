#!/usr/bin/env stack
-- stack runghc --package reanimate

{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Lens ((&), (.~))
import Data.Text (Text)
import Graphics.SvgTree (ElementRef (Ref), LineJoin (JoinBevel), strokeLineJoin)
import Reanimate

main :: IO ()
main = reanimate $ addStatic (mkBackground "black") animation

animation :: Animation
animation = mkAnimation 5 (thales . bellS 2)

thales :: Time -> SVG
thales alpha =
  let r = 4
      pointOnCircle theta = (4 * cos theta, 4 * sin theta)
      a = pointOnCircle pi
      b = pointOnCircle 0
      c = pointOnCircle (pi / 8 + (6 * alpha * pi / 8))
      origin = (0, 0)
      triangleVertices = [b, c, a]
      dot = withFillOpacity 1 $ withFillColor "white" $ mkCircle 0.06
      angleLabel x y z txt =
        let bisectAngle = bisectorAngle x y z
         in textLabel txt
              & uncurry translate y
              & translate (cos bisectAngle) (sin bisectAngle)
   in withStrokeColor "white" $
        withFillOpacity 0 $
          withStrokeWidth (0.5 * defaultStrokeWidth) $
            mkGroup
              [ mkClipPath
                  "triangle-mask"
                  [mkLinePathClosed triangleVertices],
                -- The big circle
                mkCircle r,
                -- Center
                dot,
                -- Triangle
                mkLinePathClosed
                  triangleVertices
                  & strokeLineJoin .~ pure JoinBevel,
                mkGroup [translate x y dot | (x, y) <- triangleVertices],
                mkLine (0, 0) c,
                -- Angles
                withClipPathRef (Ref "triangle-mask") $
                  mkGroup [translate x y (mkCircle 0.7) | (x, y) <- triangleVertices],
                -- vertex labels
                mkGroup $
                  zipWith
                    ( \(x, y) text ->
                        textLabel text
                          & translate (1.1 * x) (1.1 * y)
                    )
                    triangleVertices
                    ["B", "C", "A"],
                angleLabel b a c "$\\alpha$",
                angleLabel a c origin "$\\alpha$",
                angleLabel origin c b "$\\beta$",
                angleLabel c b a "$\\beta$"
              ]

-- | Given three points A, B and C, find the direction of
-- the angle bisector of <ABC in radians
bisectorAngle :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
bisectorAngle (ax, ay) (bx, by) (cx, cy) =
  let baAngle = atan2 (ay - by) (ax - bx)
      bcAngle = atan2 (cy - by) (cx - bx)
   in (baAngle + bcAngle) / 2

textLabel :: Text -> SVG
textLabel text =
  latex text
    & center
    & scale 0.4
    & withFillColor "white"
    & withFillOpacity 1
