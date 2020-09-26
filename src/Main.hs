#!/usr/bin/env stack
-- stack runghc --package reanimate

{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

--import Reanimate.Scene
import Control.Lens ((&), (.~))
import qualified Data.Text as Text
import Graphics.SvgTree (ElementRef (Ref), LineJoin (JoinBevel), strokeLineJoin)
import Linear.V2
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
      c = pointOnCircle (2*pi*alpha)-- (pi / 8 + (6 * alpha * pi / 8))
      triangleVertices = [b, c, a]
      dot = withFillOpacity 1 $ withFillColor "white" $ mkCircle 0.06
      angleLabel x y z txt =
        let avgAngle = averageAngle x y z
         in latex txt
              & center
              & scale 0.5
              & withFillColor "white"
              & withFillOpacity 1
              & uncurry translate y
              & translate (cos avgAngle) (sin avgAngle)
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
                -- mkLine (0, 0) c,
                -- Angles
                withClipPathRef (Ref "triangle-mask") $
                  mkGroup [translate x y (mkCircle 0.7) | (x, y) <- triangleVertices],
                -- vertex labels
                mkGroup $
                  zipWith
                    ( \(x, y) text ->
                        mkText text
                          & withFillOpacity 1
                          & withFillColor "white"
                          & scale 0.2
                          & translate (1.1 * x) (1.1 * y)
                    )
                    triangleVertices
                    ["B", "C", "A"],
                angleLabel c a b "$\\alpha$",
                angleLabel a b c "$\\beta$",
                angleLabel a c b "$\\gamma$"
              ]

-- | Given angle <ABC find the direction of the angle bisector in radians
averageAngle :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
averageAngle (ax, ay) (bx, by) (cx, cy) =
  let baAngle = atan2 (ay - by) (ax - bx)
      bcAngle = atan2 (cy - by) (cx - bx)
   in (baAngle + bcAngle) / 2
