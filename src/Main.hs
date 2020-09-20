#!/usr/bin/env stack
-- stack runghc --package reanimate

{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative
import Control.Lens (set, (&), (.~))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Graphics.SvgTree --(LineJoin (JoinBevel), strokeLineJoin, pattern None)
import Graphics.SvgTree.Types (fontFamily)
import Linear.V2
import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Math.Common (distance')
import Reanimate.Transition
import Reanimate.Voice

main :: IO ()
main = do
  ts <- fakeTranscript <$> Text.readFile "/home/jhrcek/Devel/github.com/jhrcek/reanimate-play/transcript.txt"
  reanimate $
    addStatic (mkBackground "white") $
      sceneAnimation $ do
        fork $ annotateWithTranscript ts

        circleDrawProgress <- newVar 0
        diameterDrawProgress <- newVar 0
        triangleDrawProgress <- newVar 0
        radiusDrawProgress <- newVar 0
        centerDrawProgress <- newVar 0
        endpointsDrawProgress <- newVar 0
        cTheta <- newVar $ 2 * pi / 3

        newSprite_ $
          fmap triangle $
            SceneVars
              <$> unVar circleDrawProgress
              <*> unVar diameterDrawProgress
              <*> unVar triangleDrawProgress
              <*> unVar radiusDrawProgress
              <*> unVar centerDrawProgress
              <*> unVar endpointsDrawProgress
              <*> unVar cTheta

        waitUntil $ wordStart $ findWord ts ["circle"] "circle"
        tweenVar circleDrawProgress 2 $ \val -> fromToS val 1

        waitUntil $ wordStart $ findWord ts ["circle"] "center"
        -- TODO some signal to dedupe the "highlight"
        tweenVar centerDrawProgress 0.5 $ \val -> fromToS val 3
        tweenVar centerDrawProgress 0.5 $ \val -> fromToS val 1

        waitUntil $ wordStart $ findWord ts ["diameter"] "diameter"
        tweenVar diameterDrawProgress 1 $ \val -> fromToS val 1

        waitUntil $ wordStart $ findWord ts ["diameter"] "endpoints"
        tweenVar endpointsDrawProgress 0.5 $ \val -> fromToS val 3
        tweenVar endpointsDrawProgress 0.5 $ \val -> fromToS val 1

        waitUntil $ wordStart $ findWord ts ["point"] "point"
        tweenVar triangleDrawProgress 1 $ \val -> fromToS val 1

        wait 1
        tweenVar cTheta 1 $ \val -> fromToS val (pi / 2) . curveS 2
        wait 1
        tweenVar cTheta 1 $ \val -> fromToS val (pi / 3) . curveS 2
        wait 1
        tweenVar cTheta 1 $ \val -> fromToS val (pi / 10) . curveS 2
        wait 1
        tweenVar cTheta 1 $ \val -> fromToS val (pi * 2 / 3) . curveS 2
        wait 1
        tweenVar radiusDrawProgress 1 $ \val -> fromToS val 1
        wait 5

triangle :: SceneVars -> SVG
triangle SceneVars {..} =
  let cx = 4 * cos cTheta
      cy = 4 * sin cTheta
   in withStrokeColor "black" $
        withFillOpacity 0 $
          withStrokeWidth (defaultStrokeWidth * 0.4) $
            mkGroup
              [ mkClipPath "triangle-mask" $ removeGroups $ mkLinePath [(-4, 0), (cx, cy), (4, 0)],
                drawIf (triangleDrawProgress > 0.99) $
                  withFillOpacity 1 $
                    mkGroup
                      [ translate (1.05 * cx) (1.05 * cy) $ textLabel "C",
                        withFillOpacity 0 $ mkCircle 0.05
                      ],
                partial circleDrawProgress $ mkCircle 4,
                partial diameterDrawProgress $ mkLine (-4, 0) (4, 0),
                -- Center
                mkCircle (centerDrawProgress * 0.05),
                drawIf (centerDrawProgress > 0.99) $
                  withFillOpacity 1 $
                    translate 0 -0.4 $ textLabel "O",
                -- Endpoints
                mkGroup
                  [ translate -4.0 0 $ mkCircle (endpointsDrawProgress * 0.05),
                    translate 4.0 0 $ mkCircle (endpointsDrawProgress * 0.05)
                  ],
                drawIf (endpointsDrawProgress > 0.99) $
                  withFillOpacity 1 $
                    mkGroup
                      [ translate -4.2 0 $ textLabel "A",
                        translate 4.2 0 $ textLabel "B"
                      ],
                -- Triangle
                partial triangleDrawProgress $
                  mkLinePath [(-4, 0), (cx, cy), (4, 0)]
                    & strokeLineJoin .~ pure JoinBevel,
                partial radiusDrawProgress $ mkLine (0, 0) (cx, cy),
                drawIf (triangleDrawProgress > 0.99) $
                  withClipPathRef (Ref "triangle-mask") $
                    mkGroup
                      [ translate cx cy $ mkCircle 0.5,
                        translate 4 0 $ mkCircle 0.5,
                        translate -4 0 $ mkCircle 0.5
                      ]
              ]

partial :: Double -> SVG -> Tree
partial progress = partialSvg progress . pathify

textLabel :: Text.Text -> Tree
textLabel = scale 0.2 . mkText

drawIf :: Bool -> SVG -> SVG
drawIf cond svg = if cond then svg else None

data SceneVars = SceneVars
  { circleDrawProgress :: Double,
    diameterDrawProgress :: Double,
    triangleDrawProgress :: Double,
    radiusDrawProgress :: Double,
    centerDrawProgress :: Double,
    endpointsDrawProgress :: Double,
    cTheta :: Double
  }

{- TRANSCRIPT

Let's draw a circle.
A circle is a shape consisting of all points in a plane that are a given distance from a given point, the centre;

A diameter of a circle is any straight line segment that passes through the center of the circle and whose endpoints lie on the circle.

-}
