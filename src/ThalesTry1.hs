#!/usr/bin/env stack
-- stack runghc --package reanimate

{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative
import Control.Lens (set, (&), (.~))
import qualified Data.Text as Text
import Graphics.SvgTree (LineJoin (JoinBevel), strokeLineJoin)
import Graphics.SvgTree.Types (fontFamily)
import Linear.V2
import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Math.Common (distance')
import Reanimate.Transition

main :: IO ()
main =
  reanimate $
    addStatic (mkBackground "white") $
      sceneAnimation $ do
        triangleDrawProgress <- newVar 0
        circleDrawProgress <- newVar 0
        xLeg <- newVar 1
        yLeg <- newVar 1
        newSprite_ $
          fmap triangle $
            SceneVars
              <$> unVar triangleDrawProgress
                <*> unVar circleDrawProgress
                <*> unVar xLeg
                <*> unVar yLeg
        tweenVar triangleDrawProgress 2 $ \val -> fromToS val 1
        wait 1
        tweenVar circleDrawProgress 2 $ \val -> fromToS val 1
        wait 1
        tweenVar xLeg 1 $ \val -> fromToS val 4
        wait 1
        tweenVar yLeg 1 $ \val -> fromToS val 4
        wait 1
        tweenVar xLeg 1 $ \val -> fromToS val 1
        wait 1
        tweenVar yLeg 1 $ \val -> fromToS val 1
        wait 1

triangle :: SceneVars -> SVG
triangle SceneVars {..} =
    withStrokeColor "black" $
      withFillOpacity 0 $
        withStrokeWidth (defaultStrokeWidth * 0.4) $
          mkGroup
            [ translate (y / 2) (x / 2) $
                partialSvg circleDrawProgress $
                  pathify $ mkCircle ((distance' (V2 0 x) (V2 y 0)) / 2),
              partialSvg triangleDrawProgress $
                pathify $
                  mkLinePath [(0, 0), (0, x), (y, 0), (0, 0)]
                    & strokeLineJoin .~ pure JoinBevel,
              translate (y / 2) (x / 2) $ mkCircle 0.03
            ]

data SceneVars = SceneVars
  { triangleDrawProgress :: Double, -- 0-1
    circleDrawProgress :: Double, -- 0-1
    cAngle :: Double,
    
  }
