{-# LANGUAGE OverloadedStrings #-}

module Cayley (anim) where

import Arrow (arrow, arrowHeadMarker)
import Control.Lens
import Data.Text (pack)
import Graphics.SvgTree.Types
import Linear.V2
import Reanimate


anim :: Animation
anim =
    docEnv2 $
        mapA center $
            scene $ do
                sqToCirc <- newVar 0
                distProg <- newVar 0
                newSprite_ $ gridToFunction <$> unVar sqToCirc <*> unVar distProg
                wait 1
                tweenVar sqToCirc 1 $ \val -> fromToS val 1
                wait 1
                tweenVar distProg 1 $ \val -> fromToS val 1 . curveS 2
                wait 1


docEnv2 :: Animation -> Animation
docEnv2 = mapA $ \svg ->
    mkGroup
        [ mkDefinitions [withFillOpacity 1 arrowHeadMarker]
        , mkBackground "black"
        , mkGroup [svg]
        ]
        & withStrokeColor "white"
        & withFillColor "white"
        & withStrokeWidth 0.02


-- | Square to circle from t = 0 to 1
squareToCircle :: Double -> Int -> Tree
squareToCircle t label =
    mkGroup
        [ defaultSvg
            & rectWidth ?~ Num 1
            & rectHeight ?~ Num 1
            & rectCornerRadius . each .~ pure (Num (0.5 * t))
            & rectangleTree
            & withFillOpacity 1
            & withFillColor "black"
        , mkText (pack $ show label)
            & scale 0.3
            & translate 0.5 0.35
            & withFillOpacity 1
        ]


gridToFunction :: Double -> Double -> Tree
gridToFunction sqToCProg distProg =
    mkGroup
        [ -- arrows
          translate domainX (2 * verticalSpread + 0.5) $ arrow (V2 domainX 0) (V2 codomainX 0)
        , translate domainX (1 * verticalSpread + 0.5) $ arrow (V2 domainX 0) (V2 codomainX 0)
        , translate domainX (0 * verticalSpread + 0.5) $ arrow (V2 domainX 0) (V2 codomainX 0)
        , -- domain
          translate domainX (2 * verticalSpread) $ squareToCircle sqToCProg 1
        , translate domainX (1 * verticalSpread) $ squareToCircle sqToCProg 2
        , translate domainX (0 * verticalSpread) $ squareToCircle sqToCProg 3
        , -- codomain
          translate codomainX (2 * verticalSpread) $ squareToCircle sqToCProg 2
        , translate codomainX (1 * verticalSpread) $ squareToCircle sqToCProg 3
        , translate codomainX (0 * verticalSpread) $ squareToCircle sqToCProg 1
        ]
  where
    domainX = 0
    codomainX = domainX + 1 + 2 * distProg
    verticalSpread = 1 + distProg
