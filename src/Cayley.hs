{-# LANGUAGE OverloadedStrings #-}

module Cayley (anim) where

import Arrow (arrow, arrowHeadMarker)
import Control.Lens (Each (each), (&), (.~), (?~))
import Data.Text (pack)
import Graphics.SvgTree.Types
import Linear.V1
import Linear.V2
import Linear.Vector (lerp)
import Reanimate


anim :: Animation
anim =
    docEnv2 $
        mapA center $
            scene $ do
                sqToCirc <- newVar 0
                distProg <- newVar 0
                codomainAlignProg <- newVar 0
                newSprite_ $ gridToFunction <$> unVar sqToCirc <*> unVar distProg <*> unVar codomainAlignProg
                wait 1
                tweenVar sqToCirc 1 $ \val -> fromToS val 1
                wait 1
                tweenVar distProg 1 $ \val -> fromToS val 1 . curveS 2
                wait 1
                tweenVar codomainAlignProg 1 $ \val -> fromToS val 1 . curveS 2
                wait 1


docEnv2 :: Animation -> Animation
docEnv2 = mapA $ \svg ->
    mkGroup
        [ mkDefinitions [arrowHeadMarker]
        , mkBackground "black"
        , mkGroup [svg]
        ]
        & withStrokeColor "white"
        & withFillColor "white"
        & withFillOpacity 1
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
            & withFillColor "black"
        , mkText (pack $ show label)
            & scale 0.3
            & translate 0.5 0.35
        ]


gridToFunction :: Double -> Double -> Double -> Tree
gridToFunction sqToCProg distProg codomainAlignProg =
    mkGroup $ viewMapping <$> function
  where
    domainX = 0
    codomainX = domainX + 1 + 2 * distProg
    verticalSpread = 1 + distProg

    -- TODO less error prone representation of function
    function :: [(Double, Double)]
    function = [(1, 2), (2, 3), (3, 1)]

    domainSize :: Double
    domainSize = fromIntegral $ length function

    viewMapping :: (Double, Double) -> SVG
    viewMapping (from, to) =
        let domainY = (domainSize - from) * verticalSpread
            V1 codomainY =
                lerp -- linear interpolation
                    codomainAlignProg
                    (V1 $ (domainSize - to) * verticalSpread) -- finish
                    (V1 domainY) -- start
         in mkGroup
                [ translate domainX 0.5 $
                    arrow
                        (V2 (domainX + 1) domainY)
                        (V2 codomainX codomainY)
                , translate domainX domainY $ squareToCircle sqToCProg (round from)
                , translate codomainX codomainY $ squareToCircle sqToCProg (round to)
                ]
