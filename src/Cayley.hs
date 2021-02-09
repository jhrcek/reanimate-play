module Cayley (anim) where

import Control.Lens
import Graphics.SvgTree.Types
import Reanimate


anim :: Animation
anim =
    docEnv2 $
        mapA center $
            playThenReverseA squareCircle
                `parA` animate (const $ grid 3 5)


grid :: Double -> Double -> SVG
grid w h =
    mkGroup
        ( [mkLine (0, y) (w, y) | y <- [0 .. h]]
          <> [mkLine (x, 0) (x, h) | x <- [0 .. w]]
        )
        & strokeLineCap .~ pure CapSquare


docEnv2 :: Animation -> Animation
docEnv2 = mapA $ \svg ->
    mkGroup
        [ mkBackground "white"
        , mkGroup [svg]
            & withStrokeColor "black"
            & withStrokeWidth 0.02
            & withFillOpacity 0
        ]


squareCircle :: Animation
squareCircle =
    animate $ \t ->
        defaultSvg
            & rectWidth ?~ Num 1
            & rectHeight ?~ Num 1
            & rectCornerRadius
                .~ ( pure . Num $ 0.5 * t
                   , pure . Num $ 0.5 * t
                   )
            & rectangleTree
