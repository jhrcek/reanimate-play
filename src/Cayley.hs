{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cayley (anim) where

import Arrow (arrow, arrowHeadMarker)
import Control.Lens (Each (each), (&), (.~), (?~))
import Data.List (genericLength)
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
                newSpriteSVG_ $ translate 0 4  $ cayleyTable
                -- fork $ play $ mapA (translate 5 0) $ scene $ 
                --     gridToFunScene 2 3 [(1, 2), (2, 3), (3, 1)]
                gridToFunScene 1 3 [(0,0), (1,1), (2,2)]
                gridToFunScene 2 3 [(0,1), (1,2), (2,0)]
                gridToFunScene 3 3 [(0,2), (1,0), (2,1)]

                

gridToFunScene :: Double -> Double -> [(Double, Double)] -> Scene s ()
gridToFunScene startHorizDist endHorizDist fun = do
    sqToCirc <- newVar 0
    distProg <- newVar 0
    arrowOpacityProg <- newVar 0
    codomainAlignProg <- newVar 0

    let gridToFunFrame =
            GridToFunction fun startHorizDist endHorizDist
                <$> unVar sqToCirc
                <*> unVar distProg
                <*> unVar arrowOpacityProg
                <*> unVar codomainAlignProg

    newSprite_ $ gridToFunction <$> gridToFunFrame
    wait 0.5
    tweenVar sqToCirc 1 $ \val -> fromToS val 1
    wait 0.5
    tweenVar distProg 1 $ \val -> fromToS val 1 . curveS 2
    wait 0.5
    tweenVar arrowOpacityProg 1 $ \val -> fromToS val 1
    wait 0.5
    tweenVar codomainAlignProg 1 $ \val -> fromToS val 1 . curveS 2
    wait 0.5


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


cayleyTable :: Tree
cayleyTable = 
    mkGroup $
        zipWith
            (\i row -> translate 0 (fromIntegral i) $ mkGroup $ 
                zipWith (\j col -> translate (fromIntegral j) 0 $ squareToCircle 0 col)
                        [0 .. length row - 1]
                        row
            )
            (reverse [0 .. length rows - 1])
            rows
        
  where
      -- TODO unhardcode
    rows =
        [ [0,0,1,2]
        , [0,0,1,2]
        , [1,1,2,0]
        , [2,2,0,1]
        ]
        

data GridToFunction = GridToFunction
    { -- TODO less error prone representation of function
      function :: [(Double, Double)] -- represents function from {1..n} to {1..n}
    , startHorizDist :: Double
    , endHorizDist :: Double 
    , squareToCircleProg :: Double -- transition from squares (0) to circles (1)
    , distanceProg :: Double -- spread of cells from 0 to 1
    , arrowOpacityProg :: Double -- arrows invisible (0) to visible (1)
    , codomainAlignProg :: Double -- align codomain from initial position (0) to be aligned with domain (1)
    }


gridToFunction :: GridToFunction -> Tree
gridToFunction GridToFunction{..} =
    mkGroup $ viewMapping <$> function
  where
    domainX = 0
    codomainX = domainX + startHorizDist + (endHorizDist - startHorizDist) * distanceProg
    verticalSpread = 1 + 0.5 * distanceProg

    domainSize :: Double
    domainSize = genericLength function

    viewMapping :: (Double, Double) -> SVG
    viewMapping (from, to) =
        let domainY = (domainSize - from) * verticalSpread
            V1 codomainY =
                lerp -- linear interpolation
                    codomainAlignProg
                    (V1 $ (domainSize - to) * verticalSpread) -- finish
                    (V1 domainY) -- start
         in mkGroup
                [ withGroupOpacity arrowOpacityProg $
                    translate domainX 0.5 $
                        arrow
                            (V2 (domainX + 1) domainY)
                            (V2 codomainX codomainY)
                , translate domainX domainY $ squareToCircle squareToCircleProg (round from)
                , translate codomainX codomainY $ squareToCircle squareToCircleProg (round to)
                ]
