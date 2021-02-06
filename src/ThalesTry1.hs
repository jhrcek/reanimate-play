{-# LANGUAGE OverloadedStrings #-}

module ThalesTry1 (animation) where

import Control.Lens
import Control.Monad
import Linear
import Reanimate
import Reanimate.Scene


animation :: Animation
animation = env $
    scene $ do
        symbols <-
            mapM
                oNew
                [symbE, symbEq, symbM, symbC2]
        mapM_ oShow symbols
        wait 1

        forM_ (zip symbols yPositions) $
            \(obj, yPos) -> do
                fork $
                    oTweenS obj 1 $ \t -> do
                        oScale %= \origin -> fromToS origin scaleFactor t
                        oLeftX %= \origin -> fromToS origin screenLeft t
                        oCenterY %= \origin -> fromToS origin yPos t
                wait 0.3

        wait 1

        ls <- mapM oNew [energy, equals, mass, speedOfLight]

        forM_ (zip ls yPositions) $
            \(obj, nth) -> do
                oModifyS obj $ do
                    oLeftX .= -4
                    oCenterY .= nth
                oShowWith obj oDraw

        wait 2

        forM_ ls $ \obj ->
            fork $ oHideWith obj oFadeOut

        forM_ (reverse symbols) $ \obj -> do
            fork $
                oTweenS obj 1 $ \t -> do
                    oScale %= \origin -> fromToS origin 1 t
                    oTranslate %= lerp t (V2 0 0)
            wait 0.3
        wait 2


scaleFactor :: Double
scaleFactor = 0.7


symbE :: SVG
symbE = snd $ splitGlyphs [0] svg


symbEq :: SVG
symbEq = snd $ splitGlyphs [1] svg


symbM :: SVG
symbM = snd $ splitGlyphs [2] svg


symbC2 :: SVG
symbC2 = snd $ splitGlyphs [3, 4] svg


svg :: SVG
svg =
    scale 3 $
        center $
            latexAlign "E = mc^2"


energy :: SVG
energy =
    scale 1.5 $
        centerX $
            latex "Energy"


equals :: SVG
equals =
    scale 1.5 $
        centerX $
            latex "equals"


mass :: SVG
mass =
    scale 1.5 $
        centerX $
            latex "mass times"


speedOfLight :: SVG
speedOfLight =
    scale 1.5 $
        centerX $
            latex "speed of light$^2$"


yPositions :: [Double]
yPositions = [3, 1, -1, -3]


env :: Animation -> Animation
env =
    addStatic (mkBackground "lightblue")
        . mapA (withStrokeWidth defaultStrokeWidth)
        . mapA (withStrokeColor "black")
