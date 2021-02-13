{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module AmiciLogo (animation) where

import Reanimate


animation :: Animation
animation =
    addStatic (mkBackground "white") $
        foldr seqA (pause 0) [a, b, c, d]
  where
    a = rotateLogo $ \t -> scaleXY t 1 amiciLogo
    b = rotateLogo $ \t -> scaleXY 1 t amiciLogo
    c = rotateLogo $ \t -> scaleXY t -1 amiciLogo
    d = rotateLogo $ \t -> scaleXY 1 (- t) amiciLogo
    rotateLogo rot = signalA (\t -> cos (pi * t)) . mkAnimation 0.3 $ rot


amiciLogo :: SVG
amiciLogo =
    center $
        withStrokeWidth 0.1 $
            withFillColor "white" $
                withStrokeColor "orange" $
                    scale 2 $
                        mkGroup
                            [ mkLinePath [(0, 0), (0, 1), (0.5, 0.5), (1, 1), (1, 0)]
                            , translate -0.05 1.3 $ mkCircle 0.1
                            , translate 1.05 1.3 $ mkCircle 0.1
                            ]
