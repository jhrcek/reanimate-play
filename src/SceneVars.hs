{-# LANGUAGE OverloadedStrings #-}

module SceneVars (animation) where

import Data.Text (Text, pack)
import Reanimate
import Text.Printf (printf)


animation :: Animation
animation =
    addStatic (mkBackground "black") $
        scene $ do
            squareRot <- newVar 0
            circleScale <- newVar 1
            squareFillOpacity <- newVar 0
            descr <- newVar ""
            newSprite_ $ view <$> unVar squareRot <*> unVar circleScale <*> unVar squareFillOpacity <*> unVar descr

            writeVar descr "Rotate square"
            spause 0.5
            tweenVar squareRot 1 $ \val -> fromToS val 360

            writeVar descr "Enlarge circle"
            spause 0.5
            tweenVar circleScale 1 $ \val -> fromToS val 2

            writeVar descr "Redden square"
            spause 0.5
            tweenVar squareFillOpacity 1 $ \val -> fromToS val 1

            writeVar descr "Shrink circle"
            spause 0.5
            tweenVar circleScale 1 $ \val -> fromToS val 1

            writeVar descr "Rotate and blacken square"
            spause 0.5
            fork $ tweenVar squareFillOpacity 1 $ \val -> fromToS val 0
            tweenVar squareRot 1 $ \val -> fromToS val 720


spause :: Duration -> Scene s ()
spause = play . pause


view :: Double -> Double -> Double -> Text -> SVG
view squareRot circleScale squareOpacity descr =
    withStrokeColor "white" $
        gridLayout
            [
                [ withFillOpacity squareOpacity $withFillColor "red" $ rotate squareRot $ mkRect 2 2
                , withStrokeWidth (0.05 / circleScale) $ scale circleScale $ mkCircle 1
                ]
            ,
                [ translate 0 (-1) $
                    mkGroup
                        [ translate 0 3 $ t descr
                        , translate 0 2 $ t $ "Square rotation " <> showDouble squareRot
                        , translate 0 1 $ t $ "Circle scale " <> showDouble circleScale
                        , translate 0 0 $ t $ "Square opacity " <> showDouble squareOpacity
                        ]
                ]
            ]
  where
    t = withFillColor "white" . scale 0.4 . mkText
    showDouble :: Double -> Text
    showDouble = pack . printf "%.2f"
