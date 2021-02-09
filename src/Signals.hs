module Signals (animation) where

import Data.Function ((&))
import Reanimate


animation :: Animation
animation =
    env $
        playThenReverseA $
            animate $ \t ->
                gridLayout
                    [
                        [ signalExample (curveS 1) t
                        , signalExample (curveS 2) t
                        , signalExample (curveS 5) t
                        ]
                    ,
                        [ signalExample (powerS 1) t
                        , signalExample (powerS 2) t
                        , signalExample (powerS 5) t
                        ]
                    ,
                        [ signalExample (bellS 1) t
                        , signalExample (bellS 2) t
                        , signalExample (bellS 5) t
                        ]
                    ,
                        [ signalExample oscillateS t
                        , signalExample (cubicBezierS (0.0, 0.8, 0.9, 1.0)) t
                        , signalExample (constantS 0.5) t
                        ]
                    ,
                        [ signalExample (fromToS 0.2 0.8) t
                        , signalExample (reverseS . fromToS 0.2 0.8) t
                        ]
                    ]


signalExample :: Signal -> Time -> SVG
signalExample sig t =
    mkGroup
        [ mkLinePath [(0, 1), (0, 0), (1, 0)]
        , mkLinePath [(x, sig x) | x <- [0, 0.02 .. 1]]
        , mkLinePath [(t, 0), (t, sig t), (0, sig t)]
        , mkCircle 0.03
            & translate 0 (sig t)
            & withFillColor "black"
            & withFillOpacity 1
        ]


env :: Animation -> Animation
env = mapA $ \svg ->
    mkGroup
        [ mkBackground "white"
        , withFillOpacity 0 $
            withStrokeWidth 0.01 $
                withStrokeColor "black" (mkGroup [svg])
        ]
