module ThalesTry3 (main) where

import Control.Monad (forM_)
import Reanimate
import System.Random


triples :: Ord a => [a] -> [(a, a, a)]
triples (x : y : z : rest) = (x, y, z) : triples rest
triples _ = []


main :: IO ()
main = do
    randomTriples <- take 10 . triples . randomRs (0, 2 * pi) <$> newStdGen

    reanimate $
        addStatic (mkBackground "black") $
            scene $ do
                alpha <- newVar 0
                beta <- newVar 0
                gamma <- newVar 0
                newSprite_ $ view <$> unVar alpha <*> unVar beta <*> unVar gamma

                forM_ randomTriples $ \(a, b, c) -> do
                    tweenVar alpha 1 $ \val -> fromToS val a
                    fork $ tweenVar beta 1 $ \val -> fromToS val b
                    fork $ tweenVar gamma 1 $ \val -> fromToS val c
                    wait 0.5


view :: Double -> Double -> Double -> SVG
view alpha beta gamma =
    let r = 4.0
     in withStrokeColor "white" $
            withFillOpacity 0 $
                withStrokeWidth (defaultStrokeWidth * 0.4) $
                    mkGroup
                        [ mkCircle r
                        , mkLinePath
                            [ anglePoint r alpha
                            , anglePoint r beta
                            , anglePoint r gamma
                            , anglePoint r alpha
                            ]
                        ]


anglePoint :: Double -> Double -> (Double, Double)
anglePoint radius angle =
    ( radius * cos angle
    , radius * sin angle
    )
