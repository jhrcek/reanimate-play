{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Arrows (main) where

import Control.Lens ((&), (.~), (?~))
import Data.Foldable (for_)
import Graphics.SvgTree
import Linear.V2
import Reanimate
import System.Random


main :: IO ()
main = do
    coords <- consecutiveParis <$> generateRandomCoords 100
    reanimate $ anim coords


anim :: [(V2 Double, V2 Double)] -> Animation
anim coords =
    addStatic (mkBackground "black") $
        scene $
            for_
                coords
                ( \(from, to) -> do
                    spr <- newSpriteSVG (myScene from to)
                    wait 0.1
                    destroySprite spr
                )


-- | Sequence of coordinates starting from (0,0) and then going not far in random direction
generateRandomCoords :: Int -> IO [V2 Double]
generateRandomCoords n = go n (pure 0)
  where
    go m p
        | m <= 0 = pure []
        | otherwise = do
            let candidates =
                    [ q
                    | d <- sequence (pure [-1, 0, 1])
                    , d /= pure 0
                    , let q@(V2 x y) = p + d
                    , abs x <= screenRight
                    , abs y <= screenTop
                    ]
            nxt <- pickElement candidates
            (p :) <$> go (m - 1) nxt


consecutiveParis :: [a] -> [(a, a)]
consecutiveParis xs = zip xs (tail xs)


pickElement :: [a] -> IO a
pickElement xs = (xs !!) <$> randomRIO (0, length xs - 1)


myScene :: V2 Double -> V2 Double -> Tree
myScene from to =
    withFillColor "red" $
        withStrokeColor "red" $
            withStrokeWidth (defaultStrokeWidth * 0.5) $
                mkGroup
                    [ mkDefinitions [arrowHeadMarker]
                    , grid
                    , arrow from to
                    ]


grid :: SVG
grid =
    withStrokeColor "white" $
        withFillColor "white" $
            mkGroup [translate x y $ mkCircle 0.01 | (x, y) <- gridCoords]


gridCoords :: [(Double, Double)]
gridCoords =
    [ (x, y)
    | x <- fmap fromIntegral [rnd screenLeft .. rnd screenRight]
    , y <- fmap fromIntegral [rnd screenBottom .. rnd screenTop]
    ]


arrow :: V2 Double -> V2 Double -> SVG
arrow (V2 fromX fromY) (V2 toX toY) =
    mkLine (fromX, fromY) (toX, toY)
        & markerEnd .~ pure (Ref "arrow")


arrowHeadMarker :: SVG
arrowHeadMarker =
    markerTree $
        defaultSvg
            & attrId ?~ "arrow"
            & markerViewBox ?~ (0, 0, len, len)
            & markerWidth ?~ Num 8
            & markerHeight ?~ Num 8
            & markerOrient ?~ OrientationAuto
            -- This moves the arrow back so that it's tip coincides with the end of line which it marks
            & markerRefPoint .~ (Num len, Num halfWidth)
            & markerElements
                .~ [ mkPath
                        [ MoveTo OriginAbsolute [V2 0 0]
                        , LineTo OriginAbsolute [V2 len halfWidth]
                        , LineTo OriginAbsolute [V2 0 (2 * halfWidth)]
                        , EndPath
                        ]
                   ]
  where
    len = 10
    halfWidth = 4


rnd :: Double -> Int
rnd = round
