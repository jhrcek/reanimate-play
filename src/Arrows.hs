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


anim :: [((Double, Double), (Double, Double))] -> Animation
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
generateRandomCoords :: Int -> IO [(Double, Double)]
generateRandomCoords n = go n (0, 0)
  where
    go m (x, y)
        | m <= 0 = pure []
        | otherwise = do
            let candidates =
                    [ (x1, y1)
                    | dx <- [-1 .. 1]
                    , let x1 = x + dx
                    , -8 <= x1 && x1 <= 8
                    , dy <- [-1 .. 1]
                    , let y1 = y + dy
                    , -4 <= y1 && y1 <= 4
                    , not (x == x1 && y == y1)
                    ]
            nxt <- pickElement candidates
            ((x, y) :) <$> go (m - 1) nxt


consecutiveParis :: [a] -> [(a, a)]
consecutiveParis [] = []
consecutiveParis [_] = []
consecutiveParis (x : y : ys) = (x, y) : consecutiveParis (y : ys)


pickElement :: [a] -> IO a
pickElement xs = (xs !!) <$> randomRIO (0, length xs - 1)


myScene :: (Double, Double) -> (Double, Double) -> Tree
myScene from to =
    withFillColor "red" $
        withStrokeColor "red" $
            withStrokeWidth (defaultStrokeWidth * 0.5) $
                mkGroup
                    [ mkDefinitions [arrowHeadMarker]
                    , grid
                    , mkGroup [arrow from to]
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


arrow :: (Double, Double) -> (Double, Double) -> SVG
arrow (fromX, fromY) (toX, toY) =
    let theta = atan2 (fromY - toY) (fromX - toX)
     in mkLine
            (fromX, fromY)
            ( toX + 0.15 * cos theta
            , toY + 0.15 * sin theta
            )
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
            & markerRefPoint .~ (Num 0, Num halfWidth)
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
