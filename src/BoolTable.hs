{-# LANGUAGE OverloadedStrings #-}

module BoolTable (animation) where

import Data.Char (intToDigit)
import Data.Function ((&))
import qualified Data.Text as Text
import Numeric (showIntAtBase)
import Reanimate
import Reanimate.Builtin.Documentation (docEnv)


animation :: Animation
animation = docEnv $ seqList $ fmap growingTable [1 .. 7]


seqList :: [Animation] -> Animation
seqList = foldr seqA (pause 0)


growingTable :: Int -> Animation
growingTable n =
    seqList
        [ expansion n `parA` legend n
        , subdivision n `parA` legend n
        , coloring (n + 1) `parA` legend (n + 1)
        ]


legend :: Int -> Animation
legend n =
    staticFrame 1 $
        latex
            ( Text.pack (show n)
                <> " variable"
                <> if n == 1
                    then ""
                    else "s"
            )
            & translate (-6) 0
            & withStrokeWidth 0.02


{- START: 2^n rows x n columns of square cells, colored red/green
   END: the whole image is expanded to 2x height
-}
expansion :: Int -> Animation
expansion n = animate $ \t -> redBlackGrid n (1 + t) 1 & scaleToHeight 8


{- START: where `expansion n` ended
   END: All the cells are cut in half, new column is added, containing squares with white fill
-}
subdivision :: Int -> Animation
subdivision n =
    animate $ \t ->
        scaleToHeight 8 . withStrokeWidth 0.02 $
            mkGroup
                [ redBlackGrid n 2 1
                , partialSvg t $
                    withFillOpacity 0 $
                        mkGroup
                            [ rect (n + 1) & pathify & translate 0 (idx - 2 ^ n)
                            | idx <- [0 .. 2 ^ (n + 1) - 1]
                            ]
                ]


{- START: where `subdivision n` ended
   TRANSITION: fill of the last columns of cells goes from white to red/green
   END: where `expansion n` starts
-}
coloring :: Int -> Animation
coloring n = animate $ \t -> redBlackGrid n 1 t & scaleToHeight 8


-- 1x1 square with lower left corner at (0,0)
square :: SVG
square = mkRect 1 1 & translate 0.5 0.5


--1 x n rectangle with lower left corner at (0,0)
rect :: Int -> SVG
rect n =
    let width = fromIntegral n
     in mkRect width 1 & translate (width / 2) 0.5


toBinaryString :: Int -> Int -> String
toBinaryString zeroPadWidth n =
    let binary = showIntAtBase 2 intToDigit n ""
     in replicate (zeroPadWidth - length binary) '0' <> binary


redBlackGrid :: Int -> Double -> Double -> SVG
redBlackGrid n yScale lastColumnFillOpacity =
    withStrokeWidth 0.01 . scaleXY 1 yScale . mkGroup $
        fmap (\idx -> row idx (toBinaryString n idx)) [0 .. 2 ^ n - 1]
  where
    row :: Int -> String -> SVG
    row idx cells =
        translate 0 (fromIntegral idx - 2 ^ (n - 1)) . mkGroup $
            zipWith cell [0 ..] cells
    cell :: Int -> Char -> SVG
    cell idx binaryDigit =
        square & translate (fromIntegral idx) 0
            & withFillColor (cellColor binaryDigit)
            & withFillOpacity
                ( if idx == n - 1
                    then lastColumnFillOpacity
                    else 1
                )
    cellColor :: Char -> String
    cellColor '0' = "red"
    cellColor _ = "green"
