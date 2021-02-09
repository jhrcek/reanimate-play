{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Picture (PixelRGBA8 (..))
import Control.Lens ((&), (.~))
import Data.Text (Text)
import Graphics.SvgTree (ElementRef (Ref), LineJoin (JoinBevel), strokeLineJoin)
import Reanimate


main :: IO ()
main = reanimate animation


animation :: Animation
animation =
    addStatic (mkBackground "black") $
        staticFrame 0.02 thales


thales :: SVG
thales =
    let radius = 4
        theta = 2 * pi / 3
        a = fromPolar radius pi
        b = fromPolar radius 0
        c = fromPolar radius theta
        origin = (0, 0)
        triangleVertices = [b, c, a]
        dot = withFillOpacity 1 $ withFillColor "white" $ mkCircle 0.06
        angleLabel x y z txt color =
            let bisectAngle = bisectorAngle x y z
             in textLabel color txt
                    & uncurry translate y
                    & translate (cos bisectAngle) (sin bisectAngle)
     in withStrokeColor "white" $
            withFillOpacity 0 $
                withStrokeWidth (0.5 * defaultStrokeWidth) $
                    mkGroup
                        [ mkClipPath "ACO" [mkLinePathClosed [a, c, origin]]
                        , mkClipPath "BCO" [mkLinePathClosed [b, c, origin]]
                        , -- The big circle
                          mkCircle radius
                        , -- Alpha angles
                          withClipPathRef (Ref "ACO") $
                            mkGroup
                                [ mkCircle 0.7
                                    & translate x y
                                    & withFillColorPixel green
                                    & withFillOpacity 1
                                    & withStrokeWidth 0
                                | (x, y) <- [a, c]
                                ]
                        , -- Beta angles
                          withClipPathRef (Ref "BCO") $
                            mkGroup
                                [ mkCircle 0.7
                                    & translate x y
                                    & withFillColorPixel red
                                    & withFillOpacity 1
                                    & withStrokeWidth 0
                                | (x, y) <- [b, c]
                                ]
                        , -- Triangle
                          mkLinePathClosed triangleVertices
                            & strokeLineJoin .~ pure JoinBevel
                        , mkGroup [translate x y dot | (x, y) <- triangleVertices]
                        , -- Center
                          dot
                        , textLabel white "O" & translate 0 -0.4
                        , mkLine (0, 0) c
                        , -- vertex labels
                          mkGroup $
                            zipWith
                                ( \(x, y) text ->
                                    textLabel white text
                                        & translate (1.1 * x) (1.1 * y)
                                )
                                triangleVertices
                                ["B", "C", "A"]
                        , angleLabel origin a c "$\\alpha$" green
                        , angleLabel origin c a "$\\alpha$" green
                        , angleLabel origin c b "$\\beta$" red
                        , angleLabel origin b c "$\\beta$" red
                        ]


{- | Given three points A, B and C, find the direction of
 the angle bisector of <ABC in radians
-}
bisectorAngle :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
bisectorAngle (ax, ay) (bx, by) (cx, cy) =
    let baAngle = atan2 (ay - by) (ax - bx)
        bcAngle = atan2 (cy - by) (cx - bx)
     in (baAngle + bcAngle) / 2


textLabel :: PixelRGBA8 -> Text -> SVG
textLabel color text =
    latex text
        & center
        & scale 0.4
        & withFillColorPixel color
        & withFillOpacity 1


red, green, white :: PixelRGBA8
red = PixelRGBA8 255 0 0 0
green = PixelRGBA8 0 255 0 0
white = PixelRGBA8 255 255 255 0


-- | Given polar coordinates (radius and angle in radians) return  cartesian (x,y) coordinates of the point
fromPolar :: Double -> Double -> (Double, Double)
fromPolar radius theta =
    (radius * cos theta, radius * sin theta)
