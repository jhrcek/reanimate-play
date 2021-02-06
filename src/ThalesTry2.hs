{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ThalesTry2 (main) where

import Control.Lens ((&), (.~))
import qualified Data.Text as Text
import Graphics.SvgTree
import Reanimate
import Reanimate.Scene
import Reanimate.Voice (
    TWord (..),
    Transcript (..),
    annotateWithTranscript,
    findWord,
    loadTranscript,
    wordStart,
 )


ts0 :: Transcript
ts0 = loadTranscript "/home/jhrcek/Devel/github.com/jhrcek/reanimate-play/transcript.txt"


main :: IO ()
main = do
    reanimate $
        addStatic (mkBackground "black") $
            scene $ do
                titleDuration <- withSceneDuration titleScene
                let ts = postponeTranscript titleDuration ts0
                fork $ annotateWithTranscript ts

                aP <- newVar 0
                bP <- newVar 0
                cP <- newVar 0
                circleP <- newVar 0
                diameterP <- newVar 0
                triangleP <- newVar 0
                cTheta <- newVar $ 2 * pi / 3

                newSprite_ $
                    fmap thales $
                        SceneVars
                            <$> unVar aP
                            <*> unVar bP
                            <*> unVar cP
                            <*> unVar circleP
                            <*> unVar diameterP
                            <*> unVar triangleP
                            <*> unVar cTheta

                waitUntil $ wordStart $ findWord ts ["statement"] "geometry"
                tweenVar circleP 2 $ \val -> fromToS val 1

                waitUntil $ wordStart $ findWord ts ["statement"] "A"
                fork $ do
                    tweenVar aP 0.5 $ \val -> fromToS val 2
                    tweenVar aP 0.5 $ \val -> fromToS val 1
                waitUntil $ wordStart $ findWord ts ["statement"] "B"
                fork $ do
                    tweenVar bP 0.5 $ \val -> fromToS val 2
                    tweenVar bP 0.5 $ \val -> fromToS val 1
                waitUntil $ wordStart $ findWord ts ["statement"] "C"
                fork $ do
                    tweenVar cP 0.5 $ \val -> fromToS val 2
                    tweenVar cP 0.5 $ \val -> fromToS val 1

                waitUntil $ wordStart $ findWord ts ["statement"] "AC"
                tweenVar diameterP 1 $ \val -> fromToS val 1

                waitUntil $ wordStart $ findWord ts ["statement"] "then"
                tweenVar triangleP 0.5 $ \val -> fromToS val 1


titleScene :: Scene s ()
titleScene = do
    title <-
        oNew $
            withStrokeColor "white" $
                withFillColor "whiteu" $
                    withStrokeWidth 0.1 $
                        scale 1.5 $
                            center $ latex "Thales's theorem"
    oShowWith title oDraw
    wait 1
    oHideWith title oFadeOut
    wait 1


thales :: SceneVars -> SVG
thales SceneVars{..} =
    let cx = 4 * cos cTheta
        cy = 4 * sin cTheta
        r = 4.0
     in withStrokeColor "white" $
            withFillOpacity 0 $
                withStrokeWidth (defaultStrokeWidth * 0.4) $
                    mkGroup
                        [ partial circleP $ mkCircle 4
                        , partial diameterP $ mkLine (-4, 0) (4, 0)
                        , mkGroup
                            [ translate (- r) 0 $ mkCircle (aP * 0.05)
                            , withFillOpacity aP $ translate (-1.05 * r) 0 $ textLabel "A"
                            ]
                        , mkGroup
                            [ translate cx cy $ mkCircle (bP * 0.05)
                            , withFillOpacity bP $ translate (1.05 * cx) (1.05 * cy) $ textLabel "B"
                            ]
                        , mkGroup
                            [ translate r 0 $ mkCircle (cP * 0.05)
                            , withFillOpacity cP $ translate (1.05 * r) 0 $ textLabel "C"
                            ]
                        , -- Center
                          withFillOpacity 1 $
                            mkGroup
                                [ mkCircle (diameterP * 0.05)
                                , drawIf (diameterP > 0.99) $
                                    translate 0 -0.4 $ textLabel "O"
                                ]
                        , partial triangleP $
                            mkLinePath [(-4, 0), (cx, cy), (4, 0)]
                                & strokeLineJoin .~ pure JoinBevel
                        ]


partial :: Double -> SVG -> Tree
partial progress = partialSvg progress . pathify


textLabel :: Text.Text -> Tree
textLabel = withFillColor "white" . withStrokeWidth 0 . scale 0.2 . mkText


drawIf :: Bool -> SVG -> SVG
drawIf cond svg = if cond then svg else None


-- P stands for drawing Progresss (number betwee 0 - 1)
data SceneVars = SceneVars
    { aP :: Double
    , bP :: Double
    , cP :: Double
    , circleP :: Double
    , diameterP :: Double
    , triangleP :: Double
    , cTheta :: Double
    }


postponeTranscript :: Double -> Transcript -> Transcript
postponeTranscript delta Transcript{..} =
    Transcript
        { transcriptWords = fmap postponeWord transcriptWords
        , ..
        }
  where
    postponeWord :: TWord -> TWord
    postponeWord TWord{..} =
        TWord
            { wordStart = wordStart + delta
            , wordEnd = wordEnd + delta
            , ..
            }
