module CircleMakeSpace (animation) where

import Data.Foldable (for_)
import Data.Text (pack)
import Reanimate


animation :: Animation
animation =
    addStatic (mkBackground "black") $
        mapA (translate 0 (-2.5)) $
            scene $ do
                vars <- traverse newVar [0 .. 6]
                newSprite_ $ view <$> traverse unVar vars
                for_ vars $ \v -> do
                    start <- readVar v
                    tweenVar v 0.5 $ \val -> fromToS val (start - 1) . curveS 2


view :: [Double] -> SVG
view ys =
    withStrokeColor "white" $
        mkGroup $
            zipWith
                ( \i y ->
                    translate 0 y $
                        mkGroup
                            [ mkCircle 0.5
                            , translate 0 (-0.2) $ scale 0.3 $ withFillColor "white" $ mkText $ pack $ show i
                            ]
                )
                (reverse [1 .. len])
                ys
  where
    len = length ys
