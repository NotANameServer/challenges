{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Scene where

import Control.Lens 
import qualified Data.Vector as V
import Linear.Metric
import Linear.V2
import System.Random

import Params

data Object 
    = Ball
        { _pos :: V2 Double
        , _vel :: V2 Double
        , _rad :: Double
        , _mass :: Double
        , _col :: (Float, Float, Float)
        }
    | WallLeft
    | WallRight
    | WallTop
    | WallBottom
makeLenses ''Object

newtype Scene = Scene
    { _objects :: V.Vector Object
    }
makeLenses ''Scene

mkScene :: IO Scene
mkScene = do
    theBalls <-  checkBalls <$> V.replicateM nSamples mkBall 
    putStrLn $ "generated " ++ show (V.length theBalls) ++ " balls"
    let theWalls = V.fromList [ WallLeft, WallRight, WallTop, WallBottom ]
    return $ Scene $ theWalls V.++ theBalls

checkBalls :: V.Vector Object -> V.Vector Object
checkBalls theObjects = 
    let fAcc acc b = if any (overlap b) acc then acc else V.cons b acc
        overlap b1 b2 = let d2 = qd (_pos b1) (_pos b2)
                            r = _rad b1 + _rad b2
                        in d2 < (radiusMargin + r)**2
    in V.foldl' fAcc V.empty theObjects

mkBall :: IO Object
mkBall = do
    radius <- randomRIO radiusMinMax 
    let m = pi * radius * radius

    posX <- randomRIO (-winWidth05+radius+radiusMargin, winWidth05-radius-radiusMargin) 
    posY <- randomRIO (-winHeight05+radius+radiusMargin, winHeight05-radius-radiusMargin) 

    velTheta <- randomRIO (0, pi) 
    velNorm <- randomRIO velocityMinMax 
    let velX = velNorm * cos velTheta
    let velY = velNorm * sin velTheta

    colR <- randomRIO (0.3, 1.0) 
    colG <- randomRIO (0.3, 1.0) 
    colB <- randomRIO (0.3, 1.0) 
    let c = (colR, colG, colB)

    return $ Ball (V2 posX posY) (V2 velX velY) radius m c

