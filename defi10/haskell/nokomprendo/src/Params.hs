module Params where

nSamples :: Int
nSamples = 40

winWidthF, winHeightF :: Float
winWidthF = 800
winHeightF = 600

winWidth05F, winHeight05F :: Float
winWidth05F = winWidthF/2
winHeight05F = winHeightF/2

winWidth05, winHeight05 :: Double
winWidth05 = realToFrac winWidth05F
winHeight05 = realToFrac winHeight05F

velocityMinMax :: (Double, Double)
velocityMinMax = (10, 200)

radiusMinMax :: (Double, Double)
radiusMinMax = (15, 30)

radiusMargin :: Double
radiusMargin = 10

radiusMarginF :: Float
radiusMarginF = realToFrac radiusMargin

elasticity :: Double
elasticity = 1.0

