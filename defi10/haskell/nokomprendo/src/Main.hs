{-# LANGUAGE StrictData #-}

import Control.Lens
import qualified Data.Vector as V
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Interface.IO.Interact
import Linear.V2
import System.Exit

import Animation
import Params
import Scene

main :: IO ()
main = do
    let width = 5 + round winWidthF
        height = 5 + round winHeightF
        window = InWindow "boules2d" (width, height) (0, 0)
        bgcolor = black
        fps = 30
    scene <- mkScene 
    playIO window bgcolor fps scene hDraw hEvent hTime

hEvent :: Event -> Scene -> IO Scene
hEvent (EventKey (SpecialKey KeyEsc) Up _ _) _ = exitSuccess
hEvent (EventKey (SpecialKey KeySpace) Up _ _) _ = mkScene
hEvent _ scene = return scene

hTime :: Float -> Scene -> IO Scene
hTime dt = return . animateScene (realToFrac dt) 

hDraw :: Scene -> IO Picture
hDraw scene = return $ Pictures $ V.toList (V.map drawObject (_objects scene))

v2ToPoint :: V2 Double -> Point
v2ToPoint p = (realToFrac (p^._x), realToFrac (p^._y))

drawObject :: Object -> Picture
drawObject (Ball p _ r _ (cr,cg,cb)) = 
    Color (makeColor cr cg cb 1) 
        $ uncurry translate (v2ToPoint p) 
        $ circleSolid (realToFrac r)
drawObject WallLeft = 
    Color white $ Line [(-winWidth05F, -winHeight05F), (-winWidth05F, winHeight05F)]
drawObject WallRight = 
    Color white $ Line [(winWidth05F, -winHeight05F), (winWidth05F, winHeight05F)]
drawObject WallTop = 
    Color white $ Line [(-winWidth05F, winHeight05F), (winWidth05F, winHeight05F)]
drawObject WallBottom = 
    Color white $ Line [(-winWidth05F, -winHeight05F), (winWidth05F, -winHeight05F)]

