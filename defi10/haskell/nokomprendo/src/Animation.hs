
module Animation where

import Control.Lens
import Data.List
import qualified Data.Vector as V
import Linear.Affine
import Linear.Matrix
import Linear.V2
import Linear.V4
import Linear.Vector
import Data.Maybe

import Params
import Scene

animateScene :: Double -> Scene -> Scene
animateScene dt scene
    | dt <= 0 = scene
    | otherwise = 
        let hits = computeHits dt scene 
            sceneDt = moveAllObjects dt scene
            cmpHits (_,_,h1) (_,_,h2) = compare (_hitTime h1) (_hitTime h2)
            (i1, i2, firstHit) = minimumBy cmpHits hits
            dti = _hitTime firstHit
            sceneDti0 = moveAllObjects dti scene
            newMoves = [(i1, _object1 firstHit), (i2, _object2 firstHit)]
            sceneDti1 = sceneDti0 & objects %~ (V.// newMoves)
            sceneDti2 = animateScene (dt-dti) sceneDti1
        in if null hits then sceneDt else sceneDti2

moveAllObjects :: Double -> Scene -> Scene
moveAllObjects dt scene = scene & objects %~ V.map (moveObject dt)

moveObject :: Double -> Object -> Object
moveObject dt b = b & pos +~ dt *^ _vel b

data Hit = Hit
    { _hitTime :: Double
    , _object1 :: Object
    , _object2 :: Object
    }

computeHits :: Double -> Scene -> [ (Int, Int, Hit) ]
computeHits dt (Scene objs) =
    let n = V.length objs
    in [ (k1, k2, fromJust maybeHit)
        | k1 <- [0 .. n-1], k2 <- [k1+1 .. n-1]
        , let maybeHit = tryHit dt (objs V.! k1) (objs V.! k2)
        , isJust maybeHit ]

-------------------------------------------------------------------------------
-- tryHit
-------------------------------------------------------------------------------

tryHit :: Double -> Object -> Object -> Maybe Hit 

tryHit t b1@Ball{} b2@Ball{} = 
    let (Ball p1@(V2 p1x p1y) v1@(V2 v1x v1y) r1 m1 _) = b1
        (Ball p2@(V2 p2x p2y) v2@(V2 v2x v2y) r2 m2 _) = b2
        -- detect collision
        dvx = v1x-v2x
        dvy = v1y-v2y
        dpx = p1x-p2x
        dpy = p1y-p2y
        dmin = r1+r2
        a = dvx*dvx + dvy*dvy
        b = dpx*dvx + dpy*dvy
        c = dpx*dpx + dpy*dpy - dmin*dmin
        delta = b*b - a*c
        rd = sqrt delta
        ti = min ((-b-rd)/a) ((-b+rd)/a)
        -- compute bounces
        d1 = p1 .+^ ti *^ v1
        d2 = p2 .+^ ti *^ v2
        nx = d2^._x - d1^._x
        ny = d2^._y - d1^._y
        tx = ny
        ty = -nx
        matA = V4 (V4 m1 0 m2 0)
                  (V4 0 m1 0 m2)
                  (V4 tx ty 0 0)
                  (V4 nx ny (-nx) (-ny))
        vecB = V4 (m1*v1x + m2*v2x)
                  (m1*v1y + m2*v2y)
                  (v1x*tx + v1y*ty)
                  (-elasticity*((v1x-v2x)*nx + (v1y-v2y)*ny))
        (V4 v1x' v1y' v2x' v2y') = luSolveFinite matA vecB
        b1' = b1 { _pos = d1, _vel = V2 v1x' v1y' }
        b2' = b2 { _pos = d2, _vel = V2 v2x' v2y' }
    in if (abs a > 0) && delta>0 && ti>=0 && ti<=t
        then Just (Hit ti b1' b2')
        else Nothing

tryHit t WallLeft b@(Ball p v r _ _) = 
    let ti = (-winWidth05 + r - p^._x) / v^._x
        o1 = moveObject ti b & vel . _x %~ negate
    in if ti>0 && ti<t then Just (Hit ti o1 WallLeft) else Nothing

tryHit t WallRight b@(Ball p v r _ _) = 
    let ti = (winWidth05 - r - p^._x) / v^._x 
        o1 = moveObject ti b & vel . _x %~ negate
    in if ti>0 && ti<t then Just (Hit ti o1 WallRight) else Nothing

tryHit t WallTop b@(Ball p v r _ _) = 
    let ti = (winHeight05 - r - p^._y) / v^._y
        o1 = moveObject ti b & vel . _y %~ negate
    in if ti>0 && ti<t then Just (Hit ti o1 WallTop) else Nothing

tryHit t WallBottom b@(Ball p v r _ _) = 
    let ti = (-winHeight05 + r - p^._y) / v^._y
        o1 = moveObject ti b & vel . _y %~ negate
    in if ti>0 && ti<t then Just (Hit ti o1 WallBottom) else Nothing

tryHit t b@Ball{} w = (\(Hit ti o1 o2) -> Hit ti o2 o1) <$> tryHit t w b

tryHit _ _ _ = Nothing

