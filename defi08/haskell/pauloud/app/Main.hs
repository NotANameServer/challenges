module Main where
import Graphics.Gloss.Interface.FRP.Yampa
import qualified Lib
import qualified Graphics.Gloss as G
import qualified FRP.Yampa as Yampa 




main :: IO ()
main = playYampa (G.InWindow "pauloud-connect4" Lib.defaultDims (0,0)) 
                 (G.makeColor 0 0.8 0.4 0)
                 60 
                 Lib.mainSF
