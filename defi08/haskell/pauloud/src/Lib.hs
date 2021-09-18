
module Lib
    ( mainSF,defaultDims
    ) where
 import FRP.Yampa
 import qualified Graphics.Gloss as G
 import qualified Graphics.Gloss.Interface.IO.Interact as I
 import Control.Arrow
 import FRP.Yampa.Arrow(arr2)
 import Data.Bool

 type Input = Event I.Event
 defWidth = 800
 defHeight = 600
 defaultDims = (defWidth,defHeight)
 mainSF :: SF Input G.Picture 
 mainSF =  (parB [unscaledColumns,unscaledCheckers,unscaledWinnerText] >>> arrPrim G.Pictures) &&& floatingDims --parB :: [SF a b] -> SF a [b]
                >>> arr2 (\pic (sx,sy) -> G.scale sx sy pic) -- arr2 :: (a -> b -> c) -> SF (a,b) c

 unscaledColumns :: SF Input G.Picture
 unscaledColumns = fmap G.Pictures $ parB $ map (\c -> constant (columnToRectangle c (1,1)) >>> arrPrim (\((x1,y1),(x2,y2))
                                                                -> G.Color G.blue $ G.Polygon [(x1,y1)
                                                                    ,(x2,y1),(x2,y2),(x1,y2)])) [0..6] 

 unscaledCheckers :: SF Input G.Picture 
 unscaledCheckers = G.scale (72/800) (68/600) <$> checkersPicture 
    where checkersPicture = G.Pictures <$> parB [(plays >>> checkersColors &&& checkersPositions  
                    >>> unresizedCheckers)]


 unscaledWinnerText :: SF Input G.Picture 
 unscaledWinnerText = game >>> gameResult >>> winnerDisplaying >>> arrPrim (G.translate (-700) (620) ) 
            >>> arrPrim (G.scale (1/1500) (1/1500) )


 type Point = G.Point 
 type WindowDims = (Int,Int)
 resizings :: SF Input (Event WindowDims) 
 resizings = arrEPrim (\e -> case e of
    Event (I.EventResize dims )-> Event dims
    _ -> NoEvent)

 windowDims :: SF Input WindowDims
 windowDims = hold defaultDims <<< resizings
 floatingDims = (\(w,h) -> (fromIntegral w, fromIntegral h)) <$> windowDims

 clics :: SF Input (Event Point)
 clics = arrEPrim (\e -> case e of
    Event (I.EventKey (I.MouseButton I.LeftButton)  
        I.Down _ position) -> Event position
    _ -> NoEvent)

 type Rectangle = (Point,Point)


 type ColumnRank = Int 

 columnToRectangle c (w,h) = ((-3/11*w +(fromIntegral c)/11*w,-4*h/12) 
                             ,(-13/66*w +(fromIntegral c)/11*w ,4*h/12))

 columnsRectangles :: SF Input [(ColumnRank,Rectangle)]
 columnsRectangles =  parB $ map (\c -> constant c &&& floatingDims >>> arr2 (\n (w,h) -> 
                                        (n,columnToRectangle n (w,h)) )
                                    ) [0..6] 


 columnsClics :: SF Input (Event ColumnRank)
 columnsClics = constant (foldr rMerge NoEvent) <*> columnsClicked 
            where columnsClicked = (constant map) <*> (columnClicked <*> clics) 
                                               <*> columnsRectangles -- (<*>) :: SF a (b -> c) -> SF a b -> SF a c 
 columnClicked :: SF Input (Event Point -> (ColumnRank,Rectangle) -> Event ColumnRank)
 columnClicked = constant(columnClickedF)
 columnClickedF clic (columnRank,((x1,y1),(x2,y2))) = case clic of
                NoEvent -> NoEvent
                Event (x,y) ->  if x >= x1 && x <= x2 && y >= y1 && y<= y2
                                then Event columnRank 
                                else NoEvent
 data Color = Red | Yellow deriving (Eq,Show)
 toGloss Red = G.red
 toGloss Yellow = G.yellow 

 data Game = Game {turn :: Color, grid :: [(Color,(Int,Int))]}
 game0 = Game {turn = Red, grid = []}
 remplissage n game = length <<< (filter (snd>>>fst>>>(==) n)) $ (grid game)

 data Play = Play Color (Int,Int) -- on choisit de consacrer une sf uniquement pour les actions de jeu
 game :: SF Input (Game,Event Play) -- sans doute pas la meilleur idée, on aurait pu regrouper game et game result
 game = columnsClics >>> sscan iteration (game0,NoEvent) 
                where iteration (g,_) NoEvent  = (g,NoEvent)
                      iteration (g,_) (Event n) = if remplissage n g >= 6 
                                                      then (g,NoEvent)
                                                      else (update n g,(Event (Play (turn g) (n,remplissage n g) ) ) )
                      update n g = g{grid = (turn g, (n,remplissage n g)) : grid g
                                     ,turn = if turn g == Red
                                             then Yellow
                                             else Red}
 
 quadrupletons :: [[(Int,Int)]] -- la liste les ensembles de 4 positions qui forment une ligne droite 
 quadrupletons = lignes ++ colonnes ++ diagonales1 ++ diagonales2 -- les listes sont une monades, on peut donc utiliser la do-notation 
        where diagonales1 = do -- les diagonales qui montent vers la droite
                c <- [0..3] -- 3 = nombre de colonnes moins 4
                r <- [0..2] -- 2 = nombre de rangées moins 4
                [[0..3] >>= (\n -> [(c + n, r + n)])] -- on "simule" une triple boucle for ainsi 
              diagonales2 = do -- les diagonales qui descendent vers la droite 
                c <- [3..6] -- [3..6] = toutes les colones sauf les 3 premières
                r <- [0..2]
                [[0..3] >>= (\n -> [(c - n, r + n)])]

              colonnes = do 
                c <- [0..6]
                r <- [0..2]
                [[0..3] >>= (\n -> [(c , r + n)])]
              lignes = do 
                c <- [0..3]
                r <- [0..5]
                [[0..3] >>= (\n -> [(c + n , r)])]


 hasWon :: Color -> [(Color,(Int,Int))] -> Bool

 hasWon color list = let list' :: [(Int,Int)]
                         list' = ((map snd) . (filter (fst >>> (==)color))) list
                         isOwn :: [(Int,Int)] -> Bool 
                         isOwn quadrupleton = foldr (\p b -> b && elem p list') True quadrupleton
                            in any isOwn quadrupletons  
 type GameResult = Maybe (Maybe Color)
 tie = Nothing :: Maybe Color 
 gameResult :: SF (Game,Event Play) GameResult
 gameResult = switch (arrPrim f) (\result -> constant (Just $ result))
                    where f (g,Event(Play c _)) = if hasWon c (grid g) 
                            then (Nothing, Event $ Just c ) else if (length $ grid g) >= 42 
                                                                 then (Nothing,Event tie)
                                                                 else (Nothing,NoEvent)
                          f _ = (Nothing,NoEvent)


 winnerDisplaying :: SF GameResult G.Picture 
 winnerDisplaying = switch (constant G.Blank &&& edgeJust)  f 
        where f (Just color) = constant $ G.Color (toGloss color) $ G.Text ("winner : " ++ show color)
              f Nothing = constant $ G.Color (G.blue) $ G.Text ("It's a Tie")


 
 plays :: SF Input (Event Play)
 plays = game >>> arrPrim snd
  
 unresizedCheckers :: SF ([Color],[(Float,Float)]) G.Picture
 unresizedCheckers = arr2 zip >>> arrPrim (map drawChecker) >>> arrPrim G.Pictures
        where drawChecker (color,(x,y)) = G.Color (toColor color ) $ G.translate (x - 2.6) (y-2.6)  $   G.circleSolid 0.4
              toColor Red = G.red
              toColor Yellow = G.yellow

 
 checkersColors:: (SF (Event Play)) [Color] -- les "checkers" sont les images des pions
 checkersColors = sscan iteration [] 
    where iteration list NoEvent = list
          iteration list (Event(Play c _)) = c:list
 acceleration = -0.8 ::Float -- l'accélération de la chute des checkers 
 updatePath dt (((x,y),speedY),(column,row)) = (((x,y1),speedY1),(column,row)) 
      {- column et row donnent les coordonées à atteindre, comme il n'y a aucun déplacement horizontal
       column et x sont constants et égaux -} 
            where y1 = max (fromIntegral row) (y + (speedY + speedY1)*(realToFrac dt)/2) -- on calcul la vitesse moyenne et on l'intègre
                  speedY1 = speedY + acceleration * (realToFrac dt) -- on intègre l'accélération 
 checkersPositions :: SF (Event Play) [(Float,Float)]
 checkersPositions = paths >>> arrPrim (map (fst.fst)) 
    where paths = let -- paths donne en sortie une liste de couples ( (positon,vitesseHorizontale),(colonne,rangée))
                      updatePaths dt = map (updatePath dt)
                      iteration (Event(Play _ (c,r) )) _ dt list = (((fromIntegral c,7.0),0),(c,r)) : updatePaths dt list
                      iteration _ _ dt list = updatePaths dt list
                        in iterFrom iteration [] 
 

