 {-# LANGUAGE RecordWildCards   #-}
 {-
  Le programme comporte des erreurs et ne compile pas, les annotations de types ont été ajoutées pour éclaircir mon intention
  et aider la correction des erreurs-}
 module Main where

    data NotAMaze = Maze {lignesM :: [[Char]], depart :: (Int,Int), sortie :: (Int,Int), w :: Int, h :: Int}
    data NotAPotentiel = Finite Int (Int,Int)| Infinite | Wall deriving (Eq,Ord)
    data NotAState = PState {minimized :: Bool, potentiel :: NotAPotentiel} deriving (Eq,Ord)
    inc1 (i,j) pState@PState{..} = case potentiel of
        Infinite -> pState {potentiel = Finite 1 (i,j)}
        Finite n _ -> pState {potentiel = Finite (n+1) (i,j)}
        Wall -> pState
    djikstra :: NotAMaze -> String
    djikstra maze = rec (idepart,jdepart) potentiels
        where (idepart,jdepart) = depart maze
              longueur = w maze
              hauteur = h maze
              lignes = lignesM maze
              potentiels :: [[NotAState]]
              potentiels =  [map (\carac -> case carac of 
                                             '.' ->  PState True (Finite 0 (idepart,jdepart))
                                             '#' -> PState True Wall 
                                             _ -> PState False Infinite) ligne
                                | ligne <- lignes]
              rec :: (Int,Int) -> [[NotAState]] -> String --String = [Char]                 
              rec (i,j) potentiels = let potentiels' = [[ (case compare i i1 of
                                                            EQ -> (case abs (j-j1) of
                                                                0 -> (potentiels !! j1 !! i1) {minimized = True}
                                                                1 -> inc1 (i,j) (potentiels !! j1 !! i1) 
                                                                _ -> potentiels !! j1 !! i1)
                                                            _ -> if abs (i-i1) == 1 && j == j1 
                                                                then inc1 (i,j) (potentiels !! j1 !! i1) 
                                                                else (potentiels !! j1 !! i1)) 

                                                | i1 <- [0..longueur-1]]
                                                | j1 <- [0..hauteur -1 ]]
                                        in case minimum (concat potentiels') of
                                            PState False (Finite _ (i,j)) -> if (i,j) == sortie maze
                                                                             then chemin (i,j) potentiels lignes
                                                                             else rec (i,j) potentiels'

                                            PState True (Finite _ (i,j)) -> chemin (i,j) potentiels lignes --erreur quand même
                                            PState _ _  -> lignes
              chemin :: (Int,Int) -> [[NotAState]] -> [[Char]] -> [Char]                          
              chemin (i,j) potentiels lignes = if (i,j) == depart maze then concatMap (\ligne -> ligne ++ "\n") lignes
                                              else let lignes' :: [[Char]]
                                                       lignes' = [[ (if (i1,j1) == (i,j) then '.' else lignes !! j1 !! i1)

                                                                  | i1 <- [0..longueur-1]]
                                                                  | j1 <- [0..hauteur-1]]
                                                       precedent :: NotAState -> (Int,Int)           
                                                       precedent PState _ (Finite _ (i1,j1)) = (i1,j1)
                                                       precedent _ = depart maze
                                                       (i',j') = precedent (potentiels !! j !! i)
                                                          in rec (i',j') potentiels lignes'  
    main = do
        input <- readFile "maze.txt" 
        maze <- return Maze {lignesM = (lines input), depart =  (0,0), sortie = (4998,4997), w = 4999, h = 4999  }
        print $ djikstra maze              
 }

