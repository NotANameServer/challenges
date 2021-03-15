module Main where
    import Text.Megaparsec
    import Text.Megaparsec.Char --bibliothèque Megaparsec à installer
    import qualified Data.List as List
    import Data.Function ((&))
    import Data.Void
    import Control.Arrow

    type NotAPosition = (Int,Int)

    data NotATileType = Wall| Terminus | Standard
    data NotATile = Tile { tilePosition :: NotAPosition,tileType :: NotATileType} 
    type NotAMaze = [NotATile]
    mazeParser :: Parec Void String NotAMaze
    mazeParser = do
        firstRow <- rowParser
        subsequentsRows <- many rowParser
        return  (let  array2D :: [[NotATileType]]
                      array2D = firstRow : subsequentsRows
                      imax = length firstRow
                      jmax = length subsequentsRows + 1
                        in [ Tile (i,j) (array2D !! i !! j) | i <- [1..imax],
                                                              j <- [1..jmax]] 
                   )
            where typeParser = (char '#' >>  return Wall) <|> (char ' ' >> return Standard) <|> (char '.' >> return Terminus) :: Parsec Void String NotATileType
                  rowParser  = do
                     row <- some typeParser
                     char('\n')
                     return row 

    data NotAPotentiel = Finite Int | Infinite deriving (Eq,Ord)
    inc _ Infinite = Infinite
    inc n (Finite k) = Finite (n+k)

    data NotANoeud = Noeud {ntype :: NotATileType, npos :: NotAPosition , predecesseur :: NotAPosition, potentiel :: NotAPotentiel, bonChemin :: Bool, potentielMinimise :: Bool} 
    instance Eq NotANoeud where 
        n1 == n2 = npos n1 == npos n2
    noeud0 (Tile pos ttype) = Noeud {ntype = ttype, npos = pos, predecesseur = pos, potentiel = Infinite, bonChemin = False, potentielMinimise = False}
    successeur n1 n2 = let ((x1,y1),(x2,y2)) = (npos n1, npos n2)
                        in case compare x1 x2 of
                            EQ -> abs (y1 - y2) == 1
                            _ ->  (y1 == y2) && (abs (x1 - x2) == 1)
    instance Ord NotANoeud where
        compare n1 n2 = if n1 == n2 then EQ else compare (potentielMinimise n1, potentiel n1) (potentielMinimise n2, potentiel n2)

    {-resolution maze = djikstra noeuds debut fin
        where (debut,fin) = case List.filter ((==)Terminus <<< tileType) maze of
                                (d:f:_) -> (noeud0 d,noeud0 f)
                                [d] -> (noeud0 d,noeud0 d)
                                [] -> error "aucun terminus trouvé"-}

    djikstra :: [NotANoeud] -> NotANoeud -> NotANoeud -> [NotANoeud]
    djikstra liste depart fin = rec depart liste
        where rec :: NotANoeud -> [NotANoeud] -> [NotANoeud]
              rec noeud liste =  if   noeud == fin 
                                then  chemin (npos fin) liste
                                else let nouvelleListe = updateSuccesseurs noeud liste in if (potentielMinimise.minimum) nouvelleListe then liste else rec (minimum nouvelleListe) nouvelleListe
              chemin :: NotAPosition-> [NotANoeud] -> [NotANoeud]
              chemin position liste = if position == position depart
                                     then   liste
                                     else let nouvelleListe = updatePredecesseurs position liste in chemin (predecesseur position) nouvelleListe
              updateSuccesseurs :: NotANoeud -> [NotANoeud] -> [NotANoeud]
              updateSuccesseurs noeud liste = let update n1 n2 = if successeur n1 n2 
                                                                 then (if (inc 1) (potentiel n1) < potentiel n2
                                                                       then n2 {potentiel = (inc 1) (potentiel n2), predecesseur = npos n1}
                                                                       else n2)
                                                                 else (if n1 == n2 then n1 {potentielMinimise = True} else n2)
                                                    in map update (liste)
              updatePredecesseurs :: NotAPosition -> [NotANoeud] -> [NotANoeud]                                      
              updatePredecesseurs position liste = if position == position depart 
                                                then liste 
                                                else let update noeud1 = if predecesseur noeud1 == position
                                                                        then noeud1 {bonChemin = True}
                                                                        else noeud1
                                                        in map update  (liste)

    main = print $ (map npos).(filter bonChemin).djikstra  (map noeud0 [Tile (1,1) Wall, Tile (1,2) Terminus, Tile (1,3) Standard, Tile (1,4) Wall,
                   Tile (1,1) Standard, Tile (1,2) Standard, Tile (1,3) Wall, Tile (1,4) Standard,
                   Tile (1,1) Standard, Tile (1,2) Terminus, Tile (1,3) Standard, Tile (1,4) Standard])
                  


