{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}

data Object = Circle Color | Arrow Heading | Square Color Heading
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui obiect.
-}
instance Show Object where
   	show (Circle c)
		| c == Red = "r"
		| c == Blue = "b"
		| otherwise = "g"
	show (Arrow h) = show h
	show (Square c h)
		| c == Red = "R" ++ show h
		| c == Blue = "B" ++ show h
		| otherwise = "G" ++ show h

{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}


data Level = Level (M.Map Position [Object])
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}
getLeftmostPos :: [(Position, [Object])] -> Int
getLeftmostPos list = minimum ys
	where
		ys = map (snd.fst) list

getRightmostPos :: [(Position, [Object])] -> Int
getRightmostPos list = maximum ys
	where
		ys = map (snd.fst) list

getDownmostPos :: [(Position, [Object])] -> Int
getDownmostPos list = maximum xs
	where
		xs = map (fst.fst) list

getUpmostPos :: [(Position, [Object])] -> Int
getUpmostPos list = minimum xs
	where
		xs = map (fst.fst) list

printOneObject :: Object -> String
printOneObject (Square c h) = (show (Square c h)) ++ " "
printOneObject (Circle c) = "  " ++ show (Circle c)
printOneObject (Arrow h)  = "  " ++ show (Arrow h)

printTwoObjects :: Object -> Object -> String
printTwoObjects (Square c h) (Circle c2) = show (Square c h) ++ show (Circle c2)
printTwoObjects (Square c h) (Arrow h2) = show (Square c h) ++ show (Arrow h2)

createElem :: Level -> Int -> Int -> Int -> Int -> [Char]
createElem (Level m) x y rightMost downMost
	| and [(x == downMost), (y == rightMost)] = ""
	| y == rightMost = "\n"
	| and [(M.notMember (x, y) m), (y <  (rightMost - 1))] = "   |"
	| and [(M.notMember (x, y) m), (y >= (rightMost - 1))] = "   "
	| and [(length objList == 1), (y <  (rightMost - 1))] = printOneObject (head objList) ++ "|"
	| and [(length objList == 1), (y >= (rightMost - 1))] = (printOneObject (head objList))
	| and [(length objList == 2), (y < (rightMost - 1))] = (printTwoObjects (head objList) (last objList))  ++ "|"
	| otherwise = printTwoObjects (head objList) (last objList)
	where objList = m M.! (x, y)

createTable :: Level -> [Int] -> [Int] -> [String]
createTable l xs ys = [createElem l x y (last ys) (last xs)| x <- xs , y <- ys]

instance Show Level where
    show (Level l) = concat $ createTable (Level l) [(getUpmostPos (M.toList l))..(getDownmostPos (M.toList l))]
    											[(getLeftmostPos (M.toList l))..((getRightmostPos (M.toList l)) + 1)]
    										

{-
    *** TODO ***

    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = Level M.empty

{-
    *** TODO ***

    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare c h p (Level m)
	| M.notMember p m = Level $ M.insert p [(Square c h)] m
	| otherwise = Level $ M.insert p [(Square c h), oldObj] m 
	where
		oldObj = head $ m M.! p

{-
    *** TODO ***
	
    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle c p (Level m)
	| M.notMember p m = Level $ M.insert p [(Circle c)] m
	| otherwise = Level $ M.insert p [(Circle c), oldObj] m 
	where
		oldObj = head $ m M.! p

{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow h p (Level m)
	| M.notMember p m = Level $ M.insert p [(Arrow h)] m
	| otherwise = Level $ M.insert p [(Arrow h), oldObj] m 
	where
		oldObj = head $ m M.! p

{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}

--HELPFUL FUNCTIONS
getHeading :: Object -> Heading
getHeading (Square c h) = h
getHeading (Arrow h) = h

getColor :: Object -> Color
getColor (Square c h) = c
getColor (Circle c) = c

isNotSquare :: Object -> Bool
isNotSquare (Square c h) = False
isNotSquare (Circle c) = True
isNotSquare (Arrow h) = True

containsSquare :: [Object] -> Bool
containsSquare [] = False
containsSquare (obj:list)
	| isNotSquare obj = containsSquare list
	| otherwise = True

isNotArrow :: Object -> Bool
isNotArrow (Square c h) = True
isNotArrow (Circle c) = True
isNotArrow (Arrow h) = False

containsArrow :: [Object] -> Bool
containsArrow [] = False
containsArrow (obj:list)
	| isNotArrow obj = containsArrow list
	| otherwise = True

isNotCircle :: Object -> Bool
isNotCircle (Square c h) = True
isNotCircle (Circle c) = False
isNotCircle (Arrow h) = True

containsCircle :: [Object] -> Bool
containsCircle [] = False
containsCircle (obj:list)
	| isNotCircle obj = containsCircle list
	| otherwise = True

getSquareFromObjects :: [Object] -> Object
getSquareFromObjects (obj:list)
	| isNotSquare obj = getSquareFromObjects list
	| otherwise = obj

getArrowFromObjects :: [Object] -> Object
getArrowFromObjects (obj:list)
	| isNotArrow obj = getArrowFromObjects list
	| otherwise = obj

getNotSquareFromObjects :: [Object] -> [Object]
getNotSquareFromObjects (obj:list)
	| isNotSquare obj = [obj]
	| otherwise = getNotSquareFromObjects list

getNextPosition :: Position -> Heading -> Position
getNextPosition (x, y) h
	| h == East  = (x, y + 1)
	| h == South = (x + 1, y)
	| h == West  = (x, y - 1)
	| otherwise  = (x - 1, y)

getNewHeading :: Heading -> Position -> Level -> Heading
getNewHeading oldHeading p (Level m)
	| M.notMember p m = oldHeading
	| not $ containsArrow objList = oldHeading
	| containsArrow objList = getHeading $ getArrowFromObjects objList
	where
		objList = m M.! p
--END OF HELPFUL FUNCTIONS

getModifiedLevel :: Position -> Heading -> Level -> Level
getModifiedLevel p h (Level m)
	| M.notMember p m = (Level m)
	| not $ containsSquare objList = (Level m)
	| (length objList) == 1 = addSquare (getColor sq) newHeading p2 $ Level $ M.delete p m
	| otherwise = addSquare (getColor sq) newHeading p2 $ Level $
					M.adjust (getNotSquareFromObjects) p m
	where
		objList = m M.! p
		sq = getSquareFromObjects objList
		p2 = getNextPosition p h
		newHeading = getNewHeading (getHeading sq) p2 (Level m)

moveAllSquaresInOneDirection :: Position -> Heading -> Level -> Level
moveAllSquaresInOneDirection p h (Level m)
	| M.notMember p m = (Level m)
	| not $ containsSquare objList  = (Level m)
	| otherwise = getModifiedLevel p h $
				  moveAllSquaresInOneDirection p2 h (Level m)
	where
		objList = m M.! p
		p2 = getNextPosition p h

move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final
move p (Level m)
	| M.notMember p m = (Level m)
	| not $ containsSquare objList  = (Level m)
	| otherwise = moveAllSquaresInOneDirection p direction (Level m)
	where
		objList = m M.! p
		direction = getHeading $ getSquareFromObjects objList
		

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}

checkColors :: [Object] -> Bool
checkColors (obj:list)
	| (getColor $ obj) == (getColor $ head list) = True
	| otherwise = False

getSquaresPositions :: Level -> [Position]
getSquaresPositions (Level m) = map (fst) $ M.toList $ M.filterWithKey (\_ k -> containsSquare k) m

getUnfittedPositions :: Level -> [Position]
getUnfittedPositions (Level m) = map (fst) $ M.toList $ M.filterWithKey (\_ k -> checkColors k)
													  $ M.filterWithKey (\_ k -> containsCircle k)
													  $ M.filterWithKey (\_ k -> containsSquare k) m

instance ProblemState Level Position where
    successors l = foldl (\acc position -> acc ++ [(position, move position l)]) [] (getSquaresPositions l)

    isGoal l
    	| (length $ getUnfittedPositions l) == (length $ getSquaresPositions l) = True
    	|otherwise = False

    -- Doar petru BONUS
    -- heuristic =
