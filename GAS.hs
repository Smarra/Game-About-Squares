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
printTwoObjects _ _ = "Not square inside this two object list."

{-
	Am adaugat la sfarsitul coloanelor o coloana doar cu caracterul newline.
	De acolo provine numarul "The rightmost element + 1".

	Forma Tabela:
	3 3 3 ... 3 3 4 2
	3 3 3 ... 3 3 4 2
		  ...
	3 3 3 ... 3 3 4 2
	3 3 3 ... 3 3 4 1 -> fara newline pe ultimul rand

	Cazul 3 este cel de baza. Acesta se imparte in 3 situatii:
	3.1 -> pe pozitia (x, y) nu se afla niciun element
	3.2 -> pe pozitia (x, y) se afla un element => printOneObject 
	3.3 -> pe pozitia (x, y) se afla 2 elemente => printTwoObjects
-}

createElem :: Level -> Int -> Int -> Int -> Int -> [Char]
createElem (Level m) x y rightMost downMost
	| and [(x == downMost), (y == rightMost)] = ""  -- 1
	| y == rightMost = "\n"							-- 2
	| and [(M.notMember (x, y) m), (y <  (rightMost - 1))] = "   |" -- 3.1
	| and [(M.notMember (x, y) m), (y >= (rightMost - 1))] = "   "  -- 4
	| and [(length objList == 1), (y <  (rightMost - 1))] = printOneObject (head objList) ++ "|" -- 3.2
	| and [(length objList == 1), (y >= (rightMost - 1))] = (printOneObject (head objList))      -- 4
	| and [(length objList == 2), (y < (rightMost - 1))] = (printTwoObjects (head objList) (last objList))  ++ "|"  -- 3.3
	| otherwise = printTwoObjects (head objList) (last objList)														-- 4
	where objList = m M.! (x, y)

createTable :: Level -> [Int] -> [Int] -> [String]
createTable l xs ys = [createElem l x y (last ys) (last xs)| x <- xs , y <- ys]

instance Show Level where
    show (Level l) = concat $ createTable (Level l) [(getUpmostPos (M.toList l))  .. (getDownmostPos (M.toList l))]
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
getHeading (Square _ h) = h
getHeading (Arrow h) = h

getColor :: Object -> Color
getColor (Square c _) = c
getColor (Circle c) = c

isSquare :: Object -> Bool
isSquare (Square _ _) = True
isSquare (Circle _) = False
isSquare (Arrow _) = False

isArrow :: Object -> Bool
isArrow (Square _ _) = False
isArrow (Circle _) = False
isArrow (Arrow _) = True

isCircle :: Object -> Bool
isCircle (Square _ _) = False
isCircle (Circle _) = True
isCircle (Arrow _) = False

containsSquare :: [Object] -> Bool
containsSquare list = not.null $ filter (isSquare) list

containsArrow :: [Object] -> Bool
containsArrow list = not.null $ filter (isArrow) list

containsCircle :: [Object] -> Bool
containsCircle list = not.null $ filter (isCircle) list

getSquareFromObjects :: [Object] -> Object
getSquareFromObjects list = head $ filter (isSquare) list

getArrowFromObjects :: [Object] -> Object
getArrowFromObjects list = head $ filter (isArrow) list

getNotSquareFromObjects :: [Object] -> [Object]
getNotSquareFromObjects list = filter (not.isSquare) list

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
	| (length objList) == 1 = addSquare (getColor sq) newHeading p2 $ Level $ M.delete p m -- un obiecte -> sigur patrat
	| otherwise = addSquare (getColor sq) newHeading p2 $ Level $ M.adjust (getNotSquareFromObjects) p m -- 2 obiecte
	where
		objList = m M.! p
		sq = getSquareFromObjects objList
		p2 = getNextPosition p h
		newHeading = getNewHeading (getHeading sq) p2 (Level m)

moveAllSquaresInOneDirection :: Position -> Heading -> Level -> Level
moveAllSquaresInOneDirection p h (Level m)
	| M.notMember p m = (Level m)									-- pozitia p este goala
	| not $ containsSquare objList  = (Level m)						-- pozitia p nu contine patrat
	| otherwise = getModifiedLevel p h $							-- apelam recursiv pentru urmatoarea casuta
				  moveAllSquaresInOneDirection p2 h (Level m)		-- 	   cat timp nu avem patrat in pozitia "p2"
	where
		objList = m M.! p 			-- lista cu obiectele de pe pozitia p
		p2 = getNextPosition p h    -- pozitia urmatoare pozitiei "p" in directia "h"

move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final
move p (Level m)
	| M.notMember p m = (Level m)										-- pozitia p este goala
	| not $ containsSquare objList  = (Level m)							-- pozitia p nu contine patrat
	| otherwise = moveAllSquaresInOneDirection p direction (Level m)	-- mutam toate patratele in directia "direction"
	where
		objList = m M.! p 											    -- lista cu obiectele de pe pozitia p
		direction = getHeading $ getSquareFromObjects objList           -- directia patratului de pe pozitia p
		

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

-- spre deosebire de getSquaresPositions care returneaza pozitiile patratelor
-- getUnfittedPositions returneaza pozitiile care contin patrat, cerc si culorile
-- acestora coincid.

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
    heuristic l
    	| isGoal l = 1
    	| otherwise = 0
