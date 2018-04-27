{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState

import qualified Data.Set as S
import qualified Data.List as L

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime.
-}
data Node s a = Node s a s Int [s]
    deriving (Eq, Show)

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}
nodeState :: Node s a -> s
nodeState (Node s _ _ _ _) = s

{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la starea dată ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.

    În afara BONUS-ului, puteți ignora parametrul boolean. Pentru BONUS, puteți
    sorta lista succesorilor folosind `sortBy` din Data.List.
-}

nodeDepth :: Node s a  -> Int
nodeDepth (Node _ _ _ d _) = d

nodeParents :: Node s a -> [s]
nodeParents (Node _ _ _ _ l) = l

getDfsList :: (ProblemState s a, Ord s) => [Node s a] -> S.Set s -> [Node s a] -> Int -> [Node s a]
getDfsList rez set [] maxDepth = reverse rez
getDfsList rez set (h:coada) maxDepth
    | (S.member s set) = getDfsList rez set coada maxDepth
    | (depth > maxDepth) = getDfsList rez set coada maxDepth
    | otherwise = getDfsList (h:rez) (S.insert s set) (newElements ++ coada) maxDepth
    where
        s = nodeState h
        l = nodeParents h
        succ = successors s
        depth = nodeDepth h
        newElements = [Node st act s (depth + 1) (st:l) | st <- (map (snd) succ), act <- (map (fst) succ)]

limitedDfs :: (ProblemState s a, Ord s)
           => s           -- Starea inițială
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
           --[(Node s Nothing s 0)]
limitedDfs s b maxDepth = getDfsList [] S.empty [(Node s randomValue s 0 [s])] maxDepth
    where
        randomValue = head $ map (fst) $ successors s
{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.

    În afara BONUS-ului, puteți ignora parametrul boolean.
-}

getIndex :: Eq s => s -> [s] -> Int
getIndex s lista = foldl (\acc elem -> if (fst elem) == s 
                                                then snd elem
                                                else acc
                                                ) 0 (zip lista [1..(length lista)])

iterativeDeepening :: (ProblemState s a, Ord s)
    => s                -- Starea inițială
    -> Bool             -- Pentru BONUS, `True` dacă utilizăm euristica
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening s b = foldl (\acc depth -> let listaNoduri = limitedDfs s b depth
                                                  listaStari = map (nodeState) listaNoduri
                                                  ct = length listaNoduri
                                                  goal = filter (isGoal) listaStari in
                                                  if (nodeState (fst acc)) /= s 
                                                    then acc
                                                    else if (length goal) > 0 
                                                        then ((Node (head goal) randomValue s depth (nodeParents (getNode (head goal) listaNoduri))), 
                                                              ((snd acc) + (getIndex (head goal) listaStari) - 1))
                                                        else ((Node s randomValue s 0 []), ((snd acc) + ct))
                                                            ) ((Node s randomValue s 0 []), 0) [0..99]
    where
        randomValue = head $ map (fst) $ successors s

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

nodeParent :: Node s a -> s
nodeParent (Node _ _ p _ _) = p 

nodeAction :: Node s a -> a
nodeAction (Node _ a _ _ _) = a

getNode :: Eq s => s -> [Node s a] -> Node s a
getNode state lista = head $ filter (\elem -> (nodeState elem) == state) lista

getNodeAction :: Eq s => s -> [(a, s)] -> a
getNodeAction state lista = fst $ head $ filter (\elem -> (snd elem) == state) lista

extractPath :: (ProblemState s a, Eq s, Ord s ) => Node s a -> [(a, s)]
extractPath (Node _ _ _ _ listaParinti) = reverse $ snd $ foldl (\acc elem -> let succList = successors (fst acc)
                                                                                  a = getNodeAction elem succList in
                                                                                  (elem, (a, elem):(snd acc))
                                                                                     ) (firstNode, []) restOfTheList
    where
        firstNode = head $ reverse listaParinti
        restOfTheList = tail (reverse listaParinti)

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}
printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))