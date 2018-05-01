{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState

import qualified Data.Set as S

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

getDfsList :: (ProblemState s a, Ord s) => [Node s a] -> S.Set s -> [Node s a] -> Int -> Bool -> [Node s a]
getDfsList rez _ [] _ _ = reverse rez                                   -- coada este goala, deci nu avem ce vizita
getDfsList rez set (h:coada) maxDepth eurBool                           
    | (S.member s set) = getDfsList rez set coada maxDepth eurBool      -- starea "s" a fost parcursa
    | (depth > maxDepth) = getDfsList rez set coada maxDepth eurBool    -- depth > maxDepth
    | and [(eurBool == True), (isGoal s)] = getDfsList ((head rez):(h:(tail rez))) (S.insert s set) (newElements ++ coada) maxDepth eurBool
    | otherwise = getDfsList (h:rez) (S.insert s set) (newElements ++ coada) maxDepth eurBool -- starea este valida, deci o adaugam la rezultat
    where                                                                                     -- si la vectorul de vizitati adaugam succesorii
        s = nodeState h
        l = nodeParents h
        succ = successors s
        depth = nodeDepth h
        newElements = [Node st act s (depth + 1) (st:l) | st <- (map (snd) succ), act <- (map (fst) succ)] -- crearea de noduri

limitedDfs :: (ProblemState s a, Ord s)
           => s           -- Starea inițială
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
    
limitedDfs s eurBool maxDepth = getDfsList [] S.empty [(Node s randomValue s 0 [s])] maxDepth eurBool
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
iterativeDeepening s b = foldl (\acc depth -> let listaNoduri = limitedDfs s b depth            -- nodurile grafului cu depth [0..999]
                                                  listaStari = map (nodeState) listaNoduri      -- starile acestor noduri
                                                  ct = length listaNoduri                       -- numarul lor
                                                  goal = filter (isGoal) listaStari in          -- nodul care indeplineste isGoal
                                                  if (nodeState (fst acc)) /= s               -- am trecut de nodul "goal", deci nu modificam nimic
                                                    then acc
                                                    else if (length goal) > 0                 -- nivelul actual contine nodul goal, deci modificam acc
                                                        then ((Node (head goal) randomValue s depth (nodeParents (getNode (head goal) listaNoduri))), 
                                                              ((snd acc) + (getIndex (head goal) listaStari) - 1))
                                                        else ((Node s randomValue s 0 []), ((snd acc) + ct))
                                                            ) ((Node s randomValue s 0 []), 0) [0..999]
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

-- pe parcurs am construit o lista de parinti pentru a reconstrui drumul catre radacina
-- astfel am plecat de la radacina si prin apeluri consecutive ale "succesors", am creat PATH-ul dorit

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