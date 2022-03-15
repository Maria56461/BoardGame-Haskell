{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game { hunter :: Position,
  targets :: Maybe [Target],
  obstacles :: [Position],
  gateways :: Maybe [(Position, Position)],
  north_child :: Maybe Game,
  south_child :: Maybe Game,
  east_child :: Maybe Game,
  west_child :: Maybe Game,
  randuri :: Int,
  coloane :: Int} 
  deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
show_particular :: Game -> (Int, Int) -> [Char]
show_particular (Game hunter targets obstacles gateways north_child south_child east_child west_child randuri
  coloane) (x, y)
  | elem (x, y) obstacles = if (y == (coloane - 1) && x /= (randuri - 1)) then "@\n" else "@"
  | (x, y) == hunter = "!"
  | otherwise = case targets of 
                  (Just targets) -> if elem (x, y) (map position targets) 
                    then "*" else
                      case gateways of 
                        (Just gateways) -> if elem (x, y) (map fst gateways) || elem (x, y) (map snd gateways) 
                          then "#" else " "
                        Nothing -> " "
                  Nothing -> case gateways of 
                        (Just gateways) -> if elem (x, y) (map fst gateways) || elem (x, y) (map snd gateways) 
                          then "#" else " "
                        Nothing -> " "
  
gameAsString :: Game -> String
gameAsString game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri
  coloane) = foldr (++) [] (map (show_particular game) [ (x, y) | x<-[0..(randuri - 1)], y<-[0..(coloane - 1)] ])

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame a b = new_game where new_game = Game {hunter = (1, 1), targets = Nothing,
obstacles = concat [[ (x, y) | x<-[1..(a - 2)], y<-[0, (b - 1)]], [(x, y) | x<-[0, (a - 1)], y<-[0..(b - 1)]]], gateways = Nothing,
north_child = Nothing, south_child = Nothing, east_child = Nothing, west_child = Nothing, randuri = a, coloane = b}

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter (a, b) game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri coloane)
  | elem (a, b) obstacles = game
  | a >= randuri || a < 0 = game
  | b >= coloane || b < 0 = game
  | otherwise = case targets of 
                  (Just targets) -> if elem (a, b) (map position targets) 
                    then game else
                      case gateways of 
                        (Just gateways) -> if elem (a, b) (map fst gateways) || elem (a, b) (map snd gateways) 
                          then game else new_game where
                            new_game = Game {hunter = (a, b), targets = Just targets, obstacles = obstacles,
                            gateways = Just gateways, north_child = north_child, south_child = south_child,
                            east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
                        Nothing -> new_game2 where
                          new_game2 = Game {hunter = (a, b), targets = Just targets, obstacles = obstacles,
                          gateways = Nothing, north_child = north_child, south_child = south_child,
                          east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
                  Nothing -> case gateways of
                        (Just gateways) -> if elem (a, b) (map fst gateways) || elem (a, b) (map snd gateways) 
                          then game else new_game3 where
                          new_game3 = Game {hunter = (a, b), targets = Nothing, obstacles = obstacles,
                          gateways = Just gateways, north_child = north_child, south_child = south_child,
                          east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
                        Nothing -> new_game4 where
                          new_game4 = Game {hunter = (a, b), targets = Nothing, obstacles = obstacles, gateways = Nothing,
                          north_child = north_child, south_child = south_child, east_child = east_child,
                          west_child = west_child, randuri = randuri, coloane = coloane}
{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}

addTarget :: Behavior -> Position -> Game -> Game
addTarget behavior (a, b)
  game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri coloane)
  | elem (a, b) obstacles = game
  | a >= randuri || a < 0 = game
  | b >= coloane || b < 0 = game
  | (a, b) == hunter = game
  | otherwise = case targets of 
                  (Just targets) -> if elem (a, b) (map position targets) then game else
                        case gateways of 
                          (Just gateways) -> if elem (a, b) (map fst gateways) || elem (a, b) (map snd gateways) then game
                               else new_game where new_game = Game {hunter = hunter, targets = Just (new_target:targets),
                                obstacles = obstacles, gateways = Just gateways, north_child = north_child,
                                south_child = south_child, east_child = east_child, west_child = west_child, randuri = randuri,
                                coloane = coloane} where new_target = Target {position = (a, b), behavior = behavior} 
                          Nothing -> new_game1 where new_game1 = Game {hunter = hunter, targets = Just (new_target1:targets),
                               obstacles = obstacles, gateways = Nothing, north_child = north_child, south_child = south_child,
                               east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
                               where new_target1 = Target {position = (a, b), behavior = behavior} 
                  Nothing -> case gateways of 
                        (Just gateways) -> if elem (a, b) (map fst gateways) || elem (a, b) (map snd gateways)
                          then game else new_game2 where
                            new_game2 = Game {hunter = hunter, targets = Just [new_target2], obstacles = obstacles,
                            gateways = Just gateways, north_child = north_child, south_child = south_child,
                            east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane} where
                            new_target2 = Target {position = (a, b), behavior = behavior}
                        Nothing -> new_game3 where
                            new_game3 = Game {hunter = hunter, targets = Just [new_target3], obstacles = obstacles,
                            gateways = Nothing, north_child = north_child, south_child = south_child,
                            east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
                            where new_target3 = Target {position = (a, b), behavior = behavior} 
{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway ((a, b), (c, d))
  game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri coloane)
  | elem (a, b) obstacles || elem (c, d) obstacles = game
  | (a >= randuri) || (c >= randuri) || (a < 0) || (c < 0) = game
  | (b >= coloane) || (d >= coloane) || (b < 0) || (d < 0) = game
  | otherwise = case targets of 
                  (Just targets) -> if (elem (a, b) (map position targets)) || (elem (c, d) (map position targets)) 
                    then game else
                      case gateways of 
                        (Just gateways) -> if (elem (a, b) (map fst gateways)) || (elem (c, d) (map fst gateways)) ||
                          (elem (a, b) (map snd gateways)) || (elem (c, d) (map snd gateways)) then game else new_game where
                            new_game = Game {hunter = hunter, targets = Just targets, obstacles = obstacles,
                            gateways = Just (((a, b), (c, d)):gateways), north_child = north_child, south_child = south_child,
                            east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
                        Nothing -> new_game2 where
                          new_game2 = Game {hunter = hunter, targets = Just targets, obstacles = obstacles,
                          gateways = (Just [((a, b), (c, d))]), north_child = north_child, south_child = south_child,
                          east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
                  Nothing -> case gateways of
                        (Just gateways) -> if (elem (a, b) (map fst gateways)) || (elem (c, d) (map fst gateways)) ||
                          (elem (a, b) (map snd gateways)) || (elem (c, d) (map snd gateways)) then game else new_game3 where 
                          new_game3 = Game {hunter = hunter, targets = Nothing, obstacles = obstacles,
                          gateways = Just (((a, b), (c, d)):gateways), north_child = north_child, south_child = south_child,
                          east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
                        Nothing -> new_game4 where
                          new_game4 = Game {hunter = hunter, targets = Nothing, obstacles = obstacles,
                          gateways = (Just [((a, b), (c, d))]), north_child = north_child, south_child = south_child,
                          east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle (a, b) game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri coloane)
  | (a, b) == hunter = game
  | a >= randuri || a < 0 || b >= coloane || b < 0 = game
  | otherwise = case targets of 
                  (Just targets) -> if elem (a, b) (map position targets)
                    then game else
                      case gateways of 
                        (Just gateways) -> if elem (a, b) (map fst gateways) || elem (a, b) (map snd gateways) then game
                          else new_game where new_game = Game {hunter = hunter, targets = Just targets,
                          obstacles = (a, b):obstacles, gateways = Just gateways, north_child = north_child,
                          south_child = south_child, east_child = east_child, west_child = west_child, randuri = randuri,
                          coloane = coloane}
                        Nothing -> new_game2 where new_game2 = Game {hunter = hunter, targets = Just targets,
                        obstacles = (a, b):obstacles, gateways = Nothing, north_child = north_child, south_child = south_child,
                        east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
                  Nothing -> case gateways of
                        (Just gateways) -> if elem (a, b) (map fst gateways) || elem (a, b) (map snd gateways) then game
                          else new_game3 where new_game3 = Game {hunter = hunter, targets = Nothing, obstacles = (a, b):obstacles,
                          gateways = Just gateways, north_child = north_child, south_child = south_child, east_child = east_child,
                          west_child = west_child, randuri = randuri, coloane = coloane}
                        Nothing -> new_game4 where new_game4 = Game {hunter = hunter, targets = Nothing,
                        obstacles = (a, b):obstacles, gateways = Nothing, north_child = north_child, south_child = south_child,
                        east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove (a, b) game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri coloane)
  | (a < 0) || (b < 0) || (a >= randuri) || (b >= coloane) = Nothing 
  | not (elem (a, b) obstacles) = case gateways of
                                                            Just gateways ->
                                                              if not ((elem (a, b) (map fst gateways))
                                                              || (elem (a, b) (map snd gateways)))
                                                              then Just (a, b) else 
                                                                (if elem (a, b) (map fst gateways) then
                                                                lookup (a, b) gateways else
                                                                lookup (a, b) (map (uncurry (flip (,))) gateways))
                                                            Nothing -> Just (a, b)
                                                      
  | elem (a, b) obstacles = Nothing
  | otherwise = Nothing 
{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

auxiliar (a, b) game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri coloane) dir =
    case var of
        Nothing -> case gateways of
    	            Just gateways -> case move of 
    	            					Just move -> Target {position = move, behavior = param}
    	                    			where move = attemptMove (a, b) game 
                    Nothing -> Target {position = (a, b), behavior = param}
                    where param = case dir of
    	                             1 -> goEast
    	                             2 -> goWest
    	                             3 -> goNorth
    	                             4 -> goSouth 
    	Just var -> Target {position = var, behavior = param} where param = case dir of
    	                    				                                    1 -> goEast
    	                    				                                    2 -> goWest
    	                    				                                    3 -> goNorth
    	                    				                                    4 -> goSouth 
    where var = case dir of
    	            1 -> attemptMove (a, b + 1) game
    	            2 -> attemptMove (a, b - 1) game
    	            3 -> attemptMove (a - 1, b) game
    	            4 -> attemptMove (a + 1, b) game

goEast :: Behavior
goEast (a, b) game = auxiliar (a, b) game 1
{-
    *** TODO ***
    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest (a, b) game = auxiliar (a, b) game 2
{-
    *** TODO ***
    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth (a, b) game = auxiliar (a, b) game 3
{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth (a, b) game = auxiliar (a, b) game 4
{-
    *** TODO ***
    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
-- (x, y) = pozitia curenta a targetului  
bounce :: Int -> Behavior
bounce a (x, y) game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri coloane) =
	case var of
    	Nothing -> case new_pos of 
    				Just new_pos -> Target {position = new_pos, behavior = bounce (-a)}
    				where new_pos = case a of
    					1 -> attemptMove (x - 1, y) game
    					-1 -> attemptMove (x + 1, y) game
    	Just var -> Target {position = var, behavior = bounce a}
    	where var = case a of 
    				1 -> attemptMove (x + 1, y) game
    				-1 -> attemptMove (x - 1, y) game

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
newtargets targets game acc = if targets == [] then acc else
	newtargets (tail targets) game (((behavior (head targets)) (position (head targets)) game):acc) 
-- functie care intoarce lista de target-uri actualizata 

moveTargets :: Game -> Game
moveTargets game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri coloane) =
	case targets of 
		Nothing -> game
		Just targets -> Game {hunter = hunter, targets = Just (newtargets targets game []), obstacles = obstacles, gateways = gateways,
						north_child = north_child, south_child = south_child, east_child = east_child, west_child = west_child,
						randuri = randuri, coloane = coloane} 
 {-   *** TODO ***
    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (a, b) target@(Target (c, d) behav)
   | (c, d) == (a, b + 1) = True
   | (c, d) == (a, b - 1) = True
   | (c, d) == (a + 1, b) = True
   | (c, d) == (a + 1, b) = True
   | otherwise = False 
{-														     
    *** TODO ***
    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir param
  game@(Game (a, b) targets obstacles gateways north_child south_child east_child west_child randuri coloane) =
	case param of
	   False -> if try2 == Nothing then game else 
	   			  case try2 of 
	   				Just try2 -> Game {hunter = try2, targets = targets, obstacles = obstacles, gateways = gateways,
	                              north_child = north_child, south_child = south_child, east_child = east_child, west_child = west_child,
	                              randuri = randuri, coloane = coloane}
	   True -> if targets /= Nothing
	   			then case new_targets of
	   				    Just new_targets ->
	   				        case try of
	   				           Just try -> Game {hunter = try, targets = Just (new_targets \\ (map fst (filter (\x -> snd x ==
	   				           	True) (zip new_targets (map (isTargetKilled try) new_targets))))), obstacles = obstacles,
	   				            gateways = gateways, north_child = north_child, south_child = south_child,
	   				            east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
	   				           Nothing -> 
	   				              case new_targets1 of 
	   				              	Just new_targets1 -> Game {hunter = (a, b), targets = Just (new_targets1 \\ (map fst (filter (\x -> snd x ==
	   				           	         True) (zip new_targets1 (map (isTargetKilled (a, b)) new_targets1))))), obstacles = obstacles,
	   				                     gateways = gateways, north_child = north_child, south_child = south_child,
	   				                     east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}
        		else Game {hunter = pos, targets = targets, obstacles = obstacles, gateways = gateways,
             		 north_child = north_child, south_child = south_child, east_child = east_child,
                     west_child = west_child, randuri = randuri, coloane = coloane}
                where game@(Game (x, y) new_targets new_obstacles new_gateways new_north_child new_south_child new_east_child new_west_child new_randuri new_coloane) = 
                     	case targets of
                            Just targets -> moveTargets (Game {hunter = pos,
	                 					targets = Just (targets \\ (map fst (filter (\x -> snd x == True) (zip targets (map (isTargetKilled pos) targets))))),
	                 					obstacles = obstacles, gateways = gateways, north_child = north_child, south_child = south_child,
	                 					east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane})
    where pos = case dir of
                  North -> (a - 1, b)
                  South -> (a + 1, b)
                  East -> (a, b + 1)
                  West -> (a, b - 1)
          try =  (attemptMove pos game)
          try2 = try
          (Game (x, y) new_targets1 new_obstacles new_gateways new_north_child new_south_child new_east_child new_west_child new_randuri new_coloane)
	   				                = case targets of
	   				                     Just targets -> (moveTargets (Game {hunter = (a, b),
	                 				        targets = Just (targets \\ (map fst (filter (\x -> snd x == True) (zip targets (map (isTargetKilled (a, b)) targets))))),
	                 					    obstacles = obstacles, gateways = gateways, north_child = north_child, south_child = south_child,
	                 					    east_child = east_child, west_child = west_child, randuri = randuri, coloane = coloane}))
{-
    ***  TODO ***
    Verifică dacă mai există Target-uri pe tabla de joc.  
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri coloane) =
	if targets /= Nothing then True else False 

{-
    *** BONUS TODO ***
    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***       
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game@(Game (a, b) targets obstacles gateways north_child south_child east_child west_child randuri coloane) =
    	zip [West, South, East, North] (map helper [West, South, East, North]) where
    		helper dir = advanceGameState dir False game 
    {-
        *** TODO *** 
        Verifică dacă starea curentă este una în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game@(Game (a, b) targets obstacles gateways north_child south_child east_child west_child randuri coloane) =
    	case targets of
    		Nothing -> False
    		Just targets -> if elem True (map (isTargetKilled (a, b)) targets) then True else False   

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game@(Game (a, b) targets obstacles gateways north_child south_child east_child west_child randuri coloane) = 
    	case targets of
    		Just targets -> case var of
    			              Just var -> hEuclidean (a, b) pos
    			              where var = (lookup True (zip (map (isTargetKilled (a, b)) targets) targets))
    			                    (Target pos behav) = case var of
    			                    						Just var -> var 
{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
