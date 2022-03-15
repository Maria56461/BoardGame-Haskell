import Data.List 

type Position = (Int, Int)

data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

type Behavior = Position -> Game -> Target

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

emptyGame :: Int -> Int -> Game
emptyGame a b = new_game where new_game = Game {hunter = (1, 1), targets = Nothing,
obstacles = concat [[ (x, y) | x<-[1..(a - 2)], y<-[0, (b - 1)]], [(x, y) | x<-[0, (a - 1)], y<-[0..(b - 1)]]], gateways = Nothing,
north_child = Nothing, south_child = Nothing, east_child = Nothing, west_child = Nothing, randuri = a, coloane = b}

gameAsString :: Game -> String
gameAsString game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri
  coloane) = foldr (++) [] (map (show_particular game) [ (x, y) | x<-[0..(randuri - 1)], y<-[0..(coloane - 1)] ])

instance Show Game where
    show = gameAsString

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

addGateway :: (Position, Position) -> Game -> Game
addGateway ((a, b), (c, d))
  game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri coloane)
  | elem (a, b) obstacles || elem (c, d) obstacles = game
  | (a >= randuri) || (c >= randuri) || (a < 0) || (c < 0) = game
  | (b >= coloane) || (d >= coloane) || (b < 0) || (d < 0) = game
  | ((a, b) == hunter) || ((c, d) == hunter) = game 
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
attemptMove pos game@(Game hunter targets obstacles gateways north_child south_child east_child west_child randuri coloane)
  | (pos /= hunter) && (not (elem pos obstacles)) = case targets of
  	                                                  Just targets -> if elem pos (map position targets) then Nothing else
  	                                                  	case gateways of
  	                                                  		Just gateways -> if not (elem pos (map fst gateways) ||
  	                                                  		  elem pos (map snd gateways)) then Just pos else 
  	                                                  		  	if elem pos (map fst gateways) then
  	                                                  		  	lookup pos (map (uncurry (flip (,))) gateways) else
  	                                                  		  	lookup pos gateways
  	                                                  		Nothing -> Just pos
  	                                                  Nothing -> case gateways of
  	                                                  		Just gateways -> if not (elem pos (map fst gateways) ||
  	                                                  		  elem pos (map snd gateways)) then Just pos else 
  	                                                  		  	if elem pos (map fst gateways) then lookup pos gateways else
  	                                                  		  		lookup pos (map (uncurry (flip (,))) gateways)
  	                                                  		Nothing -> Just pos
  | elem pos obstacles = Nothing
  | otherwise = Nothing 






