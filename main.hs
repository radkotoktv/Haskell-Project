type Location = String
type Map = [(Location, Location)]

type Object = String
type Contents = [(Location, [Object])]

findContents :: Location -> Contents -> [Object]
findContents _ []   = []
findContents l ((x,xs):content)
    | l == x    = xs
    | otherwise = findContents l content

addContents :: Location -> [Object] -> Contents -> Contents
addContents _ _ [] = []
addContents l ys ((x,xs):content)
    | x == l    = (x, xs ++ ys):content
    | otherwise = (x,xs):addContents l ys content

------------------------------------------------------

removeOne :: Eq a => [a] -> a -> [a]
removeOne [] _ = []
removeOne (x:xs) y
    | x == y   = xs
    | otherwise = x:removeOne xs y

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs [] = xs
removeAll xs (y:ys) = removeAll (removeOne xs y) ys

removeContents :: Location -> [Object] -> Contents -> Contents
removeContents _ _ [] = []
removeContents l ys ((x,xs):rest)
    | x == l    = (x, removeAll xs ys):rest
    | otherwise = (x, xs):removeContents l ys rest

--------------------------------------------------------

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =  lower ++ x:higher
  where lower  = quickSort [y | y<-xs, y <= x]
        higher = quickSort [y | y<-xs, y > x]

equal :: [Object] -> [Object] -> Bool
equal xs ys = quickSort xs == quickSort ys

----------------------------------------------------------

type Game = (Location, [Object], Contents)

type Event = Game -> Game

--------------------------------------------------------

leaveItems :: [Object] -> Event
leaveItems [] (l, o, c) = (l, o, c)
leaveItems objects (l, o, c)
    | null [y | y <- o, y `elem` objects] = (l, o, c)
    | otherwise = (l, (removeAll o objects), (addContents l objects c))

takeItems :: [Object] -> Event
takeItems [] (l, o, c) = (l, o, c)
takeItems objects (l, o, c)
    | null [x | y <- c , fst(y) == l , x <- snd(y) , x `elem` objects] = (l, o, c)
    | otherwise = (l, o ++ objects, removeContents l objects c)

exitWords :: [String]
exitWords = ["exit","EXIT","quit","QUIT"]

start :: Game
start = ("A campsite", [], idkrofl)

idkrofl :: Contents
idkrofl = 
    [ 
        ("A Campsite", ["Iron sword"]),
        ("The forest", []),
        ("The mines", ["Diamond ore", "Gold ore"])
    ]

_map :: Map
_map = [(c, f), (c, m), (m, f)]
    where
        c = "A campsite"
        f = "The forest"
        m = "The mines"

accessible :: Location -> [Location]
accessible l = [x | (x, y) <- _map , y == l] ++ [y | (x, y) <- _map , x == l]