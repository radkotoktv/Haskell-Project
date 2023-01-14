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
        ("A campsite", ["Iron sword"]),
        ("The forest", []),
        ("The mines", ["Diamond ore", "Gold ore"])
    ]

locations :: [String]
locations = ["A campsite", "The forest", "The mines"]

_map :: Map
_map = [(c, f), (c, m), (m, f)]
    where
        c = "A campsite"
        f = "The forest"
        m = "The mines"

getItems :: Location -> [Object]
getItems l = concat [y | (x, y) <- idkrofl, x == l]

enumerate :: Int -> [String] -> String
enumerate n xs = unlines [ "  " ++ show i ++ ". " ++ x | (i,x) <- zip [n..] xs ]

accessible :: Location -> [Location]
accessible l = [x | (x, y) <- _map , y == l] ++ [y | (x, y) <- _map , x == l]

findLocation _ [] = []
findLocation l (x:xs)
    = if x == l then x
    else findLocation l xs


game :: Game -> IO()
game (location, objects, contents)
    = do
        let canGo = accessible location
        let inventory = objects
        let seenItems = getItems location
        putStrLn "-------------------"
        putStr "You are in: "
        putStrLn location
        putStrLn "You have: "
        putStr (enumerate 1 inventory)
        putStrLn "You can go to: "
        putStr (enumerate 1 canGo)
        putStrLn "The items you see are: "
        putStrLn (enumerate 1 seenItems)
        choice <- getLine
        if choice `elem` exitWords then do putStrLn "Thank you for playing!"
        else if choice `notElem` locations then game (location, objects, contents)
        else do 
            print (enumerate 1 (getItems "The mines"))
            game (findLocation choice locations, objects, contents)