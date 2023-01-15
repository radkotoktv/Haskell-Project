type Location = String
type Map = [(Location, Location)]

type Object = String
type Contents = [(Location, [Object])]

type Game = (Location, [Object], Contents)

type Event = Game -> Game

--------------------------------------------------------

exitWords :: [String]
exitWords = ["exit","EXIT", "quit","QUIT", "q"]

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

removeOne :: Eq a => [a] -> a -> [a]
removeOne [] _ = []
removeOne (x:xs) y
    | x == y   = xs
    | otherwise = x:removeOne xs y

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs [] = xs
removeAll xs (y:ys) = removeAll (removeOne xs y) ys

removeFromLocation :: Location -> [Object] -> Contents -> Contents
removeFromLocation _ _ [] = []
removeFromLocation l ys ((x,xs):rest)
    | x == l    = (x, removeAll xs ys):rest
    | otherwise = (x, xs):removeFromLocation l ys rest

getItems :: Location -> Contents -> [Object]
getItems l contents = concat [y | (x, y) <- contents, x == l]

enumerate :: Int -> [String] -> String
enumerate n xs = unlines [ "  " ++ show i ++ ". " ++ x | (i,x) <- zip [n..] xs ]

accessible :: Location -> [Location]
accessible l = [x | (x, y) <- _map , y == l] ++ [y | (x, y) <- _map , x == l]

game :: Game -> IO()
game (location, objects, contents)
    = do
        let canGo = accessible location
        let inventory = objects
        let seenItems = getItems location contents
        putStrLn "-------------------\n"
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
        else if choice `elem` locations then do game (choice, objects, contents)
        else if choice == "take" then do game (location, objects ++ getItems location contents, removeFromLocation location seenItems contents)                         
        else do 
                putStrLn "\n\n"
                game (location, objects, contents)