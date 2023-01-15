type Location = String
type Map = [(Location, Location)]

type Object = String
type Contents = [(Location, [Object])]

type Game = (Location, [Object], Contents)

type Event = Game -> Game

type Dialogue = String
--------------------------------------------------------

exitWords :: [String]
exitWords = ["exit","EXIT", "quit","QUIT", "q"]

start :: Game
start = ("A campsite", [], _contents)

_contents :: Contents
_contents = 
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

---------------------------------------------------------------------------
-----------------------------------ENEMY-----------------------------------
---------------------------------------------------------------------------

type Race = String
type Name = String
type Attack = Int
type Health = Int

type Player = (Health, [Object])

type Enemy = (Race, Health, [Attack])

type Fight = (Enemy, Player)

balrog :: Enemy
balrog = ("Balrog", 25, [2, 5, 3])

testPlayer :: Player
testPlayer = (50, ["Iron sword"])

printPlayer :: Player -> IO()
printPlayer (health, items)
    = do
        if health <= 0 then putStrLn ("You have died :(")
        else do
                putStrLn "-------------------\n"
                putStrLn "Player:"
                putStr "HP: "
                putStr (show health)
                putStr "/"
                putStrLn (show startingHealth)
                putStr "Items: "
                print items

printEnemy :: Enemy -> IO()
printEnemy (race, enemyHealth, attacks)
    = do
        if enemyHealth <= 0 then putStrLn ("The " ++ race ++ " has been killed!")
        else do
                putStrLn "-------------------\n"
                putStr "Race: "
                putStrLn race
                putStr "Attacks: "
                print attacks
                putStr "Health: "
                print enemyHealth

fight:: Fight -> IO()
fight ((race, enemyHealth, attacks), (playerHealth, items))
    = do
        if enemyHealth <= 0 then do putStrLn ("\nThe " ++ race ++ " has been killed!")
        else if playerHealth <= 0 then do putStrLn "You have been killed!"
        else do
                printEnemy (race, enemyHealth, attacks)
                printPlayer (playerHealth, items)
                putStrLn "What do you do?"
                option <- getLine
                if option == "attack" then do
                                            putStr "The "
                                            putStr race
                                            putStr " hit you for 3 damage!"
                                            fight ((race, enemyHealth - 5, attacks), (playerHealth - 3, items))
                else fight((race, enemyHealth - 5, attacks), (playerHealth - 3, items))
        



--------------------------------------------------------------------------
-----------------------------------GAME-----------------------------------
--------------------------------------------------------------------------

startingHealth :: Int
startingHealth = 50

startingAttack :: Int
startingAttack = 1

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
        else if choice == "help" then do 
                                        putStrLn "Here is a list of commands you can use"
                                        putStrLn "1. take - takes the items you can see"
                                        putStrLn "2. exit - exits the application"
                                        game (location, objects, contents)
        else do 
                putStrLn "\n\n"
                game (location, objects, contents)



--------------------------------------------------------------------------
---------------------------------DIALOGUE---------------------------------
--------------------------------------------------------------------------