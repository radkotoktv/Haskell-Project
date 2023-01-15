type Location = String
type Map = [(Location, Location)]

type Contents = [(Location, [Weapon])]

type Game = (Location, [Weapon], Contents)

type Event = Game -> Game

type Dialogue = String
--------------------------------------------------------

exitWords :: [String]
exitWords = ["exit","EXIT", "quit","QUIT", "q"]

start :: Game
start = ("Campsite", [], _contents)

_contents :: Contents
_contents = 
    [ 
        ("Campsite", []),
        ("Forest", [("Wooden sword", 5)]),
        ("Mine", [("Pickaxe", 4)]),
        ("Swamp", [("Whip", 4), ("Shovel", 3)]),
        ("Lake", [("Fishing rod", 1)]),
        ("Mountain", [("Shield", 2)]),
        ("Cabin", [("Shotgun", 10)])
    ]

locations :: [String]
locations = ["Campsite", "Forest", "Mine", "Swamp", "Lake", "Mountain", "Cabin"]

_map :: Map
_map = [(campsite, forest), (forest, mines), (forest, swamp), (forest, mountain), (swamp, lake), (swamp, mountain), (lake, cabin), (mountain, cabin)]
    where
        campsite = "Campsite"
        forest = "Forest"
        mines = "Mine"
        swamp = "Swamp"
        lake = "Lake"
        mountain = "Mountain"
        cabin = "Cabin"

removeOne :: Eq a => [a] -> a -> [a]
removeOne [] _ = []
removeOne (x:xs) y
    | x == y   = xs
    | otherwise = x:removeOne xs y

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs [] = xs
removeAll xs (y:ys) = removeAll (removeOne xs y) ys

removeFromLocation :: Location -> [Weapon] -> Contents -> Contents
removeFromLocation _ _ [] = []
removeFromLocation l ys ((x,xs):rest)
    | x == l    = (x, removeAll xs ys):rest
    | otherwise = (x, xs):removeFromLocation l ys rest

getItems :: Location -> Contents -> [Weapon]
getItems l contents = concat [y | (x, y) <- contents, x == l]

enumerate :: [String] -> String
enumerate xs = unlines ["  " ++ show i ++ ". " ++ x | (i,x) <- zip [1..] xs ]

getNamesOfWeapons :: [Weapon] -> [String]
getNamesOfWeapons xs = map fst xs

getStatsOfWeapons :: [Weapon] -> [Int]
getStatsOfWeapons xs = map snd xs

accessible :: Location -> [Location]
accessible l = [x | (x, y) <- _map , y == l] ++ [y | (x, y) <- _map , x == l]

---------------------------------------------------------------------------
-----------------------------------ENEMY-----------------------------------
---------------------------------------------------------------------------

type Race = String
type Name = String
type Attack = Int
type Health = Int

type Player = (Health, [Weapon])

type Enemy = (Race, Health, [Attack])

type Fight = (Enemy, Player)

type Weapon = (String, Int)


balrog :: Enemy
balrog = ("Balrog", 25, [2, 5, 3])

testPlayer :: Player
testPlayer = (50, 
                [
                    ("Iron sword", 5),
                    ("Shield", 2)
                ]
            )

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
                putStrLn "Items: "
                putStrLn (enumerate (getNamesOfWeapons items))

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
                putStr "HP: "
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
                                            putStr " hit you for 3 damage!\n"
                                            fight ((race, enemyHealth - startingAttack, attacks), (playerHealth - 3, items))
                else do
                        putStr "The "
                        putStr race
                        putStr " hit you for 3 damage!\n"                        
                        fight((race, enemyHealth, attacks), (playerHealth - 3, items))
        



--------------------------------------------------------------------------
-----------------------------------GAME-----------------------------------
--------------------------------------------------------------------------

startingHealth :: Int
startingHealth = 50

startingAttack :: Int
startingAttack = 4

game :: Game -> IO()
game (location, weapons, contents)
    = do
        let canGo = accessible location
        let inventory = weapons
        let seenItems = getItems location contents
        putStrLn "-------------------\n"
        putStr "You are in: "
        putStrLn location
        putStrLn "You have: "
        putStrLn (enumerate (getNamesOfWeapons inventory))
        putStrLn "You can go to: "
        putStr (enumerate canGo)
        putStrLn "The items you see are: "
        putStrLn (enumerate (getNamesOfWeapons seenItems))
        choice <- getLine
        if choice `elem` exitWords then do putStrLn "Thank you for playing!"
        else if choice `elem` canGo then do game (choice, weapons, contents)
        else if choice == "take" then do game (location, weapons ++ getItems location contents, removeFromLocation location seenItems contents)                         
        else if choice == "help" then do 
                                        putStrLn "Here is a list of commands you can use"
                                        putStrLn "1. take - takes the items you can see"
                                        putStrLn "2. exit - exits the application"
                                        game (location, weapons, contents)
        else if choice == "fight" then do 
                                        fight (balrog, (startingHealth, weapons))
                                        game (location, weapons, contents)
        else do 
                putStrLn "\n\n"
                game (location, weapons, contents)





--------------------------------------------------------------------------
---------------------------------DIALOGUE---------------------------------
--------------------------------------------------------------------------