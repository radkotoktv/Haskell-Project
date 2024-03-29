import System.Random
import System.Exit

type Location = String
type Map_ = [(Location, Location)]

type Contents = [(Location, [Weapon])]

type Game = (Location, [Weapon], Contents)

type Event = Game -> Game

type Dialogue = String
--------------------------------------------------------

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

_weapons :: [Weapon]
_weapons = [("Wooden sword", 5), ("Pickaxe", 4), ("Whip", 4), ("Shovel", 3), ("Fishing rod", 1), ("Shield", 2), ("Shotgun", 10), ("Instakill", 1000)]

locations :: [Location]
locations = ["Campsite", "Forest", "Mine", "Swamp", "Lake", "Mountain", "Cabin"]

_map :: Map_
_map = [(campsite, forest), (forest, mines), (forest, swamp), (forest, mountain), (swamp, lake), (swamp, mountain), (lake, cabin), (mountain, cabin)]
    where
        campsite = "Campsite"
        forest = "Forest"
        mines = "Mine"
        swamp = "Swamp"
        lake = "Lake"
        mountain = "Mountain"
        cabin = "Cabin"

--------------------------------------------------------------------------------
--------------------------------HELPER FUNCTIONS--------------------------------
--------------------------------------------------------------------------------

removeOne :: Eq a => [a] -> a -> [a]
removeOne [] _ = []
removeOne (x:xs) y
    | x == y   = xs
    | otherwise = x:removeOne xs y

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs [] = xs
removeAll xs (y:ys) = removeAll (removeOne xs y) ys

removeFromLocation :: Location -> [Weapon] -> Contents -> Contents -- removes all elements from a given location
removeFromLocation _ _ [] = []
removeFromLocation l ys ((x,xs):rest)
    | x == l    = (x, removeAll xs ys):rest
    | otherwise = (x, xs):removeFromLocation l ys rest

getItems :: Location -> Contents -> [Weapon] -- gets the items from a given location
getItems l contents = concat [y | (x, y) <- contents, x == l]

enumerate :: [String] -> String -- ex. ["a", "b", "c"] ----> 1. a  \n   2. b  \n   3. c
enumerate xs = unlines ["  " ++ show i ++ ". " ++ x | (i,x) <- zip [1..] xs ]

enumerateWeapons :: [Weapon] -> String
enumerateWeapons [] = []
enumerateWeapons xs = unlines ["  " ++ show i ++ ". " ++ a ++ " (" ++ show b ++ " damage)"| (i, (a, b)) <- zip [1..] xs ]

accessible :: Location -> [Location] -- locations you can go to from the current one (gets information from "_map")
accessible l = [x | (x, y) <- _map , y == l] ++ [y | (x, y) <- _map , x == l]

getStatsOfAWeapon :: String -> [Weapon] -> Int
getStatsOfAWeapon _ [] = 0
getStatsOfAWeapon x (z:zs) 
    = if x == fst(z) then snd(z)
    else getStatsOfAWeapon x zs

getRandomElement :: [a] -> (StdGen -> (a, StdGen)) -- gives a random element of a list
getRandomElement xs = \gen -> let (index, newGen) = randomR (0, length xs - 1) gen
                              in (xs !! index, newGen)

---------------------------------------------------------------------------
-----------------------------------FIGHT-----------------------------------
---------------------------------------------------------------------------

type Race = String
type Name = String
type Attack = Int
type Health = Int

type Player = (Health, [Weapon])

type Enemy = (Race, Health, [Attack])

type Fight = (Enemy, Player)

type Weapon = (String, Int)

testPlayer :: Player
testPlayer = (50, 
                [
                    ("Wooden sword", 5),
                    ("Shield", 2),
                    ("Instakill", 1000)
                ]
            )

testEnemy :: Enemy
testEnemy = ("testEnemy", 30, [4, 1, 3])

printPlayer :: Player -> IO() -- prints out all the information about the player
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
                putStrLn "Weapons: "
                putStrLn (enumerateWeapons items)

printEnemy :: Enemy -> IO() -- prints out all the information about the given enemy
printEnemy (race, enemyHealth, attacks)
    = do
        if enemyHealth <= 0 then putStrLn $ "The " ++ race ++ " has been killed!"
        else do
                putStrLn "-------------------\n"
                putStr "Race: "
                putStrLn race
                putStr "Attacks: "
                print attacks
                putStr "HP: "
                print enemyHealth

fight:: Fight -> IO() -- a loop which simulates a fight between an enemy and the player
fight ((race, enemyHealth, attacks), (playerHealth, items))
    = do
        if enemyHealth <= 0 then do putStrLn $ "\nThe " ++ race ++ " has been killed!"
        else if playerHealth <= 0 then do 
                                        putStrLn "You have been killed!"
                                        exitSuccess
        else do
                printEnemy (race, enemyHealth, attacks)
                printPlayer (playerHealth, items)
                putStrLn "What weapon do you use? (name of the weapon)"
                weaponOfChoice <- getLine
                if weaponOfChoice `elem` exitWords then do 
                                                    putStrLn "Thank you for playing!"
                                                    exitSuccess
                else do
                        gen <- newStdGen
                        let (randomElem, _) = getRandomElement attacks gen
                        putStrLn $ "The " ++ race ++ " hit you for " ++ show randomElem ++ " damage!\n"  
                        fight ((race, enemyHealth - (getStatsOfAWeapon weaponOfChoice _weapons), attacks),(playerHealth - randomElem, items))
        

--------------------------------------------------------------------------
-----------------------------------GAME-----------------------------------
--------------------------------------------------------------------------

exitWords :: [String]
exitWords = ["exit","EXIT", "quit","QUIT", "q"]

balrog :: Enemy
balrog = ("Balrog", 25, [2, 5, 3])

startingHealth :: Int
startingHealth = 30

start :: Game -- "game start" starts playing
start = ("Campsite", [], _contents)

game :: Game -> IO() -- our game ("game start" to start playing!)
game (location, weapons, contents)
    = do
        let canGo = accessible location
        let inventory = weapons
        let seenItems = getItems location contents
        putStrLn "-------------------\n"
        putStr "You are in: " -- Your current location
        putStrLn location
        putStrLn "You have: " -- The items in your inventory
        putStrLn $ enumerateWeapons inventory
        putStrLn "You can go to: " -- To go there type the name of the location (ex. Forest)
        putStr $ enumerate canGo
        putStrLn "The items you see are (take): " -- take - takes all the items from the current location
        putStrLn $ enumerateWeapons seenItems
        choice <- getLine
        if choice `elem` exitWords then do 
                                            putStrLn "Thank you for playing!" -- quit the game
                                            exitSuccess
        else if choice == "Cabin" then do -- there is an enemy at "Cabin"
                                        putStrLn "You have encountered an enemy! What do you do? (fight/run)"
                                        action <- getLine
                                        if action == "fight" then do 
                                                                    fight (balrog, (startingHealth, inventory)) -- starts a fight with the enemy
                                                                    game (choice, inventory, contents) -- goes to "Cabin"
                                        else do game (location,inventory, contents) -- goes back to the current location if you choose anything other than "fight"
        else if choice `elem` canGo then do game (choice, inventory, contents) -- go to a different location
        else if choice == "take" then do game (location, inventory ++ getItems location contents, removeFromLocation location seenItems contents) -- take all items from the current location               
        else do -- if the input is invalid, nothing happens
                putStrLn "\n\n"
                game (location, inventory, contents)