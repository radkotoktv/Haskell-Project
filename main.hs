data Item
    = Item
    {
        _itemName :: String,
        _itemDescription :: String,
        _itemSize :: Int,
        _itemWeight :: Int,
        _itemPrice :: Int
    }
    deriving(Eq, Show)

data Exit
    = Exit
    {
        _exitName :: String,
        _exitDescription :: String,
        _exitFrom :: Room,
        _exitTo :: Room
    }
    deriving(Eq, Show)

data Room
    = Room
    {   
        _roomName        :: String,
        _roomDescription :: String,
        _roomItems       :: [Item],
        _roomExits       :: [Exit]
    }
    deriving(Eq, Show)

data World
    = World
    {
        _worldRooms :: [Room],
        _worldItems :: [Item],
        _worldExits :: [Exit],
        _playerRoom :: Room
    }
    deriving(Eq, Show)

room :: Room
room = Room{
_roomName = "auhfgsduhfsdjhfhsdjfhsdjfhsdk",
_roomDescription = "abbdsifjifsdjidfs"

}

renderRoom :: Room -> String
renderRoom (Room name desc _ _) =
    name ++ "  " ++ desc
























fact :: (Eq t, Num t) => t -> t
fact 0 = 1
fact n = n * fact (n - 1)