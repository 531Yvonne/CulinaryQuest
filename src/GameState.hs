module GameState where

import Control.Exception
import Data.List ()
import Data.Map qualified as M
import Direction
import Item
import Player
import Room

-- Type alias for Map type data (RoomName as key, Room as value)
type GameMap = M.Map RoomName Room

data GameState = GameState
  { message :: Maybe String,
    gmap :: GameMap,
    universe :: Universe,
    player :: Player,
    maxSlipperyMoves :: Integer
  }
  deriving (Show)

-- Take a list of "Room"s and return a GameMap (Map RoomName Room) type data
mkMap :: [Room] -> GameMap
mkMap rooms = M.fromList $ fmap (\x -> (rname x, x)) rooms

-- A constant storing game map of all rooms
gameMap :: GameMap
gameMap = mkMap allRooms

-- Initiate a GameState
initialState :: GameState
initialState =
  GameState
    { message = Nothing,
      gmap = gameMap,
      universe = univ,
      player = you,
      -- Variable controls the # of moves a player can hold the slippery item
      maxSlipperyMoves = 2
    }

data KeyError = KeyError
  deriving (Show)

instance Exception KeyError

-- Helper function for getObject, return KeyError when no such object
getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u =
  case M.lookup iname u of
    Just obj -> obj
    Nothing -> throw KeyError

-- Look up certain item in a given GameState
-- return the Item infor when found, raise KeyError when not exist.
getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

-- Helper function for getRoom
getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp =
  case M.lookup rname mp of
    Just room -> room
    Nothing -> throw KeyError

-- Look up a `Room` in GameState based on its `RoomName`
-- return the Room infor when found, otherwise raise KeyError
getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

-- getObject and getRoom functions are defined to help in developing
-- to guarantee the `KeyError` exception will never get thrown when playing
-- The rule is:
-- Don't call `getObject` or `getRoom` on a string literal while developing
-- Should only call `getObject` or `getRoom` on `ItemName` or `RoomName`

-- Take a room name, a room, and a game map as inputs
-- Return a new game map with old room replaced by new input room
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap oldRoomName newRoom oldGameMap =
  -- remove old room with given name and insert new room
  M.insert (rname newRoom) newRoom $ M.delete oldRoomName oldGameMap

-- Take a `String` and a `GameState` as an input
-- Return the same `GameState` with its message field replaced
setMessage :: String -> GameState -> GameState
setMessage "" oldGS = oldGS {message = Nothing}
setMessage newMessage oldGS = oldGS {message = Just newMessage}

-- Take a `GameState` as an input
-- Return the inventory of that `GameState`'s player.
currentInventory :: GameState -> [ItemName]
currentInventory currentGS = inventory $ player currentGS

-- Takes a `GameState` as an input, and return the room the `Player` is in.
currentRoom :: GameState -> Room
currentRoom currentGS = getRoom (location $ player currentGS) currentGS

-- or without using getRoom function
-- currentRoom currentGS = (gmap currentGS) M.! (location $ player currentGS)

-- Take a game state as input and Return the list of itemnames in the room
-- where the player is, in that game state.
nearbyObjects :: GameState -> [ItemName]
nearbyObjects currentGS = objects $ currentRoom currentGS

-- takeItem 2.0 (Implemented validation on the item)
-- Let you, the player pick up an item from the room you're in
-- and add it to your inventory.
takeItem :: ItemName -> GameState -> GameState
takeItem itemname currentGS =
  let check1 = alreadyHaveTakeCheck itemname currentGS
      check2 = inRoomTakeCheck itemname currentGS
      check3 = weightCheck itemname currentGS
   in case (check1, check2, check3) of
        (Left s, _, _) -> setMessage s currentGS
        (_, Left s, _) -> setMessage s currentGS
        (_, _, Left s) -> setMessage s currentGS
        (_, _, _) ->
          let updatedPlayer = Player.addItem itemname $ player currentGS
              updatedRoom = Room.removeItem itemname $ currentRoom currentGS
              updatedGMap =
                setRoomMap (location updatedPlayer) updatedRoom $
                  gmap currentGS
              newGS =
                currentGS
                  { gmap = updatedGMap,
                    player = updatedPlayer
                  }
           in setMessage ("You take the " <> show itemname <> ".") newGS

-- dropItem 2.0 (Implemented validation on the item)
-- Let you, the player drop an item to the room you're in
-- and remove it from your inventory.
dropItem :: ItemName -> GameState -> GameState
dropItem itemname currentGS =
  let check1 = anywhereDropCheck itemname currentGS
      check2 = inRoomDropCheck itemname currentGS
   in case (check1, check2) of
        (Left s, _) -> setMessage s currentGS
        (_, Left s) -> setMessage s currentGS
        (_, _) ->
          let updatedPlayer = Player.removeItem itemname $ player currentGS
              updatedRoom = Room.addItem itemname $ currentRoom currentGS
              updatedGMap =
                setRoomMap
                  (location updatedPlayer)
                  updatedRoom
                  $ gmap currentGS
              newGS =
                currentGS
                  { gmap = updatedGMap,
                    player = updatedPlayer
                  }
           in setMessage ("You drop the " <> show itemname <> ".") newGS

-- Let you, the player eat an item from the room you're in
-- Dish can only be eaten when have tableware in inventory
eatItem :: ItemName -> GameState -> GameState
eatItem itemname currentGS =
  let check1 = canEatCheck itemname currentGS
      check2 = haveTablewareCheck currentGS
      check3 = haveFoodCheck itemname currentGS
   in case (check1, check2, check3) of
        (Left s, _, _) -> setMessage s currentGS
        (_, Left s, _) -> setMessage s currentGS
        (_, _, Left s) -> setMessage s currentGS
        (_, _, _) ->
          let updatedItem =
                (getObject itemname currentGS) {consumed = True}
              updatedUniverse =
                M.update (\_ -> Just updatedItem) itemname $ universe currentGS
              updatedPlayer = Player.removeItem itemname $ player currentGS
              currentWeight = maxWeight updatedPlayer
              strongerPlayer = updatedPlayer {maxWeight = currentWeight + 10}
              newGS =
                currentGS
                  { universe = updatedUniverse,
                    player = strongerPlayer
                  }
           in setMessage
                ( "You eat the "
                    <> show itemname
                    <> ", and you have more power to carry heavier item!"
                )
                newGS

-- Take a `GameState` as input.
-- Return total weight of the inventory that GameState's Player is carrying.
inventoryWeight :: GameState -> Integer
inventoryWeight currentGS =
  let inventoryNames = currentInventory currentGS -- get [ItemName]
      itemList = universe currentGS -- get the universe Map (ItemName: Item)
      weightList = map (weight . (itemList M.!)) inventoryNames
   in sum weightList

type Error a = Either String a

-- Check whether tableware is in player's inventory.
-- If the player is not carrying the tableware, return a Left value containing
-- error message saying "You need to carry tableware in order to eat"
-- If carrying, return `Right` value with the input `GameState`.
haveTablewareCheck :: GameState -> Error GameState
haveTablewareCheck currentGS =
  if elem Tableware $ currentInventory currentGS
    then Right currentGS
    else Left ("You need to carry tableware in order to eat.")

-- Check whether the food to eat is in player's inventory.
-- If the player is not carrying the food, return a Left value containing
-- error message saying "You need to take the food in order to eat"
-- If carrying, return `Right` value with the input `GameState`.
haveFoodCheck :: ItemName -> GameState -> Error GameState
haveFoodCheck itemName currentGS =
  if elem itemName $ currentInventory currentGS
    then Right currentGS
    else Left ("You need to take the " <> show itemName <> " in order to eat.")

-- Check whether an item player try to eat is food.
-- Take an `ItemName` and `GameState`
-- If it's not, return a Left value containing
-- error message saying "You can't eat this!!!"
-- If it is, return `Right` value with the input `GameState`.
canEatCheck :: ItemName -> GameState -> Error GameState
canEatCheck itemName currentGS =
  if elem itemName [Tableware, Sake, Cash]
    then Left ("You can't eat the " <> show itemName <> "!!!")
    else Right currentGS

-- Check whether an item player try to pick up is already in player's inventory.
-- Take an `ItemName` and `GameState`
-- If the player is carrying the item, return a Left value containing
-- error message saying "already carrying the item"
-- If not carrying the item, return `Right` value with the input `GameState`.
alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck itemName currentGS =
  if elem itemName $ currentInventory currentGS
    then Left ("You are already carrying the " <> show itemName <> ".")
    else Right currentGS

-- Check whether an item the user try to pick up is in the room to pick up.
-- Take an `ItemName` and `GameState`
-- If the room where the `Player` is contains the item, return a `Right` value
-- containing the input state. Otherwise, return a `Left` value containing
-- an error message saying there is no such item in the room.
inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck itemName currentGS =
  if elem itemName $ nearbyObjects currentGS
    then Right currentGS
    else Left ("There is no " <> show itemName <> " in this room.")

-- Checks whether the `Player` is able to carry a new item
-- Takes an `ItemName` and `GameState`
-- If the total weight the player in the game state is carrying plus the weight
-- of the new item would exceed the player's max weight, return a Left value
-- containing the error message. Otherwise, return a Right value containing the
-- input `GameState`.
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck itemName currentGS =
  if (currentInventoryWeight + itemWeight) > weightLimit
    then Left "That's too much weight for you to carry."
    else Right currentGS
  where
    weightLimit = (maxWeight . player) currentGS
    currentInventoryWeight = inventoryWeight currentGS
    itemWeight = weight $ getObject itemName currentGS

-- Check whether an item is either in the player's inventory
-- or in the room where the player is.
-- The point is to have a catch-all error message when the user want to
-- drop something that is completely off the table for dropping,
-- because it's neither in their inventory nor in the room.
anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck itemName currentGS =
  case ( alreadyHaveTakeCheck itemName currentGS,
         inRoomTakeCheck itemName currentGS
       ) of
    -- Right _ for alreadyHaveTakeCheck means item not in inventory
    -- Left _ for inRoomTakeCheck means item not in current room
    (Right _, Left _) ->
      Left ("What do you mean, drop the \"" <> show itemName <> "\"?")
    (_, _) -> Right currentGS

-- Check whether the item the user want to drop is present in the room,
-- If yes, return a `Left` value with an error message
-- Otherwise, return a `Right` value containing the input `GameState`.
inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck itemName currentGS =
  if elem itemName $ nearbyObjects currentGS
    then Left ("You aren't carrying the " <> show itemName <> ".")
    else Right currentGS

-- Take a `Direction` and a `Room`
-- If there is an exit in that direction in the input room
-- Output the name of the room that you would end up in (wrapped in a `Just`).
-- Otherwise, return `Nothing`.
destinationName :: Direction -> Room -> Maybe RoomName
destinationName direction roomname = lookup direction $ exits roomname

-- Take a `Direction` and `GameState`
-- If direction is achievable, return new gamestate with updated location and
-- a `message` field containing `Just "You go ..."`
-- If direction is impossible, return the input state, with a message
-- `Just "There is no exit in that direction."`.
move :: Direction -> GameState -> GameState
move direction currentGS =
  -- Check whether the destination is valid using function above
  case (destinationName direction $ currentRoom currentGS) of
    Just roomname ->
      currentGS
        { message = Just ("You go " <> (show direction) <> "."),
          player = (player currentGS) {location = roomname}
        }
    Nothing -> setMessage "There is no exit in that direction." currentGS

-- Check current GameState (this function is called after each move command)
-- When the player is carrying slippery item, updates will be made on item
checkSlippery :: GameState -> GameState
checkSlippery currentGS =
  -- Get all carrying slippery items in a list
  let items = fmap (\i -> getObject i currentGS) (currentInventory currentGS)
      slipperyItems = filter (\i -> slippery i) items
   in if slipperyItems == []
        then -- No slippery items in inventory, no additional action
          currentGS
        else foldl processSlipperyItem currentGS slipperyItems

-- Helper function to check current numberOfMoves for slippery items
-- Either update the number or drop the item if the moves reach max
processSlipperyItem :: GameState -> Item -> GameState
processSlipperyItem currentGS item =
  let currentMoves = numberOfMoves item
   in if currentMoves + 1 < (maxSlipperyMoves currentGS)
        then updateSlippery currentMoves (iname item) currentGS
        else dropSlippery (iname item) currentGS

-- updateSlippery
-- Add 1 to the slippery item's numberOfMoves attribute
updateSlippery :: Integer -> ItemName -> GameState -> GameState
updateSlippery currentMoves itemname currentGS =
  let updatedItem =
        (getObject itemname currentGS) {numberOfMoves = currentMoves + 1}
      updatedUniverse =
        M.update (\_ -> Just updatedItem) itemname $ universe currentGS
   in currentGS {universe = updatedUniverse}

-- dropSlippery
-- Similar to dropItem, after drop the item:
-- reset numberOfMoves to 0 and have a slip message
dropSlippery :: ItemName -> GameState -> GameState
dropSlippery itemname currentGS =
  let droppedGS = dropItem itemname currentGS
      updatedItem = (getObject itemname currentGS) {numberOfMoves = 0}
      updatedUniverse =
        M.update (\_ -> Just updatedItem) itemname $ universe currentGS
      newGS = droppedGS {universe = updatedUniverse}
   in setMessage ("You feel something slip out of your plate...") newGS

-- Returns `True` if the `Player` is at `dining room` and carrying the `sake`
-- Otherwise return `False`.
haveWonGame :: GameState -> Bool
haveWonGame gs =
  case (rname $ currentRoom gs, elem Sake $ currentInventory gs) of
    (DiningRoom, True) -> True
    _ -> False

-- Returns `True` if the `Player` takes cash
-- Otherwise return `False`.
tookCash :: GameState -> Bool
tookCash gs = elem Cash $ currentInventory gs

-- Returns `True` if the `Player` eat any poisoned food
-- Otherwise return `False`.
consumedPoisonedFood :: GameState -> Bool
consumedPoisonedFood gs =
  any (\i -> consumed i && poisoned i) (M.elems $ universe gs)