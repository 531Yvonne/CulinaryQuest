module Player where

import Item
import Room

data Player = Player
  { inventory :: [ItemName],
    maxWeight :: Integer,
    location :: RoomName
  }
  deriving (Show, Eq)

-- Return a Player type data with new item added into inventory.
addItem :: ItemName -> Player -> Player
addItem newItemName currentPlayer =
  currentPlayer {inventory = newItemName : inventory currentPlayer}

-- Return a Player type data with specified item deleted from inventory.
removeItem :: ItemName -> Player -> Player
removeItem itemToRemove currentPlayer =
  currentPlayer
    { inventory = filter (/= itemToRemove) $ inventory currentPlayer
    }

-- Create the game player.
you :: Player
you =
  Player
    { inventory = [],
      maxWeight = 100,
      location = Reception
    }