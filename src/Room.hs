module Room where

import Direction
import Item

data RoomName
  = ItalianPizzeria
  | JapaneseSushiBar
  | DiningRoom
  | MexicanTaqueria
  | FrenchPatisserie
  | ChineseHotPotPlace
  | SpanishTapasTavern
  | KoreanBBQGrill
  | Reception
  deriving (Eq, Ord)

-- Write custom show instance for RoomName
instance Show RoomName where
  show :: RoomName -> String
  show ItalianPizzeria = "Italian Pizzeria"
  show JapaneseSushiBar = "Japanese Sushi Bar"
  show DiningRoom = "Dining Room"
  show MexicanTaqueria = "Mexican Taqueria"
  show FrenchPatisserie = "French Patisserie"
  show ChineseHotPotPlace = "Chinese Hot Pot Place"
  show SpanishTapasTavern = "Spanish Tapas Tavern"
  show KoreanBBQGrill = "Korean BBQ Grill"
  show Reception = "Reception Area"

type Exit = (Direction, RoomName)

data Room = Room
  { rname :: RoomName,
    desc :: String,
    exits :: [Exit],
    objects :: [ItemName]
  }
  deriving (Show, Eq)

-- addItem function for adding new item to a Room.
addItem :: ItemName -> Room -> Room
-- get old objects list and append new itemname to the front.
addItem itemname room = room {objects = itemname : objects room}

-- addItem itemname room@Room {objects} =
--     room {objects = itemname : objects room}

-- removeItem function for deleting an existing item in a Room.
removeItem :: ItemName -> Room -> Room
-- filter on existing objects list and return objects not in given itemname.
removeItem itemname room = room {objects = filter (/= itemname) (objects room)}

-- Create Rooms
italianPizzeria :: Room
italianPizzeria =
  Room
    { rname = ItalianPizzeria,
      desc = "You're in a Italian Pizzeria with aroma of freshly baked dough",
      exits = [(N, DiningRoom)],
      objects = [Pizza, Cheese]
    }

japaneseSushiBar :: Room
japaneseSushiBar =
  Room
    { rname = JapaneseSushiBar,
      desc = "You're in a Japanese Sushi Bar with minimalist decoration",
      exits = [(N, Reception)],
      objects = [Sashimi, Wasabi, Sake]
    }

diningRoom :: Room
diningRoom =
  Room
    { rname = DiningRoom,
      desc = "You're at the Dining Room and it's dark and quiet",
      exits =
        [ (S, ItalianPizzeria),
          (W, KoreanBBQGrill),
          (E, SpanishTapasTavern)
        ],
      objects = [Tableware]
    }

mexicanTaqueria :: Room
mexicanTaqueria =
  Room
    { rname = MexicanTaqueria,
      desc = "You're in a Mexican Taqueria with sound of lively conversation",
      exits = [(S, SpanishTapasTavern)],
      objects = [Taco, Avacado, Salsa]
    }

frenchPatisserie :: Room
frenchPatisserie =
  Room
    { rname = FrenchPatisserie,
      desc = "You're in a French Patisserie with a smell of butter",
      exits = [(W, SpanishTapasTavern)],
      objects = [Croissant]
    }

chineseHotPotPlace :: Room
chineseHotPotPlace =
  Room
    { rname = ChineseHotPotPlace,
      desc = "You're in a Chinese Hot Pot Place with many large groups",
      exits = [(S, Reception)],
      objects = [Tofu, Beef]
    }

spanishTapasTavern :: Room
spanishTapasTavern =
  Room
    { rname = SpanishTapasTavern,
      desc = "You're in a Spanish Tapas Tavern with a live soccer game playing",
      exits =
        [ (N, MexicanTaqueria),
          (W, DiningRoom),
          (E, FrenchPatisserie)
        ],
      objects = [Paella]
    }

koreanBBQGrill :: Room
koreanBBQGrill =
  Room
    { rname = KoreanBBQGrill,
      desc = "You're in a Korean BBQ Grill with K-Pop music playing",
      exits = [(W, Reception), (E, DiningRoom)],
      objects = [Bulgogi, Kimchi]
    }

reception :: Room
reception =
  Room
    { rname = Reception,
      desc = "You're at the reception area",
      exits =
        [ (N, ChineseHotPotPlace),
          (S, JapaneseSushiBar),
          (E, KoreanBBQGrill)
        ],
      objects = [Cash]
    }

-- A constant storing a list of all valid rooms
allRooms :: [Room]
allRooms =
  [ italianPizzeria,
    mexicanTaqueria,
    japaneseSushiBar,
    frenchPatisserie,
    chineseHotPotPlace,
    koreanBBQGrill,
    spanishTapasTavern,
    diningRoom,
    reception
  ]
