module Item where

import Data.Map qualified as M

data ItemName
  = Pizza
  | Avacado
  | Salsa
  | Cheese
  | Taco
  | Sashimi
  | Wasabi
  | Sake
  | Tableware
  | Croissant
  | Tofu
  | Beef
  | Paella
  | Bulgogi
  | Kimchi
  | Cash
  deriving (Eq, Ord)

-- Write custom show instance for ItemName
instance Show ItemName where
  show :: ItemName -> String
  show Pizza = "pizza"
  show Avacado = "avacado"
  show Salsa = "salsa"
  show Cheese = "cheese"
  show Taco = "taco"
  show Sashimi = "sashimi"
  show Wasabi = "wasabi"
  show Sake = "sake"
  show Tableware = "tableware"
  show Croissant = "croissant"
  show Tofu = "tofu"
  show Beef = "beef"
  show Paella = "paella"
  show Bulgogi = "bulgogi"
  show Kimchi = "kimchi"
  show Cash = "cash"

type Universe = M.Map ItemName Item

data Item = Item
  { iname :: ItemName,
    weight :: Integer,
    -- Control whether the item is slippery
    slippery :: Bool,
    -- Count the number of moves after item got taken into inventory
    -- Store this counter information for each item in order to manage
    -- multiple slippery items scenario
    numberOfMoves :: Integer,
    -- Track whether food is consumed
    consumed :: Bool,
    -- Control whether the food is poisoned
    poisoned :: Bool
  }
  deriving (Show, Eq)

-- Function for creating a new universe.
-- Take a list of items and construct a Universe:
-- (key-value store with ItemName-s as keys and Item-s as values).
mkUniverse :: [Item] -> Universe
mkUniverse items = M.fromList $ map (\x -> (iname x, x)) items

-- Create Items
pizza :: Item
pizza = Item Pizza 60 False 0 False False

avacado :: Item
avacado = Item Avacado 10 True 0 False False

salsa :: Item
salsa = Item Salsa 30 False 0 False False

cheese :: Item
cheese = Item Cheese 40 False 0 False False

taco :: Item
taco = Item Taco 40 False 0 False False

sashimi :: Item
sashimi = Item Sashimi 20 False 0 False False

wasabi :: Item
wasabi = Item Wasabi 10 False 0 False True

sake :: Item
sake = Item Sake 130 True 0 False False

tableware :: Item
tableware = Item Tableware 10 False 0 False False

croissant :: Item
croissant = Item Croissant 30 False 0 False False

tofu :: Item
tofu = Item Tofu 20 False 0 False False

beef :: Item
beef = Item Beef 50 False 0 False False

paella :: Item
paella = Item Paella 60 False 0 False True

bulgogi :: Item
bulgogi = Item Bulgogi 40 False 0 False False

kimchi :: Item
kimchi = Item Kimchi 20 True 0 False False

cash :: Item
cash = Item Cash 10 False 0 False False

-- Create the game universe
univ :: Universe
univ =
  mkUniverse
    [ salsa,
      pizza,
      taco,
      avacado,
      cheese,
      sashimi,
      sake,
      tableware,
      wasabi,
      croissant,
      tofu,
      beef,
      paella,
      bulgogi,
      kimchi,
      cash
    ]
