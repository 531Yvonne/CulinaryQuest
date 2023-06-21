module Command where

import Direction
import Item
import Text.Parsec hiding (choice, parse, runParser, sepBy, sepBy1, (<|>))
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

-- Do alternative pasr2 when prsr1 fails
(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (try sep)

parse :: Parser a -> String -> Either ParseError a
parse parser = P.parse parser ""

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Eat [ItemName]
  | Move Direction
  | Exit
  deriving (Eq, Show)

type Conjunction = [Command]

-- Parse any string representing an `ItemName` into an `ItemName` datatype
itemNameP :: Parser ItemName
itemNameP =
  (Avacado <$ string "avacado")
    <|> (Salsa <$ string "salsa")
    <|> (Sashimi <$ string "sashimi")
    <|> (Sake <$ string "sake")
    <|> (Wasabi <$ string "wasabi")
    <|> (Taco <$ string "taco")
    <|> (Croissant <$ string "croissant")
    <|> (Tableware <$ string "tableware")
    <|> (Pizza <$ string "pizza")
    <|> (Cheese <$ string "cheese")
    <|> (Tofu <$ string "tofu")
    <|> (Beef <$ string "beef")
    <|> (Bulgogi <$ string "bulgogi")
    <|> (Kimchi <$ string "kimchi")
    <|> (Paella <$ string "paella")
    <|> (Cash <$ string "cash")

-- or use choice
-- itemNameP :: Parser ItemName
-- itemNameP =
--   choice
--     [  (Avacado <$ string "avacado")
-- <|> (Salsa <$ string "salsa")
-- <|> (Sashimi <$ string "sashimi")
-- <|> (Wasabi <$ string "wasabi")
-- <|> (Taco <$ string "taco")
-- <|> (Croissant <$ string "croissant")
-- <|> (Tableware <$ string "tableware")
-- <|> (Pizza <$ string "pizza")
-- <|> (Cheese <$ string "cheese")
--     ]

-- Parse a comma-separated list of nouns.
-- It should accept either no space at all between the comma separated items,
-- or one space after each comma.
nounPhrase :: Parser [ItemName]
nounPhrase = sepBy1 itemNameP (string ",") <|> sepBy1 itemNameP (string ", ")

-- Parse the string 'inventory' and rejects everything else.
inventoryP :: Parser Command
inventoryP = Inventory <$ string "inventory"

-- Parse the word 'take' plus a noun phrase into a `Command`.
-- There needs to be EXACTLY ONE SPACE between 'take' and the
-- first word of the noun phrase.
takeP :: Parser Command
takeP = do
  takePart <- Take <$ string "take" <* space
  -- Here, remove previous "spaces" around nounPhrase since
  -- one space before first item is dealt in takePart,
  -- space between each item is dealt in nounPhrase.
  itemPart <- nounPhrase

  pure $ takePart itemPart

-- Or
-- takeP :: Parser Command
-- takeP = do
--   _ <- string "take" <* space
--   itemPart <- spaces *> nounPhrase <* spaces
--   pure $ Take itemPart

-- Or
-- takeP :: Parser Command
-- takeP = Take <$> (string "take" *> space *> spaces *> nounPhrase <* spaces)

-- Parse the quit command. Accept either 'quit' or 'exit'.
exitP :: Parser Command
exitP = (Exit <$ string "exit") <|> (Exit <$ string "quit")

-- Parse the word 'drop' plus a noun phrase into a `Command`.
dropP :: Parser Command
dropP = do
  dropPart <- Drop <$ string "drop" <* space
  itemPart <- nounPhrase
  pure $ dropPart itemPart

-- Parse the string 'look' and rejects everything else.
lookP :: Parser Command
lookP = Look <$ string "look"

-- Expect a single lowercase word denoting a direction,
-- map 'north', 'south', 'east', or 'west' to the relevant `Direction`.
directionP :: Parser Direction
directionP =
  (N <$ string "north")
    <|> (S <$ string "south")
    <|> (E <$ string "east")
    <|> (W <$ string "west")

-- Expect one of the four words 'north'`, `'south'`, `'east'`, or `'west'`.
-- Consume the relevant word off the input, and makes that word into a command
-- telling the game to move in the relevant direction.
moveP :: Parser Command
moveP = do
  directionPart <- directionP
  pure $ Move directionPart

-- Or
-- moveP = Move <$> directionP

-- Accept any single command that is syntactically well-formed,
-- Return the `Command` corresponding to the string in the language.
commandP :: Parser Command
commandP =
  inventoryP
    <|> takeP
    <|> dropP
    <|> moveP
    <|> lookP
    <|> exitP
    <|> eatP

-- Parse a list of commands, separated by the word `'and'`,
-- into a `Conjunction`, which is a type alias for a list of `Command`-s.
-- `conjunctionP` is top-level parser for entire command the user would type in
-- so it requires `eof` at the end.
conjunctionP :: Parser Conjunction
conjunctionP = (sepBy1 commandP (space *> string "and" <* space)) <* eof

-- Parse the word 'eat' plus a noun phrase into a `Command`.
eatP :: Parser Command
eatP = do
  eatPart <- Eat <$ string "eat" <* space
  itemPart <- nounPhrase
  pure $ eatPart itemPart

-- Helper function that takes a string in
-- Return the conjunction wrapped in a `Just`, if input string is well-formed
-- otherwise returns `Nothing`.
parseInput :: String -> Maybe Conjunction
parseInput userInput =
  case (parse conjunctionP userInput) of
    Left _ -> Nothing
    Right result -> Just result