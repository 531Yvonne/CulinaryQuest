module GameIO where

import Command
import Control.Monad.State
import Data.Foldable (traverse_)
import GameState
import Item
import Room
import System.Exit

type GameIO a = StateT GameState IO a

-- Take a transition between game states as an input,
-- Output a value of type `GameIO` that performs that transition
effectChange :: (GameState -> GameState) -> GameIO ()
effectChange f = modify f

-- or point free version
-- effectChange = modify

-- Prompt for user input
prompt :: GameIO ()
prompt = lift $ putStr "-> "

-- Check the current state of the game
-- If there's `Just` value in message, print then set it to `Nothing`.
-- If there's a `Nothing` value in the message field, do nothing.
printMessage :: GameIO ()
printMessage = do
  currentState <- get
  case (message currentState) of
    Just m -> do
      lift $ putStrLn m
      -- setMessage defined in GameState: update GS's message with given string
      effectChange (setMessage "")
    Nothing -> pure ()

-- Print the description of player's current room
printDescription :: GameIO ()
printDescription = do
  currentState <- get
  lift $ putStrLn $ desc $ currentRoom currentState

-- Print `"You see the following objects:"`
-- followed by a list of all the items in the room
-- If there's no object, do nothing
printObjects :: GameIO ()
printObjects = do
  currentState <- get
  case (nearbyObjects currentState) of
    [] -> pure ()
    _ -> do
      lift $ putStrLn "You see the following objects:"
      lift $ traverse_ (putStrLn . show) $ nearbyObjects currentState

-- Print `"There are exits in the following directions:"` followed by
-- a list of all the directions there are exits in for player's current room
-- If no exits in current room, do nothing.
printExits :: GameIO ()
printExits = do
  currentState <- get
  case (exits $ currentRoom currentState) of
    [] -> pure ()
    listOfExits -> do
      lift $ putStrLn "There are exits in the following directions:"
      lift $ traverse_ (putStrLn . show . fst) listOfExits

-- Check the player's current inventory,
-- If empty, print `"You aren't carrying anything."`
-- If nonempty, print `"You are carrying the following items:"`
-- followed by a list of all the `ItemName`-s in the player's inventory:
printInventory :: GameIO ()
printInventory = do
  currentState <- get
  case (currentInventory currentState) of
    [] -> lift $ putStrLn "You aren't carrying anything."
    inventoryList -> do
      lift $ putStrLn "You are carrying the following items:"
      lift $ traverse_ (putStrLn . show) inventoryList

-- Take a function describing an action on items, and a list of item names
-- and performs the action on each item in the list, in order.
-- Each time it executes an action, it runs `printMessage`.
actionOverList ::
  (ItemName -> GameState -> GameState) ->
  [ItemName] ->
  GameIO ()
actionOverList f listOfItems =
  traverse_ executeAction listOfItems
  where
    -- Helper function to process one item
    executeAction :: ItemName -> GameIO ()
    executeAction item = do
      currentState <- get
      put $ f item currentState
      printMessage

-- Print a success message to the screen, then quits the program.
-- This is what you will run when the user wins the game.
finishGame :: GameIO ()
finishGame = do
  lift $ putStrLn "You successfully carry the sake into the dining room."
  lift $ putStrLn "Congrats! You win!"
  lift $ exitSuccess

-- Print a failing message to the screen, then quits the program.
-- This is what you will run when the user got caught taking the cash.
arrestFail :: GameIO ()
arrestFail = do
  lift $ putStrLn "You are arrested for robbery!"
  lift $ putStrLn "Sorry! You lose!"
  lift $ exitSuccess

-- Print a failing message to the screen, then quits the program.
-- This is what you will run when the user got food poisoning.
poisonFail :: GameIO ()
poisonFail = do
  lift $ putStrLn "But you have food poisoning symptoms and have to leave!"
  lift $ putStrLn "Sorry! You lose!"
  lift $ exitSuccess

-- When the user decides to quit the game, rather than when they win.
-- `exit` prints `"Goodbye!"`, then exits the game with a zero exit status
exit :: GameIO ()
exit = do
  lift $ putStrLn "Goodbye!"
  lift $ exitSuccess

-- Check whether the current game state is the winning state.
-- Winning rule defined in `GameState` module with the `haveWonGame` predicate.
-- If yes, run `finishGame`. Otherwise, do nothing
checkGameOver :: GameIO ()
checkGameOver = do
  currentState <- get
  case (haveWonGame currentState) of
    True -> finishGame
    False -> pure ()

-- Check whether the current game state is the losing state.
-- Losing rule defined in `GameState` in `tookCash` and `consumerPoisonedFood`
-- If yes, run `lostGame`. Otherwise, do nothing
checkGameLost :: GameIO ()
checkGameLost = do
  currentState <- get
  case (consumedPoisonedFood currentState, tookCash currentState) of
    (True, _) -> poisonFail
    (_, True) -> arrestFail
    _ -> pure ()

-- Print the message `'I don't understand that'`
syntaxError :: GameIO ()
syntaxError = lift $ putStrLn "I don't understand that."

-- Print the message `"Welcome to Functional Adventure!"`
opening :: GameIO ()
opening = lift $ putStrLn "Welcome to Culinary Quest! Grab the Best Item!"

-- Take any `Command`, and executes the action corresponding to the command.
performCommand :: Command -> GameIO ()
performCommand Look = printDescription >> printObjects >> printExits
performCommand (Move dir) = do
  currentState <- get
  -- Add validMove to check whether a valid move is made
  -- Only when True: the checkSlippery should run after the move
  let validMove = case (destinationName dir $ currentRoom currentState) of
        Just _ -> True
        _ -> False
  put $ move dir currentState
  printMessage
  if validMove
    then do
      movedState <- get
      put $ checkSlippery movedState
      printMessage
    else pure ()
performCommand Inventory = printInventory
performCommand (Take itemList) = actionOverList takeItem itemList
performCommand (Drop itemList) = actionOverList dropItem itemList
performCommand (Eat itemList) = actionOverList eatItem itemList
performCommand Exit = exit

-- Perform every command in a `Conjunction`, in order:
performConjunction :: Conjunction -> GameIO ()
performConjunction = traverse_ performCommand

-- Parse an input string
-- if the parse succeeds, run `performConjunction` on the result.
-- If the parse fails, run `syntaxError`
parseConjunction :: String -> GameIO ()
parseConjunction playerInput =
  case (parseInput playerInput) of
    Nothing -> syntaxError
    Just result -> performConjunction result

-- Perform one round of printing the prompt, getting input from the user,
-- parsing it, performing the command the input denotes if the parse succeeds
-- and printing a syntax error message otherwise, then running `checkGameOver`
repl :: GameIO ()
repl = do
  prompt
  userInput <- lift $ getLine
  parseConjunction userInput
  checkGameLost
  checkGameOver
