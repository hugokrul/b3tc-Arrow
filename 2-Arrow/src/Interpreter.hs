module Interpreter where
import ParseLib

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (isSpace)
import Control.Monad (replicateM, join)
import Data.List.Split
import Data.List
import Data.Maybe

import Lexer
import Parser
import Model
import Algebra

-- the available contents
data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
  deriving (Eq, Ord)

-- contents get looked up in the contentstable
instance Show Contents where
  show x = [Map.fromList contentsTable Map.! x]

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents

-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ Map.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Interpreter.Empty   , '.' )
                 , (Interpreter.Lambda  , '\\')
                 , (Interpreter.Debris  , '%' )
                 , (Interpreter.Asteroid, 'O' )
                 , (Interpreter.Boundary, '#' )]

-- parses the string a string using the parser parseSpace, if it doesn't parse, it gives back an empty space
stringToSpace :: String -> Space
stringToSpace input = fromMaybe Map.empty (run' parseSpace input)
  where
    run' :: Parser Char Space -> String -> Maybe Space
    run' parser input =
        case parse parser input of
            [] -> Nothing
            [(result, _)] -> Just result
            (result, []):xs -> Just result
            (result, t):xs -> run' parser t

-- Exercise 7
printSpace :: Space -> String
printSpace input = do
  let height = fst (fst (Map.findMax input))
  let width = snd (fst (Map.findMax input))
  -- splits the input with a give width
  let chunks = chunksOf (width + 1) $ Map.elems input
  "(" ++ show width ++ ", " ++ show height ++ ")" ++ "\n" ++ printChunks chunks

printChunks :: [[Contents]] -> String
printChunks [] = ""
printChunks (x:xs) = printChunk x ++ printChunks xs
  where
    printChunk :: [Contents] -> String
    printChunk [] = "\n"
    printChunk (x:xs) = show x ++ printChunk xs

-- These three should be defined by you
type Ident = String -- is always Ident String
type Commands = [Cmd]
data Heading = North | East | South | West
  deriving (Eq, Ord, Show, Enum, Read)

type Environment = Map Ident Commands

-- i.e. [Cmd]
type Stack       =  Commands
-- uses a record because it is only necessary to update one or more attributes
data ArrowState  =  ArrowState {space :: Space, pos :: Pos, heading :: Heading, stack :: Stack}
  deriving Show

data Step =  Done  Space Pos Heading
          |  Ok    {arrowState :: ArrowState}
          |  Fail  String
  deriving Show

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment input = do
  -- first lexes
  let tokens = alexScanTokens input :: [Token]
  -- then parses the program
  let program = Program (parser tokens) :: Program
  -- if the program is valid, it makes the environment, else it gives back an empty environment
  if checkProgram program then makeEnvironment program else Map.empty
    where
      -- for every rules in the program, it adds the name and commands to the Map
      makeEnvironment = Map.fromList . map (\(Rule name cmds) -> (name, cmds)) . rules -- rules :: Program -> [Rule]

-- loads the start ident in the stack, the rest gets loaded in by the step function
loadStack :: Environment -> Stack
loadStack env = fromMaybe [] (Map.lookup "start" env)

-- | Exercise 9
step :: Environment -> ArrowState -> Step
-- gets an environment and arrowstate and executes one command from the stack
step env arrowState@(ArrowState space position@(x, y) heading stack) =
  case maybeCommand of
    Just currentCommand -> stepCommand currentCommand
    -- if the list is empty, the program must be done
    Nothing -> Done space position heading

  where
    maybeCommand = safeHead stack -- safeHead gives back a maybe if the list is empty
    -- deletes the first element from the stap, i.e., maybeCommand.
    updatedState = arrowState {stack = safeTail stack}

    -- executes one command from the step
    stepCommand :: Cmd -> Step
    stepCommand x = 
      case x of
        -- updates the position with next position
        Go             -> Ok updatedState { pos = goStep heading}
        -- updates the space
        Take           -> Ok updatedState { space = takeStep }
        -- marks the space
        Mark           -> Ok updatedState { space = markStep }
        -- does nothing
        CmdNothing     -> Ok updatedState
        -- updates the heading of the "space ship"
        Turn dir       -> Ok updatedState { heading = updateHeading dir }
        -- executes the case. 
        -- Also replaces the entire state, so it can fail
        Case dir alts  -> caseStep dir alts
        -- updates the stack with the comments of the ident
        -- replaces the entire state, this is because now it can fail if the variable is not found
        Ident variable -> identStep variable
        -- if the commands don't get recognised, the execution must fail.
        _              -> Fail "Failed execution"

    -- Takes a step in the current direction not if the next position is an asteroid or a boundary
    goStep :: Heading -> Pos
    goStep heading = do
      let nextPosition = case heading of
                North -> (x-1, y)
                East  -> (x, y+1)
                South -> (x+1, y)
                West  -> (x, y-1)
      let nextSpaceItem = Map.lookup nextPosition space
      case nextSpaceItem of
        Just content -> case content of
                          -- returns the original position, if it can't move
                          Interpreter.Asteroid -> position
                          Interpreter.Boundary -> position
                          _ -> nextPosition
        -- returns the original position, if it can't move
        Nothing -> position

    -- takes the lambda and debris from the corrent position and replaces it with Empty
    takeStep :: Space
    takeStep = do
      let maybeContents = Map.lookup position space
      case maybeContents of
        Just contents -> case contents of
                          Interpreter.Lambda -> Map.adjust (const Interpreter.Empty) position space
                          Interpreter.Debris -> Map.adjust (const Interpreter.Empty) position space
                          _ -> space
        Nothing -> space
    
    -- replaces the content with a lambda
    markStep :: Space
    markStep = Map.adjust (const Interpreter.Lambda) position space

    caseStep :: Dir -> Alts -> Step
    caseStep dir alts = do
      -- replaces the case with commands of the pattern
      -- so it first has to delete the case from the stack
      let ArrowState _ _ _ updatedStack = updatedState
      let tempHeading = updateHeading dir
      -- first checks the new position in which the case want to check for patterns
      let tempPos = case tempHeading of
                      North -> (x-1, y)
                      East  -> (x, y+1)
                      South -> (x+1, y)
                      West  -> (x, y-1)
      -- the content of the temporary position, the position should not be updated in the case, it should only look at the next position.
      let maybeContent = Map.lookup tempPos space
      -- inserts the commands of all the patterns in a map, so it can easily look them up
      let altlist = Map.fromList $ map (\(Alt pat cmds) -> (pat, cmds)) alts :: Map Pat Commands
      let pat = case maybeContent of
                  Just content -> case content of
                                    Interpreter.Empty -> Model.Empty
                                    Interpreter.Lambda -> Model.Lambda
                                    Interpreter.Asteroid -> Model.Asteroid
                                    Interpreter.Debris -> Model.Debris
                                    Interpreter.Boundary -> Model.Boundary
                  -- if the content of the doesn't much anything, it is out of bounds. So a boundary
                  Nothing -> Model.Boundary
      -- looksup the commands from a pattern
      let maybeCmds = Map.lookup pat altlist
      -- if that pattern is not in the "case", we hope there is an underscore, if there isn't an underscore, underScoreCmds is Nothing and the program fails
      let underScoreCmds = case maybeCmds of
                Nothing -> Map.lookup Model.Underscore altlist
                _ -> maybeCmds
      case underScoreCmds of
                -- if there are commands: it adds the commands to the stack
                Just cmds -> Ok updatedState { stack = cmds ++ updatedStack}
                Nothing -> Fail "Non-exhaustive patterns in match"

    -- deletes the ident from the stack, replaces it with all the commands of that ident
    identStep :: String -> Step
    identStep variable = do
      let ArrowState _ _ _ updatedStack = updatedState
      let maybeCommands = Map.lookup variable env
      case maybeCommands of
        Just commands -> Ok updatedState { stack = commands ++ updatedStack }
        -- if the variable isn't found, the program fails.
        Nothing -> Fail ""
    
    -- the heading gets updated, thats where the enum is for.
    -- to left is counter clockwise: South -> East, 2 -> 1, (2+3) = 5 % 4 = 1
    -- to right is clockwise: South -> West, 2 -> 3, (2+1) = 3 % 4 = 3
    updateHeading :: Dir -> Heading
    updateHeading dir = case dir of
                          Model.Left  -> toEnum $ (fromEnum heading + 3) `mod` 4
                          Model.Right -> toEnum $ (fromEnum heading + 1) `mod` 4
                          Model.Front -> heading

-- returns nothing if there isn't an element anymore.
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (a:as) = Just a

-- if there isn't a tail anymore it just gives back an empty list
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs