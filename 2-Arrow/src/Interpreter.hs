module Interpreter where
import ParseLib

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (isSpace)
import Control.Monad (replicateM, join)
import Debug.Trace
import Data.List.Split
import Data.List
import Data.Maybe
import Lexer
import Parser
import Model
import Algebra



data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
  deriving (Eq, Ord)

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

testSpace :: String
testSpace = "(7,7)\n.O....O.\n.OOO.OO.\n...O.O..\n.O...OO.\n.O.O....\n.O.O.OOO\nOOOO....\n\\....O.O"

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
  let width = fst (fst (Map.findMax input))
  let height = snd (fst (Map.findMax input))
  -- splits the input with a give width
  let chunks = chunksOf (height + 1) $ Map.elems input
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
type Altsmap = Map Pat Commands
data Heading = North | East | South | West
  deriving (Eq, Ord, Show, Enum, Read)

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState {space :: Space, pos :: Pos, heading :: Heading, stack :: Stack}
  deriving Show

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String
  deriving Show

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment input = do
  let tokens = alexScanTokens input
  let program = Program (parser tokens) :: Program
  if checkProgram program then makeEnvironment program else Map.empty
    where
      makeEnvironment = Map.fromList . map (\(Rule name cmds) -> (name, cmds)) . rules -- rules :: Program -> [Rule]

loadStack :: Environment -> Stack
loadStack env = fromMaybe [] (Map.lookup "start" env)

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step env arrowState@(ArrowState space position@(x, y) heading stack) =
  case currentCommand of
    Just x -> stepCommand x
    Nothing -> Done space position heading

  where
    currentCommand = safeHead stack
    updatedState = arrowState {stack = safeTail stack}

    stepCommand :: Cmd -> Step
    stepCommand x = 
      case x of
        Go             -> Ok updatedState { pos = goStep heading}
        Take           -> Ok updatedState { space = takeStep }
        Mark           -> Ok updatedState { space = markStep }
        CmdNothing     -> Ok updatedState
        Turn dir       -> Ok updatedState { heading = updateHeading dir }
        Case dir alts  -> caseStep dir alts
        Ident variable -> identStep variable
        _              -> Fail "Failed execution"


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
                          Interpreter.Asteroid -> position
                          Interpreter.Boundary -> position
                          _ -> nextPosition
        Nothing -> position

    takeStep :: Space
    takeStep = do
      let maybeContents = Map.lookup position space
      case maybeContents of
        Just contents -> case contents of
                          Interpreter.Lambda -> Map.adjust (const Interpreter.Empty) position space
                          Interpreter.Debris -> Map.adjust (const Interpreter.Empty) position space
                          _ -> space
        Nothing -> space
    
    markStep :: Space
    markStep = Map.adjust (const Interpreter.Lambda) position space

    -- todo
    caseStep :: Dir -> Alts -> Step
    caseStep dir alts = do
      let ArrowState _ _ _ updatedStack = updatedState
      let tempHeading = updateHeading dir
      let tempPos = case tempHeading of
                      North -> (x-1, y)
                      East  -> (x, y+1)
                      South -> (x+1, y)
                      West  -> (x, y-1)
      let maybeContent = Map.lookup tempPos space
      let altlist = Map.fromList $ map (\(Alt pat cmds) -> (pat, cmds)) alts
      let pat = case maybeContent of
                  Just content -> case content of
                                    Interpreter.Empty -> Model.Empty
                                    Interpreter.Lambda -> Model.Lambda
                                    Interpreter.Asteroid -> Model.Asteroid
                                    Interpreter.Debris -> Model.Debris
                                    Interpreter.Boundary -> Model.Boundary

                  Nothing -> Model.Boundary
      let maybeCmds = Map.lookup pat altlist
      let underScoreCmds = case maybeCmds of
                Nothing -> Map.lookup Model.Underscore altlist
                _ -> maybeCmds
      case underScoreCmds of
                Just cmds -> Ok updatedState { stack = cmds ++ updatedStack}
                Nothing -> Fail "Non-exhaustive patterns in match"

    identStep :: String -> Step
    identStep variable = do
      let ArrowState _ _ _ updatedStack = updatedState
      let maybeCommands = Map.lookup variable env
      case maybeCommands of
        Just commands -> Ok updatedState { stack = commands ++ updatedStack }
        Nothing -> Fail ""
    
    updateHeading :: Dir -> Heading
    updateHeading dir = case dir of
                          Model.Left  -> toEnum $ (fromEnum heading + 3) `mod` 4
                          Model.Right -> toEnum $ (fromEnum heading + 1) `mod` 4
                          Model.Front -> heading


safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (a:as) = Just a


safeTail :: [a] -> [a]
safeTail [] =[]
safeTail (x:xs) = xs