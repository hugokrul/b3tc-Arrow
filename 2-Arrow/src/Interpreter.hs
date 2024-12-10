module Interpreter where
import ParseLib

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (isSpace)
import Control.Monad (replicateM, join)
import Debug.Trace
import Data.List.Split
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

run :: Parser Char Space -> String -> Space
run parser input = fromMaybe Map.empty (run' parser input)
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
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment input = do
  let tokens = alexScanTokens input
  let program = Program (parser tokens) :: Program
  if checkProgram program then makeEnvironment program Map.empty else Map.empty

makeEnvironment :: Program -> Environment -> Environment
makeEnvironment (Program rules) = makeEnvironment' rules
  where
    makeEnvironment' :: [Rule] -> Environment -> Environment
    makeEnvironment' [] env = Map.empty
    makeEnvironment' ((Rule name cmds):xs) env = Map.insert name cmds (makeEnvironment' xs env)

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step = undefined


