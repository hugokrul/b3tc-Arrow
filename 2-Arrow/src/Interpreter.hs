module Interpreter where

import ParseLib

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (isSpace)
import Control.Monad (replicateM, join)
import Debug.Trace
import Data.List.Split

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
type Ident = ()
type Commands = ()
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment = undefined

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step = undefined


