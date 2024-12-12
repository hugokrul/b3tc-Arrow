-- Made by Hugo Krul (8681929) and Tijmen Vis (7155409)

module Main where

import Algebra
import Model
import Interpreter
import Lexer
import Parser

import Data.Maybe
import qualified Data.Map as Map

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive = undefined

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch = undefined

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
main :: IO()
main = do
  mainArrow

mainSpace :: IO()
mainSpace = do
  chars <- readFile "examples/EmptySpace.space"
  let space = Map.fromList [((x, y), Interpreter.Debris) | x <- [0 .. 7], y <- [0 .. 7]] :: Space
  putStrLn $ printSpace space

-- TODO: eindigen zonder punt mag, moet opgelost worden

mainArrow :: IO ()
mainArrow = do
  chars <- readFile "examples/Add.arrow"
  spaceChars <- readFile "examples/Maze.space"
  let space = stringToSpace spaceChars :: Space
  let env = toEnvironment chars
  let stack = loadStack env
  let arrowState = ArrowState space (0, 0) South stack
  -- let Ok nogEenKeer = step env arrowState
  -- let Ok oneMoreTime = step env nogEenKeer
  -- let Done space pos heading = step env oneMoreTime
  let Done space pos heading = stepRecurse env $ step env arrowState
  print pos
  print heading
  putStrLn $ printSpace space
  -- print env

stepRecurse :: Environment -> Step -> Step
stepRecurse env currStep = case currStep of
                         Ok arrowState -> stepRecurse env $ step env arrowState
                         _ -> currStep
