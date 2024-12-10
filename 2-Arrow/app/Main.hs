-- Made by Hugo Krul (8681929) and Tijmen Vis (7155409)

module Main where

import Algebra
import Model
import Interpreter
import Lexer
import Parser
import qualified Data.Map as L

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
  let test = L.fromList [((x, y), Interpreter.Debris) | x <- [0 .. 7], y <- [0 .. 7]] :: Space
  putStrLn $ printSpace test

mainArrow :: IO ()
mainArrow = do
  chars <- readFile "examples/Add.arrow"
  let env = toEnvironment chars
  print env
