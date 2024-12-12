-- Made by Hugo Krul (8681929) and Tijmen Vis (7155409)

module Main where

import Algebra
import Model
import Interpreter
import Lexer
import Parser

import Data.Maybe
import qualified Data.Map as Map
import System.Exit

safeHead :: [Cmd] -> String
safeHead []     = ""
safeHead (a:as) = show a

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive env state = do
  -- step aanroepen op env en arrowstate
  let newStep = step env state
  case newStep of
    Done space position heading               -> do
                                                  putStrLn "Done!"
                                                  print position
                                                  print heading
                                                  putStrLn $ printSpace space
                                                  exitSuccess

    Ok s@(ArrowState space position heading stack)  -> do 
                                                  let result = Main.safeHead stack
                                                  if result /= "" then do
                                                    putStrLn result
                                                    print position
                                                    print heading
                                                    putStrLn $ printSpace space

                                                    putStrLn "enter y or enter for next step, enter n to stop"
                                                    chars <- getLine
                                                    if chars == "y" || chars == "" then
                                                      interactive env s
                                                    else die "program stopped"
                                                  else interactive env s

    Fail error                                -> do
                                                  print error
                                                  die error

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env state@(ArrowState sp p h s) = do 
  let result = stepRecurse env $ step env state
  case result of
    Done space pos heading -> (space, pos, heading)
    _ -> (sp, p, h)

stepRecurse :: Environment -> Step -> Step
stepRecurse env currStep = case currStep of
                         Ok arrowState -> stepRecurse env $ step env arrowState
                         _ -> currStep

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
  
  putStrLn "enter the .arrow file"
  arrowString <- getLine
  arrow <- readFile arrowString

  putStrLn "enter the .space file"
  spaceString <- getLine
  space <- readFile spaceString
  spaceChars <- readFile "examples/EmptySpace.space"

  putStrLn "enter the x position"
  x <- getLine

  putStrLn "enter the y position"
  y <- getLine

  putStrLn ""

  putStrLn "enter 1 for north, 2 for east, 3 for south, 4 for east"
  heading <- getLine


  let space = stringToSpace spaceChars :: Space
  let env = toEnvironment arrow
  let stack = loadStack env
  let arrowState = ArrowState space (read x, read y) (toEnum (read heading)) stack

  interactive env arrowState
