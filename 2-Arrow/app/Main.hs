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


-- Exercise 11
-- Interactive environment for going through an arrow file
interactive :: Environment -> ArrowState -> IO ()
interactive env state = do
  let newStep = step env state
  -- When done or fail, stops the program, if Ok, prints the next step and executes it
  case newStep of
    Done space position heading               -> do
                                                  putStrLn "Done!"
                                                  print position
                                                  print heading
                                                  putStrLn $ printSpace space
                                                  exitSuccess

    Ok s@(ArrowState space position heading stack)  -> do 
                                                  let result = getFirstCommand stack
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

      where
        -- Converts first command from the stack to a human readable string
        getFirstCommand :: [Cmd] -> String
        getFirstCommand []     = ""
        getFirstCommand (a:as) = show a
        
    Fail error                                -> do
                                                  print error
                                                  die error

-- Uses the stepRecurse function and returns the outputs
batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env state@(ArrowState sp p h s) = do 
  let result = stepRecurse env $ step env state
  case result of
    Done space pos heading -> (space, pos, heading)
    _ -> (sp, p, h)

-- Recurses the step function while an Ok output is given
stepRecurse :: Environment -> Step -> Step
stepRecurse env (Ok arrowState) = stepRecurse env $ step env arrowState
stepRecurse env currStep = currStep

-- Main program loop
main :: IO ()
main = do
  putStrLn "Enter the .arrow file (Ex: examples/Test.arrow)"
  arrowString <- getLine
  arrow <- readFile arrowString

  putStrLn "Enter the .space file (Ex: examples/EmptySpace.space)"
  spaceString <- getLine
  spaceChars <- readFile spaceString

  putStrLn "Enter the x position"
  x <- getLine

  putStrLn "Enter the y position"
  y <- getLine

  putStrLn ""

  putStrLn "Enter North, East, South, West for a starting facing"
  heading <- getLine
  
  let space = stringToSpace spaceChars :: Space
  let env = toEnvironment arrow
  let stack = loadStack env
  let arrowState = ArrowState space (read y, read x) (read heading) stack

  putStrLn "Enter the execution way: Batch or Interactive"
  executionWay <- getLine
  case executionWay of
    "Batch" -> do 
      let (space, pos, heading) = batch env arrowState
      print pos
      print heading
      putStrLn $ printSpace space
    "Interactive" -> interactive env arrowState
