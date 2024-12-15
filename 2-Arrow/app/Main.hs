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
import Data.Char

-- Exercise 11
-- Interactive environment for going through an arrow file
interactive :: Environment -> ArrowState -> IO ()
interactive env state@(ArrowState _ originalPos@(originalY, originalX) originalHeading originalStack) = do
  let newStep = step env state
  -- When done or fail, stops the program, if Ok, prints the next step and executes it
  case newStep of
    Done space (y, x) heading               -> do
                                                  putStrLn "Done!"
                                                  print (x, y)
                                                  print heading
                                                  putStrLn $ printSpace space
                                                  exitSuccess

    Ok s@(ArrowState space position@(y, x) heading stack)  -> do 
                                                  let result = getFirstCommand originalStack
                                                  if result /= "" then do
                                                    putStrLn result
                                                    if originalPos /= position then 
                                                      putStrLn (show (originalX, originalY) ++ " -> " ++ show (x, y)) 
                                                    else print (x, y)
                                                    if originalHeading /= heading then 
                                                      putStrLn $ show originalHeading ++ " -> " ++ show heading 
                                                    else print heading
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
  putStrLn "Enter the .arrow file (Ex: examples/Add.arrow)"
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
  -- toEnvironment gives back an empty environment if the program is not correct
  if env == Map.empty then die "program is not correct" else putStr ""
  let stack = loadStack env
  -- the space Map has the positions as (y, x) we want it (x, y) but only handle that in the printing.(&&)
  -- The space Map does this because it automatically stores (0, 1) before (1, 0) so printing the map would be way harder and more time consuming
  let arrowState = ArrowState space (read y, read x) (read heading) stack

  putStrLn "Enter the execution way: Batch or Interactive"
  executionWay <- getLine
  
  case map toLower executionWay of
    "batch" -> do 
      let (space, pos, heading) = batch env arrowState
      print pos
      print heading
      putStrLn $ printSpace space
    "interactive" -> interactive env arrowState
    _ -> die "pattern unknown"
