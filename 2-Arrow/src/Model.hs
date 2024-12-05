module Model where

import Data.List.Split
import Debug.Trace

-- Exercise 1
data Token = 
                    TArrow      |
                    TDot        |
                    TComma      |
                    TCmd Cmd     |
                    TEnd        |
                    TSemiColon  |
                    TPat Pat    |
                    Token
    deriving (Show, Ord, Eq)

newtype Variable = Variable String
    deriving (Show, Ord, Eq)

data Cmd = Go | Take | Mark | NothingCmd | Turn Dir | Case Dir | VariableCmd Variable
    deriving (Show, Ord, Eq)

parseCmd :: String -> Cmd
parseCmd "go" = Model.Go
parseCmd "take" = Model.Take
parseCmd "mark" = Model.Mark
parseCmd "nothing" = Model.NothingCmd
parseCmd x =    if and [elem y x | y <- "turn "] then parseTurn x else
                if and [elem y x | y <- "case "]  then parseCase x else
                Model.VariableCmd (Variable x)

parseCase :: String -> Cmd
parseCase input = Model.Case (parseDir ((splitOn " " input ) !! 1))

data Pat = Lambda | Debris | Asteroid | Boundary | Underscore | Empty
    deriving (Show, Ord, Eq)

parsePat :: String -> Pat
parsePat "Lambda" = Model.Lambda
parsePat "Debris" = Model.Debris
parsePat "Asteroid" = Model.Asteroid
parsePat "Boundary" = Model.Boundary
parsePat "Empty" = Model.Empty
parsePat "_" = Model.Underscore

parseTurn :: String -> Cmd
parseTurn x = let dir = (splitOn " " x) !! 1 in Turn $ parseDir dir

data Dir = Left | Right | Front
    deriving (Show, Ord, Eq)

parseDir :: String -> Dir
parseDir "left" = Model.Left
parseDir "right" = Model.Right
parseDir "front" = Model.Front

-- Exercise 2
data Program = Program { rules :: [Rule] }
    deriving (Show, Eq, Ord)


data Rule = Rule {  variable :: Variable,
                    cmds     :: [Cmd]
                 } 
    deriving (Eq, Ord)

instance Show Rule where
    show = printRule

printRule :: Rule -> String
printRule (Rule (Variable x) y) = x ++ " " ++(show y)

testRule :: Rule
testRule = Rule (Variable "ewas") [Go, (Turn Model.Left)]