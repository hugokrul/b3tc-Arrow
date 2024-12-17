module Model where

import Data.List.Split
import Debug.Trace

-- Exercise 1
data Token = 
                    TArrow      |
                    TDot        |
                    TComma      |
                    TGo         |
                    TTake       |
                    TMark       |
                    TNothing    |
                    TTurn       |
                    TCase       |
                    TLeft       |
                    TRight      |
                    TFront      |
                    TOf         |
                    TEnd        |
                    TSemiColon  |
                    TEmpty      |
                    TLambda     |
                    TDebris     |
                    TAsteroid   |
                    TBoundary   |
                    TUnderscore |
                    Token       |
                    TIdent String
    deriving (Show, Eq, Ord)

data Dir = Left | Right | Front
    deriving (Show, Eq, Ord)

-- Exercise 2
newtype Program = Program { rules :: [Rule] }
    deriving (Show, Eq, Ord)

data Rule = Rule { name :: String, cmds :: Cmds}
  deriving (Show, Eq, Ord)

type Cmds = [Cmd]

data Cmd = Go | Take | Mark | CmdNothing | CmdEmpty
         | Turn Dir
         | Case { caseDir :: Dir, caseAlts :: Alts }
         | Ident { identName :: String }
    deriving (Eq, Ord)

instance Show Cmd where
    show Go = "Go"
    show Take = "Take"
    show Mark = "Mark"
    show CmdNothing = "Nothing"
    show CmdEmpty = "Empty"
    show (Turn dir) = "Turn " ++ show dir
    show (Case dir alts) = "Case " ++ show dir ++ " of\n" ++ printAlts alts
    show (Ident name) = name ++ "()"

type Alts = [Alt]

printAlts :: [Alt] -> String
printAlts [] = ""
printAlts (x:xs) = "   " ++ show x ++ "\n" ++ printAlts xs

data Alt = Alt { pat :: Pat, altCmds :: Cmds }
    deriving (Eq, Ord)

instance Show Alt where
    show (Alt pattern commands) = show pattern ++ " -> " ++ show commands

data Pat = Empty | Lambda | Debris | Asteroid | Boundary | Underscore
    deriving (Show, Eq, Ord)