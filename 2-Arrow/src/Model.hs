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
    deriving (Show, Eq, Ord)
        
type Alts = [Alt]

data Alt = Alt { pat :: Pat, altCmds :: Cmds }
    deriving (Show, Eq, Ord)

data Pat = Empty | Lambda | Debris | Asteroid | Boundary | Underscore
    deriving (Show, Eq, Ord)