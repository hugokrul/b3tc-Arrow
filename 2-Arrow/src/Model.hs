module Model where

-- Exercise 1
data Token = 
                    TArrow      |
                    TDot        |
                    TComma      |
                    TGo         |
                    TTake       |
                    TMark       |
                    TNothing    |
                    TTurn Turn  |
                    TCase       |
                    TDir Dir    |
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
                    TVariable String
    deriving Show

newtype Turn = Turn  Dir
    deriving Show

data Dir = Left | Right | Front
    deriving Show

-- Exercise 2
data Program = Program deriving Show
