{
module Lexer where

import Model
import Debug.Trace
import Data.List.Split
}

%wrapper "basic"

$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$digit      = [0-9]
$alpha      = [a-zA-Z]
$plus       = \+
$minus      = \-

@variable = ($alpha|$digit|$plus|$minus)+

tokens :-
  $white+           ;
  "--".*            ;
  "->"              { const TArrow }
  "."               { const TDot }
  ","               { const TComma }
  "go"              { const TGo }
  "take"            { const TTake }
  "mark"            { const TMark }
  "nothing"         { const TNothing }
  "case"            { const TCase }
  "of"              { const TOf }
  "end"             { const TEnd }
  "turn"            { const TTurn }
  "left"            { const TLeft}
  "right"           { const TRight}
  "front"           { const TFront}
  ";"               { const TSemiColon }
  "Empty"           { const TEmpty }
  "Lambda"          { const TLambda }
  "Debris"          { const TDebris }
  "Asteroid"        { const TAsteroid }
  "Boundary"        { const TBoundary }
  "_"               { const TUnderscore }
  @variable         { \input -> (TIdent input) }
  _                 ;