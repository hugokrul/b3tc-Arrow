{
module Lexer where

import Model
}

%wrapper "basic"

$digit      = [0-9]
$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$alpha      = [a-zA-Z]
$plus       = \+

@variable = ($alpha|$digit|\+|\-)+

tokens :-
  $white+   ;
  "--".*    ;
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
  "turn left"       { const (TTurn (Turn Model.Left)) }
  "turn right"      { const (TTurn (Turn Model.Right)) }
  "turn front"      { const (TTurn (Turn Model.Front)) }
  "left"            { const (TDir Model.Left)}
  "right"           { const (TDir Model.Right)}
  "front"           { const (TDir Model.Front)}
  ";"               { const TSemiColon }
  "Empty"           { const TEmpty }
  "Lambda"          { const TLambda }
  "Debris"          { const TDebris }
  "Asteroid"        { const TAsteroid }
  "Boundary"        { const TBoundary }
  "_"               { const TUnderscore }
  @variable       { \input -> (TVariable input) }
  _                 ;