{
module Lexer where

import Model
import Debug.Trace
import Data.List.Split
}

%wrapper "basic"

$digit      = [0-9]
$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$alpha      = [a-zA-Z]
$plus       = \+

@variable = ($alpha|$digit|\+|\-)+
@case = (case\ ([a-zA-Z]|[0-9]|\+|\-)+\ of)+
@turn = (turn\ ([a-zA-Z]|[0-9]|\+|\-)+)+
@pat = (Empty|Lambda|Debris|Asteroid|Boundary|\_)+
@cmd = (go|take|mark|nothing|@turn|@case|@variable)+

tokens :-
  $white+   ;
  "--".*    ;
  "->"              { const TArrow }
  "."               { const TDot }
  ","               { const TComma }
  "end"             { const TEnd }
  ";"               { const TSemiColon }
  @cmd              { \input -> TCmd $ parseCmd input}
  @pat              { \input -> TPat (parsePat input)}
  _                 ;