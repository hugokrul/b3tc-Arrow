{
module Parser where

import Model
import Debug.Trace
}

%name parser
%tokentype { Token }

-- Variables with all the tokens defined in Model.hs
%token
  x         { Token       }
  "->"      { TArrow      }
  '.'       { TDot        }
  ','       { TComma      }
  go        { TGo         }
  take      { TTake       }
  mark      { TMark       }
  nothing   { TNothing    }
  case      { TCase       }
  of        { TOf         }
  end       { TEnd        }
  turn      { TTurn       }
  left      { TLeft       }
  right     { TRight      }
  front     { TFront      }
  ';'       { TSemiColon  }
  e         { TEmpty      }
  lambda    { TLambda     }
  debris    { TDebris     }
  asteroid  { TAsteroid   }
  boundary  { TBoundary   }
  '_'       { TUnderscore }
  ident     { TIdent $$   }


%%

-- A program is a list of rules, A new rule gets appended to the already existing rules, the rules end with a '.' and thus get seperated it by '.'
Program : Program '.' rule            { $1 ++ [$3] }
        | Program '.'                 { $1 } -- a program always ends with '.'
        | rule                        { [$1] } -- one rule 
        | {- empty -}                 { [] } -- base case

-- One rule, is a variable name (ident), a "->" and a list of commands
rule : ident "->" cmds  { Rule $1 $3}

-- The commands get seperated by ',', almost the same as Program, but they don't end with ','
cmds : cmds ',' cmd     { $1 ++ [$3] }
     | cmd              { [$1] }
     | {- empty -}      { [] }

-- a command consists of either a go, take, mark nothing, turn (with a direction), cases or ident with a variable name
cmd : go                    { Model.Go }
    | take                  { Model.Take }
    | mark                  { Model.Mark }
    | nothing               { Model.CmdNothing }
    | turn left             { Model.Turn Model.Left }
    | turn right            { Model.Turn Model.Right }
    | turn front            { Model.Turn Model.Front }
    -- a case is the token "case", "dir", "of", list of alt ("alts"), and end
    -- in the gramar, end gets followed by a '.', but that is handeled in Program
    | case dir of alts end  { Model.Case $2 $4 }
    | ident                 { Model.Ident $1 }

dir : left    { Model.Left }
    | right   { Model.Right }
    | front   { Model.Front }

-- The alts is a list of alt that is sperated by ';'
alts : alts ';' alt   { $1 ++ [$3] }
     | alt            { [$1] }
     | {- empty -}    { [] }

-- an alt is a pattern, a "->" and a list of commands
alt : pat "->" cmds { Alt $1 $3 }

-- patterns are either Empty, Lambda, Debris, Asteroid, Boundary or Underscore
-- These are the contents of a .space file
pat : e         { Model.Empty }
    | lambda    { Model.Lambda }
    | debris    { Model.Debris }
    | asteroid  { Model.Asteroid }
    | boundary  { Model.Boundary }
    | '_'       { Model.Underscore }

{

happyError _ = error "parse error"

}