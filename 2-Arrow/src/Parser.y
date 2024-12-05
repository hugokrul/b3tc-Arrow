{
module Parser where

import Model
import Debug.Trace
}

%name parser
%tokentype { Token }

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
Program : Program '.' rule            { $3 : $1 }
        | Program '.'                 { $1 }
        | rule                        { [$1] }
        | {- empty -}                 { [] }

rule : ident "->" cmds  { Rule $1 $3}

cmds : cmds ',' cmd     { $3 : $1 }
     | cmd              { [$1] }
     | {- empty -}      { [] }

cmd : go                    { Model.Go }
    | take                  { Model.Take }
    | mark                  { Model.Mark }
    | nothing               { Model.CmdNothing }
    | turn left             { Model.Turn Model.Left }
    | turn right            { Model.Turn Model.Right }
    | turn front            { Model.Turn Model.Front }
    | case dir of alts end  { Model.Case $2 $4 }
    | ident                 { Model.Ident $1 }

dir : left    { Model.Left }
    | right   { Model.Right }
    | front   { Model.Front }

alts : alts ';' alt   { $3 : $1 }
     | alt            { [$1] }
     | {- empty -}    { [] }

alt : pat "->" cmds { Alt $1 $3 }

pat : e         { Model.Empty }
    | lambda    { Model.Lambda }
    | debris    { Model.Debris }
    | asteroid  { Model.Asteroid }
    | boundary  { Model.Boundary }
    | '_'       { Model.Underscore }

{

happyError _ = error "parse error"

}