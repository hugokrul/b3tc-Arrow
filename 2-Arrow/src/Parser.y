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

Program : { Rule }

Rule : ident "->" Cmds '.' { Rule $1 $3 }

Cmds : Cmds ',' Cmd     { $3 : $1 }
     | Cmd              { [$1] }
     |                  { [] }

Cmd : go                { Model.Go }
    | take              { Model.Take }
    | mark              { Model.Mark }
    | nothing           { Model.CmdNothing }
    | turn left         { Model.Turn Model.Left }
    | turn right        { Model.Turn Model.Right }
    | turn front        { Model.Turn Model.Front }
    -- | case _ of + end   {  }
    | ident             { Model.Ident $1 }

{

happyError _ = error "parse error"

}