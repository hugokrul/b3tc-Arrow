{
module Parser where

import Model
import Debug.Trace
}

%name parser
%tokentype { Token }

%token
  '->' { TArrow }

%%

Program : { Program }

{

happyError _ = error "parse error"

}