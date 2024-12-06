module Algebra where

import Model

{-
There are no calls to undefined rules (rules may be used before they are defined though).
There is a rule named start.
No rule is defined twice. 

voor bovenstaande 3 kan je een count functie dan maken met algebra,
count? string -> int

ik snap er de ballen van...

There is no possibility for pattern match failure, i.e., 
    all case expressions must either contain a catch-all pattern _ or contain cases for all five other options.
-}

-- Exercise 5
type Algebra = ()
fold = undefined



-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined