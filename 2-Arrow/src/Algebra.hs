module Algebra where

import Model

import Debug.Trace

import Data.List

-- Exercise 5
-- the algebra exists of a combination of algebras of all the type of the gramar
data Algebra program rule cmd dir alt pat = Algebra {   programAlg  :: ProgramAlgebra rule program,
                                                        rulesAlg    :: RuleAlgebra String cmd rule,
                                                        commandsAlg :: CmdAlgebra cmd dir alt String,
                                                        dirAlg      :: DirAlgebra dir,
                                                        altAlg      :: AltAlgebra pat cmd alt,
                                                        patAlg      :: PatAlgebra pat
                                                    }

newtype ProgramAlgebra rule program = ProgramAlgebra { p1 :: [rule] -> program }
newtype RuleAlgebra name cmd rule   = RuleAlgebra    { r1:: name -> [cmd] -> rule }
data CmdAlgebra cmd dir alt name = CmdAlgebra        { cmdGo :: cmd,
                                                       cmdTake :: cmd,
                                                       cmdMark :: cmd,
                                                       cmdNothing :: cmd,
                                                       cmdEmpty :: cmd,
                                                       cmdTurn :: dir -> cmd,
                                                       cmdCase :: dir -> [alt] -> cmd,
                                                       cmdIdent :: String -> cmd }

data DirAlgebra dir              = DirAlgebra       {  dirLeft :: dir,
                                                       dirRight :: dir,
                                                       dirFront :: dir }

newtype AltAlgebra pat cmd alt      = AltAlgebra     { alt1 :: pat -> [cmd] -> alt }

data PatAlgebra pat              = PatAlgebra        { patEmpty :: pat,
                                                       patLambda :: pat,
                                                       patDebris :: pat,
                                                       patAsteroid :: pat,
                                                       patBoundary :: pat,
                                                       patUnderscore :: pat }

-- a default algebra, this way we don't have to write unnecessary stuf, but we can update the defaultalgebra records
defaultAlgebra :: a -> b -> c -> d -> e -> f -> Algebra a b c d e f
defaultAlgebra in1 in2 in3 in4 in5 in6 = Algebra
    {
      programAlg  = ProgramAlgebra (const in1),
      rulesAlg    = RuleAlgebra (\_ _ -> in2),
      commandsAlg = CmdAlgebra
        { cmdGo      = in3,
          cmdTake    = in3,
          cmdMark    = in3,
          cmdNothing = in3,
          cmdEmpty   = in3,
          cmdTurn    = const in3,
          cmdCase    = \x y -> in3,
          cmdIdent   = const in3
        },
      dirAlg      = DirAlgebra in4 in4 in4,
      altAlg      = AltAlgebra (\_ _ -> in5),
      patAlg      = PatAlgebra in6 in6 in6 in6 in6 in6
    }

-- checks all rules for the name: 'start', when found, the flag will be true
-- higher up, an 'or' is used for checking if there is at least one start rule
startAlgebra :: Algebra Bool Bool () () () ()
startAlgebra = (defaultAlgebra False False () () () ())  {
                                programAlg = ProgramAlgebra or,
                                rulesAlg = RuleAlgebra (\name _ -> name == "start")
                            }

-- checks if the original list of names is equal to the list of names if you remove the duplicates
duplicateAlgebra :: Algebra Bool [String] () () () ()
duplicateAlgebra = (defaultAlgebra True [] () () () ()) {
                                programAlg = ProgramAlgebra (\names -> length names /= length (nub names)),
                                rulesAlg = RuleAlgebra (\name _ -> [name])
                            }

-- list of names of all defined rules 
ruleNameAlgebra :: Algebra [String] String () () () ()
ruleNameAlgebra = (defaultAlgebra [] "" () () () ()) {
                                programAlg = ProgramAlgebra id,
                                rulesAlg = RuleAlgebra const
                            }

-- list of names of all called ruled
identNameAlgebra :: Algebra [String] [String] [String] () [String] ()
identNameAlgebra = (defaultAlgebra [] [] [] () [] ())
                            -- concats the string lists of the cmdIdents (the variable calls) 
                            -- from normal commands or commands from the alt way
                            {   programAlg  = ProgramAlgebra concat,
                                rulesAlg    = RuleAlgebra (\_ cmds -> concat cmds),
                                commandsAlg = CmdAlgebra
                                    {   cmdGo      = [],
                                        cmdTake    = [],
                                        cmdMark    = [],
                                        cmdNothing = [],
                                        cmdEmpty   = [],
                                        cmdTurn    = const [],
                                        cmdCase    = \_ alts -> concat alts,
                                        cmdIdent   = (: [])
                                    },
                                altAlg      = AltAlgebra (\_ cmds -> concat cmds)
                            }

-- a list of all the patterns, for every case
allPatterns :: Algebra [[Pat]] [[Pat]] [[Pat]] () [Pat] Pat
allPatterns = (defaultAlgebra [] [] [] () [] [])
                            {   programAlg  = ProgramAlgebra concat,
                                rulesAlg    = RuleAlgebra (\_ cmds -> concat cmds),
                                commandsAlg = CmdAlgebra
                                    {   cmdGo      = [],
                                        cmdTake    = [],
                                        cmdMark    = [],
                                        cmdNothing = [],
                                        cmdEmpty   = [],
                                        cmdTurn    = const [],
                                        cmdCase    = \_ alts -> [concat alts],
                                        cmdIdent   = const []
                                    },
                                altAlg      = AltAlgebra (\pat cmds -> pat : concat (concat cmds)),
                                patAlg      = PatAlgebra {
                                    patEmpty = Empty,
                                    patLambda = Lambda,
                                    patDebris = Debris,
                                    patAsteroid = Asteroid,
                                    patBoundary = Boundary,
                                    patUnderscore = Underscore
                                }
                            }


fold :: Algebra program rule cmd dir alt pat -> Program -> program
fold (
    Algebra
    programAlg
    rulesAlg
    commandsAlg
    dirAlg
    altAlg
    patAlg
    ) = foldProgram
    where
        -- foldProgram folds all the rules of 1 program
        foldProgram (Program rules) = p1 programAlg (map foldRule rules)
        -- foldRule folds all the commands of 1 rule
        foldRule (Rule name commands) = r1 rulesAlg name (map foldCmd commands)

        -- own defined commands
        foldCmd Go = cmdGo commandsAlg
        foldCmd Take = cmdTake commandsAlg
        foldCmd Mark = cmdMark commandsAlg
        foldCmd CmdNothing = cmdNothing commandsAlg
        foldCmd CmdEmpty = cmdEmpty commandsAlg
        foldCmd (Turn turnDir) = cmdTurn commandsAlg (foldDir turnDir)
        -- also folds all the commands in the cases
        foldCmd (Case caseDir caseAlts) = cmdCase commandsAlg (foldDir caseDir) (map foldAlt caseAlts)
        foldCmd (Ident name) = cmdIdent commandsAlg name

        foldDir Model.Left = dirLeft dirAlg
        foldDir Model.Right = dirRight dirAlg
        foldDir Model.Front = dirFront dirAlg

        -- folds all the alternative commands used in cases 
        foldAlt (Alt pat commands) = alt1 altAlg (foldPat pat) (map foldCmd commands)

        foldPat Empty = patEmpty patAlg
        foldPat Lambda = patLambda patAlg
        foldPat Debris = patDebris patAlg
        foldPat Asteroid = patAsteroid patAlg
        foldPat Boundary = patBoundary patAlg
        foldPat Underscore = patUnderscore patAlg

-- Exercise
-- Defines each different test and outputs the 'and'ed tests
checkProgram :: Program -> Bool
checkProgram program = do
    -- check if the program has only 1 start command
    let start = fold startAlgebra program
    -- checks if there are no duplicates
    let noDuplicates = not $ fold duplicateAlgebra program
    -- a list of all the rules
    let allRules = nub $ fold ruleNameAlgebra program
    -- a list of all the variable rules
    let allIdents = nub $ fold identNameAlgebra program
    -- checks if there are left over variable rules from the difference between the variables and the rules
    let noUndefined = null (allIdents \\ allRules)
    -- checks if all the cases consists of either an underscore, or all the 5 different patterns.
    let patternFailure = all checkAllPatterns (fold allPatterns program)

    start && noDuplicates && noUndefined && patternFailure
    where 
        checkAllPatterns :: [Pat] -> Bool
        checkAllPatterns pats = elem Model.Underscore pats || (elem Model.Empty pats && elem Model.Lambda pats && elem Model.Debris pats && elem Model.Asteroid pats && elem Model.Boundary pats)