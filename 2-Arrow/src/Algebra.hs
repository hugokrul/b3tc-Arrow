module Algebra where

import Model

import Debug.Trace

import Data.List

{-
There are no calls to undefined rules (rules may be used before they are defined though).
There is a rule named start.
No rule is defined twice (Int, [Rule] -> Int -> Int)
voor bovenstaande 3 kan je een count functie dan maken met algebra,
count? string -> int

ik snap er de ballen van...

There is no possibility for pattern match failure, i.e., 
    all case expressions must either contain a catch-all pattern _ or contain cases for all five other options.
-}

testProgram :: Program
testProgram = Program [Rule "start" [Turn Model.Right,Go,Turn Model.Left,Ident "firstArg"],Rule "turnAround" [Turn Model.Right,Turn Model.Right],Rule "return" [Case Front [Alt Boundary [CmdNothing], Alt Underscore [Go, Ident "return"]]],Rule "firstArg" [Case Model.Left [Alt Lambda [Go,Ident "firstArg",Mark,Go],Alt Underscore [Ident "turnAround",Ident "return",Turn Model.Left,Go,Go,Turn Model.Left,Ident "secondArg"]]],Rule "secondArg" [Case Model.Left [Alt Lambda [Go,Ident "secondArg",Mark,Go],Alt Underscore [Ident "turnAround",Ident "return",Turn Model.Left,Go,Turn Model.Left]]]]

testCommand :: [Cmd]
testCommand = [Case Front [Alt Boundary [CmdNothing],Alt Underscore [Go,Ident "return"]]]


-- Exercise 5
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

startAlgebra :: Algebra Int Int Int Int Int Int
startAlgebra = (defaultAlgebra 0 0 0 0 0 0)  {
                                programAlg = ProgramAlgebra sum,
                                rulesAlg = RuleAlgebra (\name _ -> if name == "start" then 1 else 0)
                            }

duplicateAlgebra :: Algebra Bool [String] () () () ()
duplicateAlgebra = (defaultAlgebra True [] () () () ()) {
                                programAlg = ProgramAlgebra (\names -> length names /= length (nub names)),
                                rulesAlg = RuleAlgebra (\name _ -> [name])
                            }

ruleNameAlgebra :: Algebra [String] String () () () ()
ruleNameAlgebra = (defaultAlgebra [] "" () () () ()) {
                                programAlg = ProgramAlgebra id,
                                rulesAlg = RuleAlgebra const
                            }

identNameAlgebra :: Algebra [String] [String] [String] () [String] ()
identNameAlgebra = (defaultAlgebra [] [] [] () [] ())
                            {   programAlg  = ProgramAlgebra concat, -- Concatenate all collected Ident names
                                rulesAlg    = RuleAlgebra (\_ cmds -> concat cmds), -- Collect from commands in each rule
                                commandsAlg = CmdAlgebra
                                    {   cmdGo      = [],
                                        cmdTake    = [],
                                        cmdMark    = [],
                                        cmdNothing = [],
                                        cmdEmpty   = [],
                                        cmdTurn    = const [],
                                        cmdCase    = \_ alts -> concat alts, -- Collect from alternatives in a case
                                        cmdIdent   = (: []) -- Collect the Ident name
                                    },
                                altAlg      = AltAlgebra (\_ cmds -> concat cmds)
                            }

allPatterns :: Algebra [[Pat]] [[Pat]] [[Pat]] () [Pat] Pat
allPatterns = (defaultAlgebra [] [] [] () [] [])
                            {   programAlg  = ProgramAlgebra concat, -- Concatenate all collected Ident names
                                rulesAlg    = RuleAlgebra (\_ cmds -> concat cmds), -- Collect from commands in each rule
                                commandsAlg = CmdAlgebra
                                    {   cmdGo      = [],
                                        cmdTake    = [],
                                        cmdMark    = [],
                                        cmdNothing = [],
                                        cmdEmpty   = [],
                                        cmdTurn    = const [],
                                        cmdCase    = \_ alts -> [concat alts], -- Collect from alternatives in a case
                                        cmdIdent   = const [] -- Collect the Ident name
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
        foldProgram (Program rules) = p1 programAlg (map foldRule rules)
        foldRule (Rule name commands) = r1 rulesAlg name (map foldCmd commands)

        foldCmd Go = cmdGo commandsAlg
        foldCmd Take = cmdTake commandsAlg
        foldCmd Mark = cmdMark commandsAlg
        foldCmd CmdNothing = cmdNothing commandsAlg
        foldCmd CmdEmpty = cmdEmpty commandsAlg
        foldCmd (Turn turnDir) = cmdTurn commandsAlg (foldDir turnDir)
        foldCmd (Case caseDir caseAlts) = cmdCase commandsAlg (foldDir caseDir) (map foldAlt caseAlts)
        foldCmd (Ident name) = cmdIdent commandsAlg name

        foldDir Model.Left = dirLeft dirAlg
        foldDir Model.Right = dirRight dirAlg
        foldDir Model.Front = dirFront dirAlg

        foldAlt (Alt pat commands) = alt1 altAlg (foldPat pat) (map foldCmd commands)

        foldPat Empty = patEmpty patAlg
        foldPat Lambda = patLambda patAlg
        foldPat Debris = patDebris patAlg
        foldPat Asteroid = patAsteroid patAlg
        foldPat Boundary = patBoundary patAlg
        foldPat Underscore = patUnderscore patAlg

-- Exercise 6
checkProgram :: Program -> Bool
checkProgram program = do
    let start = fold startAlgebra program == 1
    let noDuplicates = not $ fold duplicateAlgebra program
    let allRules = nub $ fold ruleNameAlgebra program
    let allIdents = nub $ fold identNameAlgebra program
    let noUndefined = null (allIdents \\ allRules)
    let patternFailure = all checkAllPatterns (fold allPatterns program)

    start && noDuplicates && noUndefined && patternFailure
    where 
        checkAllPatterns :: [Pat] -> Bool
        checkAllPatterns pats = elem Model.Underscore pats || (elem Model.Empty pats && elem Model.Lambda pats && elem Model.Debris pats && elem Model.Asteroid pats && elem Model.Boundary pats)