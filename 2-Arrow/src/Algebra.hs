module Algebra where

import Model

import Debug.Trace

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

data TestAlgebra r a = RuleAlg { x :: r -> a -> a, y :: a }

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

countAlgebra :: TestAlgebra Rule Int
countAlgebra = RuleAlg x 0
    where
        x :: Rule -> Int -> Int
        x _ y = y+1

filterCountAlgebra :: String -> TestAlgebra Rule Int
filterCountAlgebra s = RuleAlg x 0
    where
        x :: Rule -> Int -> Int
        x (Rule name cmds) y = if name == s then y + 1 else y

cmdPatAlgebra :: TestAlgebra Cmd [Pat]
cmdPatAlgebra = RuleAlg x []
    where
        x :: Cmd -> [Pat] -> [Pat]
        x (Case dir []) _ = []
        x (Case dir n) acc = map pat n
        x _ _ = []

testfold :: TestAlgebra r a -> [r] -> a
testfold alg = foldr (x alg) (y alg)



-- Exercise 6

checkProgram :: Program -> Bool
checkProgram p = do
    let start = testfold (filterCountAlgebra "start") (rules p) == 1

    let noDuplicates = let
            allRules = rules p
            noDups = map ((\n -> testfold (filterCountAlgebra n) allRules == 1) . name) allRules
            in and noDups

    let noUndefined = let
            allRules = rules p
            in True

    let patternFailure = let
            allRules = rules p
            allCmds = map cmds allRules
            underscoreOrAllPatterns = map (\cmd -> let pats = testfold cmdPatAlgebra cmd in ((null pats || elem Model.Underscore pats) || checkAllPatterns pats)) allCmds
            in and underscoreOrAllPatterns

    start && noDuplicates && noUndefined && patternFailure

checkAllPatterns :: [Pat] -> Bool
checkAllPatterns pats = elem Model.Empty pats && elem Model.Lambda pats && elem Model.Debris pats && elem Model.Asteroid pats && elem Model.Boundary pats