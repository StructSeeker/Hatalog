module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Datalog qualified as DL

-- Helper functions for constructing terms and atoms
varTerm :: Text -> DL.Term
varTerm = DL.VarTerm . DL.Var

var :: Text -> DL.Var
var = DL.Var

constTerm :: Text -> DL.Term
constTerm = DL.ConstTerm . DL.Const

constVal :: Text -> DL.Const
constVal = DL.Const

predicate :: Text -> Int -> DL.Pred
predicate name n = DL.Pred (DL.PredID name) n

atom :: Text -> Int -> [DL.Term] -> DL.Atom DL.Term
atom name n terms = DL.Atom (predicate name n) terms

groundAtom :: Text -> Int -> [Text] -> DL.Atom DL.Const
groundAtom name n vals = DL.Atom (predicate name n) (map constVal vals)

-- Example 1: Directed Acyclic Graph - Reachability
-- Given edges in a DAG, compute transitive closure (reachability)
dagExample :: IO ()
dagExample = do
  putStrLn "=== DAG Reachability Example ==="
  putStrLn "\nInitial edges:"
  putStrLn "  a -> b"
  putStrLn "  b -> c"
  putStrLn "  c -> d"
  putStrLn "  a -> e"
  putStrLn "  e -> f"

  let edges =
        [ groundAtom "edge" 2 ["a", "b"],
          groundAtom "edge" 2 ["b", "c"],
          groundAtom "edge" 2 ["c", "d"],
          groundAtom "edge" 2 ["a", "e"],
          groundAtom "edge" 2 ["e", "f"]
        ]

  -- Rules:
  -- reachable(X, Y) :- edge(X, Y).
  -- reachable(X, Z) :- edge(X, Y), reachable(Y, Z).
  let rule1 =
        DL.Rule
          { DL.head = atom "reachable" 2 [varTerm "X", varTerm "Y"],
            DL.body = [atom "edge" 2 [varTerm "X", varTerm "Y"]]
          }

  let rule2 =
        DL.Rule
          { DL.head = atom "reachable" 2 [varTerm "X", varTerm "Z"],
            DL.body =
              [ atom "edge" 2 [varTerm "X", varTerm "Y"],
                atom "reachable" 2 [varTerm "Y", varTerm "Z"]
              ]
          }

  let result = DL.evalDatalog [rule1, rule2] edges

  putStrLn "\nComputed reachability:"
  mapM_ print $ filter (\a -> DL.predName (DL.pred a) == DL.PredID "reachable") result
  putStrLn ""

-- Example 2: Ancestor relationship (Family Tree)
ancestorExample :: IO ()
ancestorExample = do
  putStrLn "=== Family Tree - Ancestor Example ==="
  putStrLn "\nInitial parent relationships:"

  let parents =
        [ groundAtom "parent" 2 ["alice", "bob"],
          groundAtom "parent" 2 ["alice", "charlie"],
          groundAtom "parent" 2 ["bob", "david"],
          groundAtom "parent" 2 ["bob", "eve"],
          groundAtom "parent" 2 ["charlie", "frank"]
        ]

  mapM_ print parents

  -- Rules:
  -- ancestor(X, Y) :- parent(X, Y).
  -- ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
  let rule1 =
        DL.Rule
          { DL.head = atom "ancestor" 2 [varTerm "X", varTerm "Y"],
            DL.body = [atom "parent" 2 [varTerm "X", varTerm "Y"]]
          }

  let rule2 =
        DL.Rule
          { DL.head = atom "ancestor" 2 [varTerm "X", varTerm "Z"],
            DL.body =
              [ atom "parent" 2 [varTerm "X", varTerm "Y"],
                atom "ancestor" 2 [varTerm "Y", varTerm "Z"]
              ]
          }

  let result = DL.evalDatalog [rule1, rule2] parents

  putStrLn "\nComputed ancestors:"
  mapM_ print $ filter (\a -> DL.predName (DL.pred a) == DL.PredID "ancestor") result
  putStrLn ""

-- Example 3: Simple path finding
pathExample :: IO ()
pathExample = do
  putStrLn "=== Path Finding Example ==="
  putStrLn "\nInitial edges:"

  let edges =
        [ groundAtom "edge" 2 ["1", "2"],
          groundAtom "edge" 2 ["2", "3"],
          groundAtom "edge" 2 ["3", "4"],
          groundAtom "edge" 2 ["2", "5"],
          groundAtom "edge" 2 ["5", "4"]
        ]

  mapM_ print edges

  -- Rules:
  -- path(X, Y) :- edge(X, Y).
  -- path(X, Z) :- edge(X, Y), path(Y, Z).
  let rule1 =
        DL.Rule
          { DL.head = atom "path" 2 [varTerm "X", varTerm "Y"],
            DL.body = [atom "edge" 2 [varTerm "X", varTerm "Y"]]
          }

  let rule2 =
        DL.Rule
          { DL.head = atom "path" 2 [varTerm "X", varTerm "Z"],
            DL.body =
              [ atom "edge" 2 [varTerm "X", varTerm "Y"],
                atom "path" 2 [varTerm "Y", varTerm "Z"]
              ]
          }

  let result = DL.evalDatalog [rule1, rule2] edges

  putStrLn "\nAll paths:"
  mapM_ print $ filter (\a -> DL.predName (DL.pred a) == DL.PredID "path") result
  putStrLn ""

-- Example 4: Simple inference - Socrates syllogism
syllogismExample :: IO ()
syllogismExample = do
  putStrLn "=== Syllogism Example (Socrates) ==="
  putStrLn "\nInitial facts:"

  let facts =
        [ groundAtom "human" 1 ["socrates"],
          groundAtom "human" 1 ["plato"],
          groundAtom "human" 1 ["aristotle"]
        ]

  mapM_ print facts

  -- Rule: mortal(X) :- human(X).
  let rule =
        DL.Rule
          { DL.head = atom "mortal" 1 [varTerm "X"],
            DL.body = [atom "human" 1 [varTerm "X"]]
          }

  let result = DL.evalDatalog [rule] facts

  putStrLn "\nInferred facts:"
  mapM_ print $ filter (\a -> DL.predName (DL.pred a) == DL.PredID "mortal") result
  putStrLn ""

-- Example 5: Same generation in family tree
sameGenerationExample :: IO ()
sameGenerationExample = do
  putStrLn "=== Same Generation Example ==="
  putStrLn "\nInitial parent relationships:"

  let parents =
        [ groundAtom "parent" 2 ["alice", "bob"],
          groundAtom "parent" 2 ["alice", "charlie"],
          groundAtom "parent" 2 ["dave", "eve"],
          groundAtom "parent" 2 ["dave", "frank"],
          groundAtom "parent" 2 ["bob", "george"],
          groundAtom "parent" 2 ["charlie", "henry"],
          groundAtom "parent" 2 ["eve", "iris"]
        ]

  mapM_ print parents

  -- Rules:
  -- sameGen(X, X) :- parent(_, X).  (everyone is same generation as themselves)
  -- sameGen(X, Y) :- parent(P1, X), parent(P2, Y), sameGen(P1, P2).
  let rule1 =
        DL.Rule
          { DL.head = atom "sameGen" 2 [varTerm "X", varTerm "X"],
            DL.body = [atom "parent" 2 [varTerm "P", varTerm "X"]]
          }

  let rule2 =
        DL.Rule
          { DL.head = atom "sameGen" 2 [varTerm "X", varTerm "Y"],
            DL.body =
              [ atom "parent" 2 [varTerm "P1", varTerm "X"],
                atom "parent" 2 [varTerm "P2", varTerm "Y"],
                atom "sameGen" 2 [varTerm "P1", varTerm "P2"]
              ]
          }

  let result = DL.evalDatalog [rule1, rule2] parents

  putStrLn "\nSame generation pairs:"
  mapM_ print $ filter (\a -> DL.predName (DL.pred a) == DL.PredID "sameGen") result
  putStrLn ""

-- Run all examples
runAllExamples :: IO ()
runAllExamples = do
  dagExample
  ancestorExample
  pathExample
  syllogismExample
  sameGenerationExample

-- Unit tests for individual functions
unitTests :: IO ()
unitTests = do
  putStrLn "=== Unit Tests ==="

  -- Test substitution
  putStrLn "\n1. Testing substitution:"
  let s1 = [(var "X", constTerm "a"), (var "Y", constTerm "b")]
  print $ DL.subst s1 (varTerm "X") -- Should be: ConstTerm (Const "a")
  print $ DL.subst s1 (varTerm "Z") -- Should be: VarTerm (Var "Z")
  print $ DL.subst s1 (constTerm "c") -- Should be: ConstTerm (Const "c")

  -- Test unification
  putStrLn "\n2. Testing unification:"
  let atomTerm = atom "edge" 2 [varTerm "X", varTerm "Y"]
  let atomConst = groundAtom "edge" 2 ["a", "b"]
  print $ DL.unify atomTerm atomConst
  -- Should be: Just [(Var "Y", ConstTerm (Const "b")), (Var "X", ConstTerm (Const "a"))]

  let atomTerm2 = atom "edge" 2 [constTerm "a", varTerm "Y"]
  print $ DL.unify atomTerm2 atomConst
  -- Should be: Just [(Var "Y", ConstTerm (Const "b"))]

  let atomConst2 = groundAtom "edge" 2 ["c", "d"]
  print $ DL.unify atomTerm2 atomConst2
  -- Should be: Nothing (constant mismatch)

  putStrLn "\nAll unit tests completed!"

main :: IO ()
main = runAllExamples *> unitTests
