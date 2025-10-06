# Hatalog
A vanilla semi-naive implementation of Datalog in Haskell.

## Usage

Run the test suite with:
```bash
cabal run Hatalog-test
```

Run with GHCi:
```bash
> cabal repl
Resolving dependencies...
Build profile: -w ghc-9.6.7 -O1
In order, the following will be built (use -v for more details):
 - Hatalog-0.1.0.0 (interactive) (lib) (configuration changed)
Configuring library for Hatalog-0.1.0.0...
Preprocessing library for Hatalog-0.1.0.0...
GHCi, version 9.6.7: https://www.haskell.org/ghc/  :? for help
Ok, one module loaded.
ghci> import qualified Datalog as DL
ghci> :t DL.unify
DL.unify :: Atom Term -> Atom Const -> Maybe Subst
ghci>
```
You may see test suites for examples of usage, including

```haskell
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
```

## Reflections

During implementation, It comes to me that substitution is a special form of unification and the whole process of solving a datalog constraint is quite similar to that of the Hindley-Milner type inference algorithm. They follows similar pattern: generate constraints, unify to find solutions, apply substitutions to propagate knowledge, and iterate toward a fixed point. The`Subst = [(Var, Term)]` in Datalog mirrors the type substitution `[(TypeVar, Type)]` in HM, and unifying atoms with facts is essentially the same process as unifying type expressions during inference.

Of course, termination conditions differ between the two domains. In type inference, unification must perform an self-occurence check to prevent infinite types as one cannot unify a type variable `α` with `α → β` because that creates a circular definition and the overall goal is to find the most general unifier. In Datalog, however, unification has a simpler termination criterion: we succeed when we match a term-based atom against an all-ground atom containing only constants. There's no risk of self-occurrence because one side is always fully instantiated. This asymmetry—variables meeting constants—is what makes Datalog unification simpler and guaranteed to terminate, unlike the general unification problem in type inference.

Simple searching reveal to me other unification/logic engines such ea eqlog, which applys equational unification for type checking. I am also interested in how unification method can combined with bidirectional type checking to achieve a more powerful type inference with less annotation burden.
