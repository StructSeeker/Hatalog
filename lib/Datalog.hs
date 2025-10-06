module Datalog where

import Data.List (foldl', nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)

newtype Var = Var Text deriving (Eq, Show)

newtype Const = Const Text deriving (Eq, Show)

data Term where
  VarTerm :: Var -> Term
  ConstTerm :: Const -> Term
  deriving (Eq, Show)

newtype PredId = PredID Text deriving (Eq, Show)

data Pred = Pred {predName :: PredId, arity :: Int}
  deriving (Eq, Show)

-- | An atom formula whose each placeholder is occupied by Var, Const or Term
data Atom dom = Atom {pred :: Pred, terms :: [dom]}
  deriving (Eq, Show)

type Subst = [(Var, Term)]

emptySubst :: Subst
emptySubst = []

type Known = [Atom Const]

data Rule = Rule {head :: Atom Term, body :: [Atom Term]}
  deriving (Eq, Show)

-- | Apply substitution to a term
subst :: Subst -> Term -> Term
subst s (VarTerm v) = case lookup v s of
  Just t -> t
  Nothing -> VarTerm v -- Unbound variable stays as is
subst _ (ConstTerm c) = ConstTerm c

-- | Apply substitution to all terms in an atom
substAtom :: Subst -> Atom Term -> Atom Term
substAtom s (Atom p ts) = Atom p (map (subst s) ts)

-- | Unify body atom with one in known to obtain one possible subst
-- The first argument should already have previous substitutions applied
unify :: Atom Term -> Atom Const -> Maybe Subst
unify (Atom p1 terms1) (Atom p2 terms2)
  | predName p1 /= predName p2 = Nothing
  | length terms1 /= length terms2 = Nothing
  | otherwise = unifyTerms emptySubst (zip terms1 terms2)
  where
    unifyTerms :: Subst -> [(Term, Const)] -> Maybe Subst
    unifyTerms s [] = Just s
    unifyTerms s ((t, c) : rest) = case t of
      ConstTerm c' ->
        if c == c'
          then unifyTerms s rest
          else Nothing
      VarTerm v -> case lookup v s of
        Nothing -> unifyTerms ((v, ConstTerm c) : s) rest
        Just (ConstTerm c') ->
          if c == c'
            then unifyTerms s rest
            else Nothing
        Just (VarTerm _) -> Nothing -- Should not happen after substitution

-- | Based on possible substs from previous atoms, extend with new substs
evalAtom :: [Subst] -> Atom Term -> Known -> [Subst]
evalAtom prevSubsts atom known =
  concatMap (\s -> evalAtomWithSubst s atom known) prevSubsts
  where
    evalAtomWithSubst :: Subst -> Atom Term -> Known -> [Subst]
    evalAtomWithSubst s atom_ known_ =
      let substAtom' = substAtom s atom_ -- Substitute with previous knowledge
          unifications = mapMaybe (unify substAtom') known_
       in map (mergeSubst s) unifications

    -- Merge two substitutions, It might not be necessary since the unify step ignore existing variable mapping
    mergeSubst :: Subst -> Subst -> Subst
    mergeSubst s1 s2 = nub (s1 ++ s2)

-- | Convert a Term to Maybe Const (only succeeds if all variables are bound)
termToConst :: Term -> Maybe Const
termToConst (ConstTerm c) = Just c
termToConst (VarTerm _) = Nothing

-- | Apply substitution to head atom and convert to ground atom
instantiateHead :: Subst -> Atom Term -> Maybe (Atom Const)
instantiateHead s (Atom p ts) = do
  let ts' = map (subst s) ts
  consts <- traverse termToConst ts'
  return $ Atom p consts

-- | For each round and each rule, output new known facts based on the old one
evalRuleStep :: Rule -> Known -> Known
evalRuleStep (Rule headAtom bodyAtoms) known =
  let substs = foldl' (\ss atom -> evalAtom ss atom known) [emptySubst] bodyAtoms
      newFacts = mapMaybe (`instantiateHead` headAtom) substs
   in nub newFacts

-- | Continue the loop until no new fact is returned
evalDatalog :: [Rule] -> Known -> Known
evalDatalog rules = fixedPoint
  where
    fixedPoint :: Known -> Known
    fixedPoint current =
      let newFacts = concatMap (`evalRuleStep` current) rules
          combined = nub (current ++ newFacts)
       in if length combined == length current
            then current
            else fixedPoint combined

someFunc :: IO ()
someFunc = putStrLn "Datalog implementation complete"
