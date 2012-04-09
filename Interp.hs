{-
  A basic parser and interpreter for a cut-down version of Prolog

  Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
module Interp where

import Data.List (nub)
import Control.Monad(liftM)
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint ((<>),(<+>),($$))

import Ast
import Parser

----------------------------------------------------------------------
-- Interpreter
----------------------------------------------------------------------

type Unifier = [(Variable, Term)]

compose u1 u2 = (map (\(v, t) -> (v, subs u2 t)) u1) ++ u2

occursIn :: Variable -> Term -> Bool
occursIn v (Var x)     = v == x
occursIn v (Comp _ ms) = any (occursIn v) ms
occursIn v (Val _)     = False

subs :: Unifier -> Term -> Term
subs u t@(Var x)   = maybe t id (lookup x u)
subs u (Comp n ts) = Comp n (map (subs u) ts)
subs u t@(Val _)   = t

unify :: Term -> Term -> Maybe Unifier
unify (Var x) (Var y) | x == y         = return []
unify (Var x) t | not(x `occursIn` t)  = return [(x, t)]
unify t v@(Var _)                      = unify v t
unify (Comp m ms) (Comp n ns) | m == n = unifyList ms ns
unify (Val x) (Val y) | x == y         = return []
unify _ _                              = Nothing

unifyList (t : ts) (r : rs) =
    do u1 <- unify t r
       u2 <- unifyList (map (subs u1) ts) (map (subs u1) rs)
       return $ u1 `compose` u2
unifyList [] [] = Just []
unifyList _ _   = Nothing

variables ts = nub $ varsList ts
    where vars (Var x) = [x]
          vars (Comp _ ts) = varsList ts
          vars (Val _) = []
          varsList ts = [ v | t <- ts, v <- vars t]

freshen bound (tc, tb) = (subs sub tc, map (subs sub) tb)
    where vars = variables(tc : tb)
          sub = [ (v, Var $ nextVar 0 v) | v <- vars, v `elem` bound]
          nextVar i v = let v' = "_" ++ show i ++ "_" ++ v in
                        if v' `elem` bound then nextVar (i+1) v
                        else v'

-- | Evaluate a grounded arithmetic term. That is, a term without variables
eval :: Program -> Term -> Int
eval prog (Var _) = error "Non-instantiated arithmetic term"
eval prog (Val n) = n
eval prog (Comp "plus" [t1, t2]) =
  let n1 = eval prog t1
      n2 = eval prog t2
  in n1 + n2
eval prog@(_, functions) (Comp f args) | Just (vars, body) <- lookup f functions =
  eval prog (subs (zip vars args) body)


evalIs :: Program -> Term -> Maybe Unifier
evalIs prog (Comp "is" [Var x, t]) = return [(x, Val $! eval prog t)]
evalIs prog (Comp "is" [t1, t2])   = if eval prog t1 == eval prog t2 then return []
                                     else Nothing

evalCond :: Program -> Term -> Bool
evalCond prog (Comp "lt" [t1, t2]) = eval prog t1 < eval prog t2
evalCond prog (Comp n args) = if n `elem` conditionComps then error $ "Wrong number of arguments for " ++ n
                              else error $ "Unknown operator " ++ n

conditionComps = ["lt"]
isCond t@(Comp n _) = n `elem` conditionComps
isCond _ = False

symbolicCond t | isCond t = not $ null $ variables [t]
symbolicCond _ = False

nonSymbolicCond t = isCond t && (not $ symbolicCond t)

normalizeGoal :: Program -> Goal -> Maybe Goal
normalizeGoal prog (t1@(Comp "is" _) : rest) = do u <- evalIs prog t1
                                                  return $ map (subs u) rest
normalizeGoal prog (t1 : rest) | nonSymbolicCond t1 = if evalCond prog t1 then Just rest
                                                      else Just []
normalizeGoal prog g@(t1 : _) | symbolicCond t1 = Just $ n : symb ++ non
  where
    (n : non, symb) = symbolicPrefix g
    symbolicPrefix (t : rest) | symbolicCond t = let (non, symb) = symbolicPrefix rest
                                                 in  (non, t : symb)
    symbolicPrefix nonsymb = (nonsymb, [])
normalizeGoal _ _ = Nothing

newtype Solution = Solution ([(Variable, Term)], Terms)
                 deriving (Eq, Read)

instance Show Solution where
  show (Solution(bindings, conds)) = PP.render(renderB bindings $$ renderC conds)
    where
      renderB bindings = PP.braces $ PP.vcat $ map renderBindings bindings

      renderBindings (var, term) = PP.text var <+> PP.equals <+> renderT term

      renderT (Var v) = PP.text v
      renderT (Val n) = PP.int n
      renderT (Comp a []) = PP.text a
      renderT comp@(Comp f args) =
        case listTerm comp of
          Just tt -> PP.brackets $ renderTerms tt
          Nothing -> PP.text f <> (PP.parens $ renderTerms args)

      renderTerms terms = PP.sep $ PP.punctuate PP.comma $ map renderT terms

      renderC [] = PP.empty
      renderC conds = PP.text "Conditions:" $$ (PP.nest 4 $ renderTerms conds)

      listTerm (Comp "[]" [])    = return []
      listTerm (Comp "." [h, t]) = do tt <- listTerm t
                                      return $ h:tt
      listTerm _                 = Nothing

data SearchTree = Sol Solution
                | Node Goal [SearchTree]
                  deriving (Eq, Show, Read)

-- Uses the List monad for backtracking
solve :: Program -> Goal -> [SearchTree]
solve _ g@(r : conds) | isReportGoal r =  return $ Sol $ getSolution g
solve prog@(clauses,_) g@(t1 : ts) = return $ Node g trees
    where trees =
            case normalizeGoal prog g of
              Just [] -> []
              Just ng -> solve prog ng
              Nothing -> do c <- clauses
                            let (tc, tsc) = freshen (variables g) c
                            case unify tc t1 of
                              Just u -> do
                                let g' = map (subs u) $ tsc ++ ts
                                solve prog g'
                              Nothing -> []
--solve _ _ = []

makeReportGoal goal = [Comp "_report" reportVars]
    where reportVars = map (\ v -> Comp "=" [Comp v [], Var v]) vars
          vars = variables goal

isReportGoal (Comp "_report" _) = True
isReportGoal _                  = False

getSolution ((Comp "_report" args) : conds) = Solution (sol, conds)
    where sol = map (\ (Comp "=" [Comp v [], t]) -> (v, t)) args

-- Use the trick of inserting an extra reporting goal
makeReportTree prog goal = Node goal $ solve prog (goal ++ makeReportGoal goal)


----------------------------------------------------------------------
-- Traveral of Search Trees
----------------------------------------------------------------------

-- Depth first
dfs :: SearchTree -> [Solution]
dfs (Sol sols) = [sols]
dfs (Node g st) = [ s | t <- st, s <- dfs t]

-- Breath first
bfs :: SearchTree -> [Solution]
bfs t = trav [t]
    where trav [] = []
          trav ((Sol x) : q) = x : trav q
          trav ((Node _ st)  : q) = trav (q ++ st)


----------------------------------------------------------------------
-- Testing
----------------------------------------------------------------------

test filename goalString search =
    do Right p <- clausesFromFile filename
       let Right g = goalFromString goalString
       let t = makeReportTree p g
       return $ search t

tree filename goalString =
    do Right p <- clausesFromFile filename
       let Right g = goalFromString goalString
       let t = makeReportTree p g
       return $ t


siblings = test "siblings.pl" "?- sibling(homer, X)."
siblingsDFS = siblings dfs
siblingsBFS = siblings bfs


nats = test "nats.pl" "?- natlist(X)."
natsDFS = liftM (take 10) $ nats dfs
natsBFS = liftM (take 10) $ nats bfs
