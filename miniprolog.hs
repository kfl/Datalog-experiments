{-
  Reference solution for Advanced Programming exam question 2 and 3.
  A basic parser and interpreter for a cut-down version of Prolog

  Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
import Text.ParserCombinators.Parsec
import Data.List (nub)

----------------------------------------------------------------------
-- Abstract Syntax Tree
----------------------------------------------------------------------

type Goal = [Term]
type Program = Clauses
type Clauses = [Clause]
type Clause = (Term, Terms) -- head and body
data Term = Var Variable
          | Comp Ident [Term]
            deriving (Eq, Show, Read)
type Terms = [Term]
type Ident = String
type Variable = String

----------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------

csymb c = (try(spaces >> char c) >> spaces)
symb s = (try(spaces >> string s) >> spaces)

goal :: Parser Goal
goal = do symb "?-" 
          ts <- terms 
          csymb '.'
          return ts

program :: Parser Program
program = many clause

clause :: Parser Clause
clause = do t <- term
            body <- option []
                    (symb ":-" >> terms)
            csymb '.'
            return (t, body)

term :: Parser Term
term =  variable
    <|> literal  
    <|> list     <?> "list term"

terms :: Parser Terms
terms = sepBy1 term (csymb ',')

literal :: Parser Term
literal = do id <- ident    
             option (Comp id [])
                    (parens terms >>= return . Comp id)

parens :: Parser p -> Parser p
parens p = between (csymb '(') (csymb ')') p 

list :: Parser Term
list = between (csymb '[') (csymb ']') 
               (option emptyListTerm listTerms) 

listTerms :: Parser Term
listTerms =
    do heads <- terms
       tail <- option emptyListTerm
                      (csymb '|' >> term)
       return (foldr cons tail heads)

emptyListTerm :: Term
emptyListTerm = Comp "[]" []

cons :: Term -> Term -> Term
cons h t = Comp "." [h,t]

ident :: Parser Ident
ident = (do c <- lower
            cs <- many (alphaNum <|> char '_')
            return (c:cs)) <?> "identifier"

variable :: Parser Variable
variable = (do c <- upper <|> char '_'
               cs <- many (alphaNum <|> char '_')
               return $ Var (c:cs)) <?> "variable"
                                

----------------------------------------------------------------------
-- Interpreter
----------------------------------------------------------------------

type Unifier = [(Variable, Term)]

occursIn :: Variable -> Term -> Bool
occursIn v (Var x)     = v == x
occursIn v (Comp _ ms) = any (occursIn v) ms

subs :: Unifier -> Term -> Term
subs u t@(Var x) = maybe t id (lookup x u)
subs u (Comp n ts) = Comp n (map (subs u) ts) 

unify :: Term -> Term -> Maybe Unifier
unify (Var x) (Var y) | x == y         = return []
unify (Var x) t | not(x `occursIn` t)  = return [(x, t)]
unify t v@(Var _)                      = unify v t
unify (Comp m ms) (Comp n ns) | m == n = unifyList ms ns
unify _ _                              = Nothing

unifyList (t : ts) (r : rs) = 
    do u1 <- unify t r
       u2 <- unifyList (map (subs u1) ts) (map (subs u1) rs)
       return $ u1 ++ u2
unifyList [] [] = Just []
unifyList _ _   = Nothing

variables ts = nub $ varsList ts
    where vars (Var x) = [x]
          vars (Comp _ ts) = varsList ts
          varsList ts = [ v | t <- ts, v <- vars t]

freshen bound (tc, tb) = (subs sub tc, map (subs sub) tb)
    where vars = variables(tc : tb)
          sub = [ (v, Var $ nextVar 0 v) | v <- vars, v `elem` bound]
          nextVar i v = let v' = "_" ++ show i ++ "_" ++ v in
                        if v' `elem` bound then nextVar (i+1) v
                        else v'

data SearchTree = Solution [(Variable, Term)]
                | Node Goal [SearchTree]
                  deriving (Eq, Show, Read)

solve :: Program -> Goal -> [SearchTree]
solve prog g@(t1 : ts) = return $ Node g trees
    where trees = do c <- prog
                     let (tc, tsc) = freshen (variables g) c
                     case unify tc t1 of
                       Just u -> do 
                         let g' = map (subs u) $ tsc ++ ts
                         solve prog g' 
                       Nothing -> []
solve _ [r] | isReportGoal r =  return $ Solution $ getSolution r
solve _ _ = []

makeReportGoal goal = [Comp "_report" reportVars]
    where reportVars = map (\ v -> Comp "=" [Comp v [], Var v]) vars
          vars = variables goal

isReportGoal (Comp "_report" _) = True
isReportGoal _                  = False

getSolution (Comp "_report" args) = sol
    where sol = map (\ (Comp "=" [Comp v [], t]) -> (v, t)) args

-- Use the trick of inserting an extra reporting goal
makeTopTree prog goal = Node goal $ solve prog (goal ++ makeReportGoal goal)


----------------------------------------------------------------------
-- Traveral of Search Trees
----------------------------------------------------------------------

-- Depth first
dfs :: SearchTree -> [[(Variable, Term)]]
dfs (Solution sols) = [sols]
dfs (Node g st) = [ s | t <- st, s <- dfs t]

-- Breath first
bfs :: SearchTree -> [[(Variable, Term)]]
bfs t = trav [t]
    where trav [] = []
          trav ((Solution x) : q) = x : trav q
          trav ((Node _ st)  : q) = trav (q ++ st)


----------------------------------------------------------------------
-- Testing
----------------------------------------------------------------------

testSibling =
    do Right p <- parseFromFile program "siblings.pl"
       let Right g = parse goal "<string>" "?- sibling(homer, X)."
       return $ makeTopTree p g

testNats =
    do Right p <- parseFromFile program "nats.pl"
       let Right g = parse goal "<string>" "?- natlist(X)."
       return $ makeTopTree p g

