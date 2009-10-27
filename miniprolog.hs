import Text.ParserCombinators.Parsec
import Data.List (nub)
import Control.Monad
import Control.Monad.State
--import Control.Monad.Trans

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

ctok c = (try(spaces >> char c) >> spaces)
stok s = (try(spaces >> string s) >> spaces)

goal :: Parser Goal
goal = do stok "?-" 
          ts <- terms 
          ctok '.'
          return ts

program :: Parser Program
program = many clause

clause :: Parser Clause
clause = do t <- term
            body <- option []
                    (stok ":-" >> terms)
            ctok '.'
            return (t, body)

term :: Parser Term
term =  (variable >>= return . Var)
    <|> literal 
    <|> list

terms :: Parser Terms
terms = sepBy1 term (ctok ',')

literal :: Parser Term
literal = do id <- ident    
             option (Comp id [])
                    (parens terms >>= return . Comp id)

parens :: Parser p -> Parser p
parens p = between (ctok '(') (ctok ')') p 

list :: Parser Term
list = between (ctok '[') (ctok ']') 
               (option emptyListTerm listTerms) 

listTerms =
    do heads <- terms
       tail <- option emptyListTerm
                      (ctok '|' >> term)
       return (foldr cons tail heads)

emptyListTerm = Comp "[]" []

cons h t = Comp "." [h,t]

ident :: Parser Ident
ident = (do c <- lower
            cs <- many (alphaNum <|> char '_')
            return (c:cs)) <?> "identifier"

variable :: Parser Variable
variable = (do c <- upper <|> char '_'
               cs <- many (alphaNum <|> char '_')
               return (c:cs)) <?> "variable"
                                

----------------------------------------------------------------------
-- Interpreter
----------------------------------------------------------------------

type Unifier = [(Variable, Term)]

occur :: Variable -> Term -> Bool
occur v (Var x)     = v == x
occur v (Comp _ ms) = any (occur v) ms

subs :: Unifier -> Term -> Term
subs u t@(Var x) = maybe t id (lookup x u)
subs u (Comp n ts) = Comp n (map (subs u) ts) 

unify :: Term -> Term -> Maybe Unifier
unify (Var x) (Var y)                  = if x < y then return [(y, Var x)]
                                         else return [(x, Var y)]
unify (Var x) t | not(x `occur` t)     = return [(x, t)]
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

data TreeState = TS {next :: Int}

nextVar x = do st <- get
               let n = next st
               put $ st{next = n+1}
               return $ "_" ++ show n ++ "_" ++ x

freshSub vs =
    do sub <- mapM freshVar vs
       return $ sub
    where freshVar v = do v' <- nextVar v
                          return $ (v, Var v')

fresh (head, body) = 
    do let vars = variables(head:body) 
       sub <- freshSub vars
       return (subs sub head, map (subs sub) body)

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
solve _ [Comp "_report" args] = return $ Solution sol
    where sol = map (\ (Comp "=" [Comp v [], t]) -> (v, t)) args
solve prog g@(t1 : ts) = return $ Node g trees
    where trees = do c <- prog
                     let (tc, tsc) = freshen (variables g) c
                     case unify tc t1 of
                       Just u -> do 
                         let g' = map (subs u) $ tsc ++ ts
                         solve prog g' 
                       Nothing -> []

{- Failed attempt at using StateT monad transformer 

-- Back-Tracking-State
type BTS a = StateT TreeState [] a

solve :: Program -> Goal -> BTS SearchTree
solve _ [Comp "_report" args] = return $ Solution sol
    where sol = map (\ (Comp "=" [Comp v [], t]) -> (v, t)) args
solve prog g@(t1 : ts) =
    do c <- lift prog
       (tc, tsc) <- fresh c
       case unify tc t1 of
         Just u -> do 
           let g' = map (subs u) $ tsc ++ ts
           t <- solve prog g' 
           return $ Node g' [t]
         Nothing -> lift []

makeTree prog goal = Node goal trees
    where trees = evalStateT (solve prog goal) initialState 
          initialState = TS 0
-}

makeTopTree prog goal = Node goal $ solve prog reportGoal
    where reportVars = map (\ v -> Comp "=" [Comp v [], Var v]) vars
          vars = variables goal
          reportGoal = goal ++ [Comp "_report" reportVars]



testSibling =
    do Right p <- parseFromFile program "siblings.pl"
       let Right g = parse goal "<string>" "?- sibling(homer, X)."
       return $ makeTopTree p g

testNats =
    do Right p <- parseFromFile program "nats.pl"
       let Right g = parse goal "<string>" "?- natlist(X)."
       return $ makeTopTree p g

-- Depth first
dfs (Solution sols) = [sols]
dfs (Node g st) = [ s | t <- st, s <- dfs t]

-- Breath first
bfs t = trav [t]
    where trav [] = []
          trav ((Solution x) : q) = x : trav q
          trav ((Node _ st)  : q) = trav (q ++ st)

