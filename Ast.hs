module Ast where

----------------------------------------------------------------------
-- Abstract Syntax Tree
----------------------------------------------------------------------

type Goal = [Term]
type Program = (Clauses, Funcs)
type Clauses = [Clause]
type Clause = (Term, Terms) -- head and body
type Func = (Ident, ([Variable], Term)) -- name and (arguments and body)
type Funcs = [Func]
data Term = Var Variable
          | Val Int
          | Comp Ident [Term]
            deriving (Eq, Show, Read)
type Terms = [Term]
type Ident = String
type Variable = String
