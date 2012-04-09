module Parser 
       (clausesFromFile, goalFromString)
where

import Text.ParserCombinators.Parsec

import Ast

---------------------------------------------------------------------
-- Expernal Interface
---------------------------------------------------------------------

clausesFromFile filename = parseFromFile program filename

goalFromString string = parse goal "<goalstring>" string



----------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------
comment = do char '%' 
             manyTill anyChar (try newline)
             return ()
          
spacesOrComments = skipMany ((space >> return()) <|> comment)

csymb c = (try(spacesOrComments >> char c) >> spacesOrComments)
symb s = (try(spacesOrComments >> string s) >> spacesOrComments)

goal :: Parser Goal
goal = do symb "?-"
          ts <- terms
          csymb '.'
          return ts

program :: Parser Program
program = do spacesOrComments
             mixed <- many1 clauseOrFunction
             return ([ c | Left c <- mixed], [ f | Right f <- mixed]) 



clauseOrFunction :: Parser (Either Clause Func)
clauseOrFunction =  (try clause >>= return . Left)
                <|> (try function >>= return . Right)

clause :: Parser Clause
clause = do t <- term
            body <- option []
                    (symb ":-" >> terms)
            csymb '.'
            return (t, body)
            
function :: Parser Func
function = do name <- ident
              args <- parens $ sepBy1 variable (csymb ',')
              body <- csymb ':' >> term
              csymb '.'
              return (name, (args, body))

term :: Parser Term
term =  (variable >>= return . Var)
    <|> literal
    <|> (list     <?> "list term")
    <|> intval

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

variable :: Parser String
variable = (do c <- upper <|> char '_'
               cs <- many (alphaNum <|> char '_')
               return (c:cs)) <?> "variable"

intval :: Parser Term
intval = (do digits <- many1 digit
             return $ Val $ read digits) <?> "integer"



