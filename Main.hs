{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs

import Ast
import qualified Parser as P
import qualified Interp as I

data Search = DFS | BFS
            deriving (Show, Eq, Data, Typeable)
instance Default Search where
  def = DFS

searchFunction DFS = I.dfs
searchFunction BFS = I.bfs


data Options =
  Options { search  :: Search
          , clauses :: FilePath
          , goal    :: String
          }
  deriving (Show, Data, Typeable)

startOptions =
  Options { search = def &= help "specify wether to use DFS or BFS"
          , clauses = def &= typFile &= help "prolog file with clauses"
          , goal = def &= args &= typ "GOALSTRING"
          }
  &= summary "Pure Prolog Interpreter v1.0, (C) Ken Friis Larsen 2012"


checkOptions = do
  opts <- cmdArgs startOptions
  case clauses opts of
    "" -> error "You must provide a clauses file with the -c flag"
    _ -> case goal opts of
      "" -> error "You must provide a goal to prove"
      _ -> return opts

main = do
  opts <- checkOptions
  Right p <- P.clausesFromFile $ clauses opts
  let searchF = searchFunction $ search opts
      Right g = P.goalFromString $ goal opts
      t = I.makeReportTree p g
      solutions = searchF t
  case solutions of
    [] -> print "no solutions"
    _  -> mapM_ print solutions
