module Main where

import           Parser                         ( parseComm )
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           System.Exit
import           Control.Monad                  ( when )
import           PPLis

import qualified Eval

---------------------------------------------------------

data Options = Options
  { optPrint :: Bool
  , optAST   :: Bool
  , optHelp  :: Bool
  }
  deriving Show

defaultOptions :: Options
defaultOptions =
  Options { optPrint = False, optAST = False, optHelp = False }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['p']
           ["print"]
           (NoArg (\opts -> opts { optPrint = True }))
           "Imprimir el programa de entrada."
  , Option ['a']
           ["AST"]
           (NoArg (\opts -> opts { optAST = True }))
           "Mostrar el AST del programa de entrada."
  , Option ['h']
           ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "Imprimir guia de uso."
  ]

finalOptions :: [String] -> IO (Options, [String])
finalOptions argv = case getOpt Permute options argv of
  (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Uso:"

main :: IO ()
main = do
  s : opts   <- Env.getArgs
  (opts', _) <- finalOptions opts
  runOptions s opts'

runOptions :: FilePath -> Options -> IO ()
runOptions fp opts
  | optHelp opts = putStrLn (usageInfo "Uso: " options)
  | otherwise = do
    s <- readFile fp
    case parseComm fp s of
      Left  error -> print error
      Right ast   -> if
        | optAST opts       -> print ast
        | optPrint opts     -> putStrLn (renderComm ast)
        | otherwise         -> print (Eval.eval ast)

