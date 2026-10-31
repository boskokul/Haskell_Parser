module Main where

import qualified HsParser
import TypeInfer
import Desugar
import AstTypes
import System.Environment (getArgs)
import System.Exit (exitFailure)

usage :: String
usage = "Usage: ./compiler <input-file> [output-json]\nExample: ./compiler testLanguage.hs typed_ast.json"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> runPipeline inputFile "typed_ast.json"
    [inputFile, outputFile] -> runPipeline inputFile outputFile
    _ -> do
      putStrLn usage
      exitFailure

runPipeline :: FilePath -> FilePath -> IO ()
runPipeline inputFile outputFile = do
  parsed <- HsParser.parseFile inputFile
  case lowerProgram parsed of
    Left err -> do
      print err
      exitFailure
    Right lowered -> do
      case inferProgram lowered of
        Left err -> do
          print err
          exitFailure
        Right inferred -> do
          let canonical = desugarProgram inferred
          writeProgramJson outputFile canonical
          putStrLn ("Wrote typed AST to " ++ outputFile)
