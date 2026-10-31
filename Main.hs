-- Main.hs
-- Stage 4: CLI entry point for the Haskell frontend.
-- Parses a source file, converts it to typed AST, infers types, and writes JSON for the backend.
module Main where

import qualified HsParser
import TypeInfer
import Desugar
import AstTypes
import System.Environment (getArgs)
import System.Exit (exitFailure)

usage :: String
usage = "Usage: ./compiler <input-file> [output-json]\nExample: ./compiler test.prog typed_ast.json"

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
