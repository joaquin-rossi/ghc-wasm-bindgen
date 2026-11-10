module Main (main) where

import Control.Monad (forM_)
import Data.Char (isAlphaNum)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import GhcWasmBindgen

usage :: String
usage =
  unlines
    [ "usage:"
    , "  ghc-wasm-bindgen build MODULE.hs --name NAME --out DIR"
    , "  ghc-wasm-bindgen generate MODULE.hs --name NAME --hs-out DIR --js-out DIR [-i DIR]..."
    , ""
    , "build     From the cabal project that contains MODULE.hs, writes NAME.js,"
    , "          NAME.d.ts, NAME.wasm and the JS runtime to --out."
    , "          Needs cabal, and wasm32-wasi-cabal, wasm32-wasi-ghc and wasm-opt"
    , "          (ghc-wasm-meta) on PATH."
    , "generate  For plain source files: writes NameWasmBindings.hs and the Haskell"
    , "          runtime to --hs-out, NAME.js, NAME.d.ts and the JS runtime to --js-out."
    , "          Compiling them is up to you; NAME.js loads NAME.wasm from its own"
    , "          directory."
    , ""
    , "  MODULE.hs      module whose export list is the API (e.g. src/ShapeWasm.hs)"
    , "  --name NAME    package and file name (e.g. shape)"
    , "  --out DIR      output directory (e.g. pkg/src)"
    , "  -i DIR         import directory for the modules MODULE.hs imports"
    , "                 (MODULE.hs's own source root is added)"
    ]

data Command = Build Options | Generate GenerateOptions

parse :: [String] -> Either String Command
parse = \case
  "build" : rest -> Build <$> go rest (Options "" "" "")
    (\f o -> o {optModule = f}) optModule
    (\n o -> o {optName = n})
    [("--out", \d o -> o {optOut = d})]
  "generate" : rest -> Generate <$> go rest (GenerateOptions "" "" "" "" [])
    (\f o -> o {genModule = f}) genModule
    (\n o -> o {genName = n})
    [ ("--hs-out", \d o -> o {genHsOut = d})
    , ("--js-out", \d o -> o {genJsOut = d})
    , ("-i", \d o -> o {genImportDirs = genImportDirs o ++ [d]})
    ]
  _ -> Left "expected a command: build or generate"
  where
    go args o setModule getModule setName flags = case args of
      [] -> Right o
      "--name" : n : rest -> name n >>= \n' -> go rest (setName n' o) setModule getModule setName flags
      flag : value : rest | Just set <- lookup flag flags -> go rest (set value o) setModule getModule setName flags
      flag@('-' : _) : _ -> Left ("unknown option " ++ flag)
      f : rest
        | null (getModule o) -> go rest (setModule f o) setModule getModule setName flags
        | otherwise -> Left ("unexpected argument " ++ f)
    name n
      | n `elem` ["ghc_wasm_bindgen_internal", "ghc_wasm_jsffi"] = Left ("--name " ++ n ++ " would overwrite the runtime files")
      | null n || not (all (\c -> isAlphaNum c || c `elem` ("_-" :: String)) n) = Left ("--name must be letters, digits, _ or -: " ++ show n)
      | otherwise = Right n

main :: IO ()
main = do
  args <- getArgs
  if args `elem` [["-h"], ["--help"]]
    then putStr usage
    else case parse args of
      Left e -> hPutStrLn stderr (e ++ "\n\n" ++ usage) >> exitFailure
      Right (Build o)
        | any null [optModule o, optName o, optOut o] -> hPutStrLn stderr usage >> exitFailure
        | otherwise -> report (optModule o) (build o) (\out -> ["ghc-wasm-bindgen: built " ++ out])
      Right (Generate o)
        | any null [genModule o, genName o, genHsOut o, genJsOut o] -> hPutStrLn stderr usage >> exitFailure
        | otherwise -> report (genModule o) (generate o) (map ("wrote " ++))

report :: FilePath -> IO (Either [String] a) -> (a -> [String]) -> IO ()
report file act ok =
  act >>= \case
    Right a -> mapM_ putStrLn (ok a)
    Left errs -> do
      hPutStrLn stderr ("ghc-wasm-bindgen: cannot process " ++ file ++ ":")
      forM_ errs (hPutStrLn stderr . ("  - " ++))
      exitFailure
