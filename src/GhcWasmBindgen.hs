{-# LANGUAGE TemplateHaskell #-}

-- | Bindings for a Haskell API module, in two modes:
--
-- * 'build' (cabal projects): builds an npm package (JS bindings + .wasm).
--   The package that contains the module is built natively, so the module can
--   be inspected with its real dependencies; then the Haskell bindings are
--   generated, together with the Haskell runtime module, as a package under
--   dist-newstyle/ghc-wasm-bindgen/ and compiled with wasm32-wasi-cabal
--   against the user's own project.
--
-- * 'generate' (plain source files): only writes the bindings, Haskell and
--   JS; compiling them is up to the user.
module GhcWasmBindgen
  ( Options (..)
  , build
  , GenerateOptions (..)
  , generate
  ) where

import Control.Exception (Exception, IOException, throwIO, try)
import Control.Monad (filterM, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace, toLower, toUpper)
import Data.FileEmbed (embedFile)
import Data.List (intercalate, isSuffixOf, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text.Encoding as TE
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath
import System.Process
import qualified GHC.Paths

import GhcWasmBindgen.EmitHaskell (emitHaskell)
import GhcWasmBindgen.EmitJavaScript (emitDts, emitJs)
import GhcWasmBindgen.Inspect (InspectOptions (..), Source (..), inspect)
import GhcWasmBindgen.Model (Api)

-- | Options of 'build'.
data Options = Options
  { optModule :: FilePath
    -- ^ The API module.
  , optName :: String
    -- ^ Base name of the outputs: NAME.js, NAME.d.ts, NAME.wasm.
  , optOut :: FilePath
    -- ^ Directory of the npm package to write.
  }

newtype BuildError = BuildError String

instance Show BuildError where
  show (BuildError e) = e

instance Exception BuildError

-- | Run from the user's cabal project directory. Returns the output
-- directory, or the problems that prevent building.
build :: Options -> IO (Either [String] FilePath)
build o = catchErrors $ do
  pkg <- findPackage (optModule o)
  env <- nativeEnvironment pkg
  res <- inspect InspectOptions {inspFile = optModule o, inspSource = PackageEnv env}
  traverse (buildWasm o pkg) res

-- | Options of 'generate'.
data GenerateOptions = GenerateOptions
  { genModule :: FilePath
  , genName :: String
  , genHsOut :: FilePath
    -- ^ Receives NameWasmBindings.hs and GhcWasmBindgen/Runtime.hs.
  , genJsOut :: FilePath
    -- ^ Receives NAME.js, NAME.d.ts and the JS runtime.
  , genImportDirs :: [FilePath]
  }

-- | Writes the bindings for plain source files. Returns the files written.
generate :: GenerateOptions -> IO (Either [String] [FilePath])
generate o = catchErrors $ do
  res <- inspect InspectOptions {inspFile = genModule o, inspSource = SourceFiles (genImportDirs o)}
  traverse write res
  where
    write api =
      mapM (\(path, contents) -> path <$ writeIfChanged path contents) $
        [ (genHsOut o </> hsFileName (genName o), TE.encodeUtf8 (emitHaskell api))
        , (genHsOut o </> "GhcWasmBindgen" </> "Runtime.hs", runtimeHs)
        ]
          ++ [(genJsOut o </> file, contents) | (file, contents) <- jsFiles (genName o) api]

catchErrors :: IO (Either [String] a) -> IO (Either [String] a)
catchErrors act = do
  r <- try (try act)
  pure $ case r of
    Left (e :: IOException) -> Left [show e]
    Right (Left (BuildError e)) -> Left [e]
    Right (Right result) -> result

-- | Builds the package natively and returns a GHC environment file with it
-- and its dependencies (the one @cabal exec@ provides).
nativeEnvironment :: Package -> IO FilePath
nativeEnvironment pkg = do
  let args = ["--builddir=" ++ workDir </> "native", "--with-compiler=" ++ GHC.Paths.ghc]
  run "cabal" (["build"] ++ args ++ [pkgName pkg ++ ":lib:" ++ pkgName pkg])
  env <- output "cabal" (["exec", "-v0"] ++ args ++ ["--", "sh", "-c", "cat \"$GHC_ENVIRONMENT\""])
  path <- makeAbsolute (workDir </> "native.env")
  writeIfChanged path . BC.pack =<< withoutUnbuilt env
  pure path

-- | Drops local packages that were not built (other packages of the project),
-- which GHC would otherwise fail to find.
withoutUnbuilt :: String -> IO String
withoutUnbuilt env = unlines <$> filterM keep ls
  where
    ls = lines env
    dbs = [db | l <- ls, Just db <- [stripPrefix "package-db " l]]
    keep l = case stripPrefix "package-id " l of
      Just uid | "-inplace" `isSuffixOf` uid -> or <$> mapM (\db -> doesFileExist (db </> uid <.> "conf")) dbs
      _ -> pure True

buildWasm :: Options -> Package -> Api -> IO FilePath
buildWasm o pkg api = do
  let name = optName o
      bindingsPkg = cabalName name ++ "-wasm-bindings"
      exe = cabalName name
      target = bindingsPkg ++ ":exe:" ++ exe
      hsFile = hsFileName name
      projectArg = "--project-file=" ++ (workDir </> "cabal.project")

  -- Intermediate Haskell package: the bindings and the runtime module.
  writeIfChanged (workDir </> "bindings" </> bindingsPkg <.> "cabal") (bindingsCabal bindingsPkg exe hsFile (pkgName pkg))
  writeIfChanged (workDir </> "bindings" </> hsFile) (TE.encodeUtf8 (emitHaskell api))
  writeIfChanged (workDir </> "bindings" </> "GhcWasmBindgen" </> "Runtime.hs") runtimeHs
  writeIfChanged (workDir </> "cabal.project") =<< projectFile

  -- Compile, post-link and optimize.
  run "wasm32-wasi-cabal" ["build", projectArg, target]
  wasm <- trim <$> output "wasm32-wasi-cabal" ["list-bin", "-v0", projectArg, target]
  libdir <- trim <$> output "wasm32-wasi-ghc" ["--print-libdir"]
  createDirectoryIfMissing True (optOut o)
  run (libdir </> "post-link.mjs") ["-i", wasm, "-o", optOut o </> "ghc_wasm_jsffi.js"]
  run "wasm-opt" ["-Oz", wasm, "-o", optOut o </> name <.> "wasm"]

  -- JS side.
  mapM_ (\(file, contents) -> writeIfChanged (optOut o </> file) contents) $
    ("package.json", packageJson name (pkgVersion pkg)) : jsFiles name api
  pure (optOut o)

-- | The JS bindings and runtime, by file name.
jsFiles :: String -> Api -> [(FilePath, ByteString)]
jsFiles name api =
  [ (name <.> "js", TE.encodeUtf8 (emitJs api))
  , (name <.> "d.ts", TE.encodeUtf8 (emitDts api))
  , ("ghc_wasm_bindgen_runtime.js", $(embedFile "runtime-js/ghc_wasm_bindgen_runtime.js"))
  , ("ghc_wasm_bindgen_runtime.d.ts", $(embedFile "runtime-js/ghc_wasm_bindgen_runtime.d.ts"))
  , ("ghc_wasm_bindgen_internal.js", $(embedFile "runtime-js/ghc_wasm_bindgen_internal.js"))
  , ("ghc_wasm_bindgen_internal.d.ts", $(embedFile "runtime-js/ghc_wasm_bindgen_internal.d.ts"))
  , ("ghc_wasm_jsffi.d.ts", $(embedFile "runtime-js/ghc_wasm_jsffi.d.ts"))
  ]

runtimeHs :: ByteString
runtimeHs = $(embedFile "runtime-hs/GhcWasmBindgen/Runtime.hs")

workDir :: FilePath
workDir = "dist-newstyle" </> "ghc-wasm-bindgen"

-- Cabal --------------------------------------------------------------------

data Package = Package {pkgName :: String, pkgVersion :: String}

-- | The cabal package that contains the API module: the nearest .cabal file
-- above it.
findPackage :: FilePath -> IO Package
findPackage file = makeAbsolute (takeDirectory file) >>= go
  where
    go dir = do
      cabalFiles <- filter (".cabal" `isSuffixOf`) <$> listDirectory dir
      case cabalFiles of
        f : _ -> do
          fields <- mapMaybe field . lines . BC.unpack <$> BS.readFile (dir </> f)
          case lookup "name" fields of
            Just n -> pure (Package n (fromMaybe "0" (lookup "version" fields)))
            Nothing -> throwIO (BuildError (dir </> f ++ " has no name field"))
        [] | takeDirectory dir == dir -> throwIO (BuildError ("no .cabal file found above " ++ file))
           | otherwise -> go (takeDirectory dir)
    field l = case break (== ':') l of
      (k@(c : _), ':' : v) | not (isSpace c) -> Just (map toLower (trim k), trim v)
      _ -> Nothing

-- | Imports the user's project (absolute paths: cabal resolves imports from
-- the importing file) and adds the generated packages.
projectFile :: IO ByteString
projectFile = do
  root <- getCurrentDirectory
  hasProject <- doesFileExist "cabal.project"
  hasLocal <- doesFileExist "cabal.project.local"
  pure . BC.pack . unlines $
    ["-- Generated by ghc-wasm-bindgen. Do not edit."]
      ++ ["import: " ++ root </> "cabal.project" | hasProject]
      ++ ["import: " ++ root </> "cabal.project.local" | hasLocal]
      ++ ["packages: " ++ unwords (["." | not hasProject] ++ [workDir </> "bindings"])]

bindingsCabal :: String -> String -> FilePath -> String -> ByteString
bindingsCabal pkg exe hsFile userPkg =
  BC.pack . unlines $
    [ "cabal-version: 3.0"
    , "-- Generated by ghc-wasm-bindgen. Do not edit."
    , "name: " ++ pkg
    , "version: 0"
    , "build-type: Simple"
    , ""
    , "executable " ++ exe
    , "    main-is: " ++ hsFile
    , "    other-modules: GhcWasmBindgen.Runtime"
    , "    default-language: GHC2021"
    , "    build-depends: base, " ++ userPkg ++ ", aeson >=2.2.1 && <2.3, text, ghc-experimental"
    , "    ghc-options: -no-hs-main -optl-mexec-model=reactor"
    ]

-- | @shape@ → @ShapeWasmBindings.hs@, @my-lib@ → @MyLibWasmBindings.hs@.
hsFileName :: String -> FilePath
hsFileName name = concatMap capitalize (words (map dashToSpace name)) ++ "WasmBindings.hs"
  where
    dashToSpace c = if c `elem` ("-_" :: String) then ' ' else c
    capitalize (c : cs) = toUpper c : cs
    capitalize [] = []

-- | Cabal package and component names allow letters, digits and @-@.
cabalName :: String -> String
cabalName = map (\c -> if c == '_' then '-' else c)

-- npm ----------------------------------------------------------------------

packageJson :: String -> String -> ByteString
packageJson name version =
  BC.pack . unlines $
    [ "{"
    , "    \"name\": " ++ show (map toLower name) ++ ","
    , "    \"version\": " ++ show (semver version) ++ ","
    , "    \"type\": \"module\","
    , "    \"main\": " ++ show ("./" ++ name ++ ".js") ++ ","
    , "    \"types\": " ++ show ("./" ++ name ++ ".d.ts") ++ ","
    , "    \"exports\": {"
    , "        \".\": { \"types\": " ++ show ("./" ++ name ++ ".d.ts") ++ ", \"default\": " ++ show ("./" ++ name ++ ".js") ++ " },"
    , "        \"./runtime\": { \"types\": \"./ghc_wasm_bindgen_runtime.d.ts\", \"default\": \"./ghc_wasm_bindgen_runtime.js\" },"
    , "        " ++ show ("./" ++ name ++ ".wasm") ++ ": " ++ show ("./" ++ name ++ ".wasm")
    , "    }"
    , "}"
    ]

-- | Cabal versions have any number of components; npm wants exactly three.
semver :: String -> String
semver v = intercalate "." (take 3 (splitOn '.' v ++ repeat "0"))
  where
    splitOn c s = case break (== c) s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn c rest

-- Files and processes ------------------------------------------------------

-- | Leaves the file untouched when it already has these contents, so cabal
-- and GHC don't rebuild for nothing.
writeIfChanged :: FilePath -> ByteString -> IO ()
writeIfChanged path contents = do
  exists <- doesFileExist path
  same <- if exists then (== contents) <$> BS.readFile path else pure False
  unless same $ do
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path contents

run :: FilePath -> [String] -> IO ()
run cmd args = do
  r <- try (withCreateProcess (proc cmd args) (\_ _ _ p -> waitForProcess p))
  case r of
    Left (e :: IOException) -> throwIO (cannotRun cmd e)
    Right ExitSuccess -> pure ()
    Right (ExitFailure c) -> throwIO (BuildError (unwords (cmd : args) ++ " failed (exit code " ++ show c ++ ")"))

output :: FilePath -> [String] -> IO String
output cmd args = do
  r <- try (readCreateProcessWithExitCode (proc cmd args) "")
  case r of
    Left (e :: IOException) -> throwIO (cannotRun cmd e)
    Right (ExitSuccess, out, _) -> pure out
    Right (ExitFailure c, _, err) -> throwIO (BuildError (unwords (cmd : args) ++ " failed (exit code " ++ show c ++ "):\n" ++ err))

cannotRun :: FilePath -> IOException -> BuildError
cannotRun cmd e =
  BuildError ("cannot run " ++ cmd ++ " (" ++ show e ++ "); is the WASM toolchain on PATH? (source ~/.ghc-wasm/env)")

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace
