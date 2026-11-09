-- | Reads the export module with the GHC API and builds the
-- 'GhcWasmBindgen.Model.Api' model.
--
-- This is the only module that imports GHC; a GHC upgrade should only touch
-- this file.
module GhcWasmBindgen.Inspect
  ( InspectOptions (..)
  , Source (..)
  , inspect
  ) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (isPrefixOf, nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Directory (makeAbsolute)
import System.FilePath (joinPath, splitDirectories, takeDirectory)

import GHC
import GHC.Builtin.Types (boolTyCon, charTyCon, doubleTyCon, floatTyCon, intTyCon, integerTyCon, listTyCon, maybeTyCon, naturalTyCon, wordTyCon)
import GHC.Core.DataCon (dataConOrigArgTys)
import GHC.Core.InstEnv (ClsInst (..))
import GHC.Core.Multiplicity (scaledThing)
import GHC.Core.TyCon (isAlgTyCon, isBoxedTupleTyCon, tyConName)
import GHC.Core.Type (expandTypeSynonyms, mkTyConTy, splitFunTy_maybe, splitFunTys, splitTyConApp_maybe)
import GHC.Data.FastString (unpackFS)
import GHC.Data.StringBuffer (hGetStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Parser.Header (getImports)
import GHC.Paths (libdir)
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.FieldLabel (flLabel)
import GHC.Types.Name (getOccString, isSymOcc, nameModule_maybe, nameOccName)
import GHC.Unit.Types (unitString)
import GHC.Utils.Outputable (showPprUnsafe)
import Language.Haskell.Syntax.Basic (field_label)

import GhcWasmBindgen.Model

-- | What to inspect, and where its code comes from.
data InspectOptions = InspectOptions
  { inspFile :: FilePath
    -- ^ The API module's source; its module name is read from the header.
  , inspSource :: Source
  }

-- | Where the module's code and its dependencies come from.
data Source
  = PackageEnv FilePath
    -- ^ Read the module, already compiled, from the packages of this GHC
    -- environment file (written by cabal).
  | SourceFiles [FilePath]
    -- ^ Typecheck the module from source, with these extra import
    -- directories (its own source root is added) and the global package
    -- database.

-- | Describe what the module exports. On failure, returns every problem found
-- (not just the first).
inspect :: InspectOptions -> IO (Either [String] Api)
inspect opts = do
  file <- makeAbsolute (inspFile opts)
  runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    logger <- getLogger
    -- "-" ignores ambient package environments, so the result does not
    -- depend on the machine.
    let env = case inspSource opts of
          PackageEnv e -> e
          SourceFiles _ -> "-"
    (dflags1, _, _) <- parseDynamicFlags logger dflags0 [noLoc "-package-env", noLoc env]
    modName <- liftIO (readModuleName dflags1 file)
    dirs <- case inspSource opts of
      PackageEnv _ -> pure []
      SourceFiles extra -> liftIO ((:) (sourceRoot file modName) <$> mapM makeAbsolute extra)
    _ <- setSessionDynFlags dflags1 {ghcLink = NoLink, backend = noBackend, importPaths = dirs ++ importPaths dflags1}
    ok <- case inspSource opts of
      PackageEnv _ -> pure True
      SourceFiles _ -> do
        target <- guessTarget file Nothing Nothing
        setTargets [target]
        succeeded <$> load LoadAllTargets
    if not ok
      then pure (Left ["GHC could not typecheck " ++ inspFile opts ++ " (errors above)"])
      else do
        setContext [IIDecl (simpleImportDecl (mkModuleName modName))]
        apiMod <- lookupModule (mkModuleName modName) Nothing
        describe modName apiMod

-- | Module name from the file header, without typechecking anything.
readModuleName :: DynFlags -> FilePath -> IO String
readModuleName dflags file = do
  buf <- hGetStringBuffer file
  res <- getImports (initParserOpts dflags) False buf file file
  case res of
    Right (_, _, _, L _ m) -> pure (moduleNameString m)
    Left _ -> fail ("could not parse the module header of " ++ file)

-- | @/x/src/Foo/Api.hs@ with module @Foo.Api@ → @/x/src@.
sourceRoot :: FilePath -> String -> FilePath
sourceRoot file modName
  | modDirs == take n comps = joinPath rootDirs
  | otherwise = takeDirectory file
  where
    comps = splitOn modName
    dirs = splitDirectories (takeDirectory file)
    n = length comps - 1
    (rootDirs, modDirs) = splitAt (length dirs - n) dirs
    splitOn s = case break (== '.') s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn rest

------------------------------------------------------------------------------

describe :: String -> Module -> Ghc (Either [String] Api)
describe modName apiMod = do
  Just info <- getModuleInfo apiMod
  let exports = modInfoExports info
      exported = Set.fromList exports
  things <- catMaybes <$> mapM lookupName exports

  errs <- liftIO (newIORef [])
  let report e = modifyIORef' errs (e :)

  -- Functions.
  fns <- liftIO $ fmap catMaybes $ forM things $ \case
    AnId i | not (isRecordSelector i) -> do
      let name = getOccString i
      if isSymOcc (nameOccName (getName i))
        then Nothing <$ report ("(" ++ name ++ "): operators cannot be exported; export a named alias instead")
        else case runConv (fnFromType (idType i)) of
          Left e -> Nothing <$ report (name ++ " :: " ++ showPprUnsafe (idType i) ++ "\n    " ++ e)
          Right ((args, res), tcs) -> pure (Just (Fn (T.pack name) args res, tcs))
    _ -> pure Nothing

  -- Data types: every exported one plus everything reachable from them and
  -- from the function signatures.
  let exportedTcs = [tc | ATyCon tc <- things, isAlgTyCon tc, not (isClassTyCon tc)]
  adts <- liftIO (closure report exported (exportedTcs ++ concatMap snd fns))

  -- Instances the generated code would otherwise duplicate.
  hasGeneric <- Set.fromList . map (tyConName . fst) <$> filterM (hasInstance genericClass . fst) adts
  hasJson <- filterM (\(tc, _) -> (||) <$> hasInstance toJsonClass tc <*> hasInstance fromJsonClass tc) adts
  forM_ hasJson $ \(tc, _) ->
    liftIO (report (getOccString tc ++ ": already has aeson instances; ghc-wasm-bindgen derives its own with fixed options, remove them from the core"))

  -- Mangled JS names must stay distinct.
  let jsNames = [(jsIdent (fnName f), fnName f) | (f, _) <- fns]
  forM_ (Map.toList (Map.fromListWith (++) [(j, [h]) | (j, h) <- jsNames])) $ \(j, hs) ->
    when (length hs > 1) $
      liftIO (report ("functions " ++ show hs ++ " would all be exported to JS as " ++ show j))

  problems <- liftIO (readIORef errs)
  pure $
    if null problems
      then Right Api
        { apiModule = T.pack modName
        , apiTypes = sortOn adtName [adt {adtGeneric = tyConName tc `Set.notMember` hasGeneric} | (tc, adt) <- adts]
        , apiFns = sortOn fnName (map fst fns)
        }
      else Left (reverse problems)

-- | A class, as (name, modules that may define it).
type ClassRef = (String, [String])

genericClass, toJsonClass, fromJsonClass :: ClassRef
genericClass = ("Generic", ["GHC.Generics", "GHC.Internal.Generics"])
toJsonClass = ("ToJSON", ["Data.Aeson.Types.ToJSON"])
fromJsonClass = ("FromJSON", ["Data.Aeson.Types.FromJSON"])

-- | Whether a visible instance of the class exists for the type.
hasInstance :: ClassRef -> TyCon -> Ghc Bool
hasInstance (cls, mods) tc = any matches <$> getInstancesForType (mkTyConTy tc)
  where
    matches inst =
      getOccString (is_cls_nm inst) == cls
        && maybe False ((`elem` mods) . moduleNameString . moduleName) (nameModule_maybe (is_cls_nm inst))

-- | Converts every reachable type constructor, reporting unsupported ones.
-- Keeps first-seen order (exported types first).
closure :: (String -> IO ()) -> Set.Set Name -> [TyCon] -> IO [(TyCon, Adt)]
closure report exported = go Set.empty []
  where
    go _ acc [] = pure (reverse acc)
    go seen acc (tc : rest)
      | tyConName tc `Set.member` seen = go seen acc rest
      | otherwise = do
          let seen' = Set.insert (tyConName tc) seen
          case runConv (adtFromTyCon tc) of
            Left e -> report (getOccString tc ++ ": " ++ e) >> go seen' acc rest
            Right (adt, more) -> do
              let missing =
                    [getOccString tc | tyConName tc `Set.notMember` exported]
                      ++ [ getOccString tc ++ "(" ++ T.unpack (conName c) ++ ")"
                         | (c, dc) <- zip (adtCons adt) (tyConDataCons tc)
                         , getName dc `Set.notMember` exported
                         ]
              unless (null missing) $
                report (getOccString tc ++ ": used by the API, so the type and all its constructors must be exported (" ++ unwords (nub missing) ++ " missing); add `" ++ getOccString tc ++ " (..)` to the export list")
              go seen' ((tc, adt) : acc) (rest ++ more)

------------------------------------------------------------------------------
-- Type conversion. Collects the data types it encounters.

newtype Conv a = Conv {runConv :: Either String (a, [TyCon])}

instance Functor Conv where
  fmap f (Conv r) = Conv (fmap (\(a, w) -> (f a, w)) r)

instance Applicative Conv where
  pure a = Conv (Right (a, []))
  Conv f <*> Conv a = Conv $ do
    (g, w1) <- f
    (x, w2) <- a
    pure (g x, w1 ++ w2)

instance Monad Conv where
  Conv a >>= k = Conv $ do
    (x, w1) <- a
    (y, w2) <- runConv (k x)
    pure (y, w1 ++ w2)

failConv :: String -> Conv a
failConv e = Conv (Left e)

tell :: TyCon -> Conv ()
tell tc = Conv (Right ((), [tc]))

fnFromType :: Type -> Conv ([Ty], Ty)
fnFromType ty = do
  let (tvs, theta, tau) = tcSplitSigmaTy ty
  unless (null tvs) $ failConv "polymorphic functions are not supported; give the export a concrete type"
  unless (null theta) $ failConv "constraints are not supported; give the export a concrete type"
  let (args, res) = splitFunTys tau
  (,) <$> mapM (toTy . scaledThing) args <*> resultTy res
  where
    resultTy r = case splitTyConApp_maybe (expandTypeSynonyms r) of
      Just (tc, _) | getOccString tc == "IO" -> failConv "IO is not supported yet; exports must be pure"
      _ -> toTy r

toTy :: Type -> Conv Ty
toTy t0 = do
  let t = expandTypeSynonyms t0
  case splitFunTy_maybe t of
    Just _ -> failConv ("function-typed values cannot cross the boundary yet: " ++ showPprUnsafe t0)
    Nothing -> case splitTyConApp_maybe t of
      Nothing -> failConv ("unsupported type " ++ showPprUnsafe t0)
      Just (tc, args) -> case args of
        []
          | tc == doubleTyCon -> pure TDouble
          | tc == floatTyCon -> pure TFloat
          | tc == intTyCon -> pure TInt
          | tc == wordTyCon -> pure TWord
          | tc == integerTyCon -> pure TInteger
          | tc == naturalTyCon -> pure TNatural
          | tc == boolTyCon -> pure TBool
          | tc == charTyCon -> pure TChar
          | Just n <- sized "Int" intModules -> pure (TIntN n)
          | Just n <- sized "Word" wordModules -> pure (TWordN n)
          | is "Text" ["Data.Text.Internal"] -> pure TText
          | is "Day" ["Data.Time.Calendar.Days"] -> pure TDay
          | is "UTCTime" ["Data.Time.Clock.Internal.UTCTime"] -> pure TUTCTime
          | is "IntSet" ["Data.IntSet.Internal"] -> pure (TSet TInt)
        [a]
          | tc == listTyCon -> case splitTyConApp_maybe a of
              Just (c, []) | c == charTyCon -> pure TString
              _ -> TList <$> toTy a
          | tc == maybeTyCon -> TMaybe <$> toTy a
          | is "Vector" ["Data.Vector"] -> TVector <$> toTy a
          | is "NonEmpty" ["GHC.Base", "GHC.Internal.Base"] -> TNonEmpty <$> toTy a
          | is "Set" ["Data.Set.Internal"] || is "HashSet" ["Data.HashSet.Internal"] -> TSet <$> toTy a
          | is "IntMap" ["Data.IntMap.Internal"] -> TIntMap <$> toTy a
        [k, v]
          | is "Either" ["Data.Either", "GHC.Internal.Data.Either"] -> TEither <$> toTy k <*> toTy v
          | is "Map" ["Data.Map.Internal"] || is "HashMap" ["Data.HashMap.Internal"] -> do
              key <- toTy k
              case key of
                TAdt _ -> pure ()
                _ | isTextKey key -> pure ()
                _ -> failConv ("unsupported map key type " ++ showPprUnsafe k ++ "; use a string, number, Bool, Char, date or one of your data types")
              TMap key <$> toTy v
        _
          | isBoxedTupleTyCon tc, length args >= 2 -> TTuple <$> mapM toTy args
          | isAlgTyCon tc, not (isClassTyCon tc), not (fromBase tc) ->
              if null args
                then TAdt (T.pack (getOccString tc)) <$ tell tc
                else failConv ("types with parameters are not supported yet: " ++ showPprUnsafe t0)
          | otherwise -> failConv ("unsupported type " ++ showPprUnsafe t0)
        where
          is occ mods =
            getOccString tc == occ
              && maybe False ((`elem` mods) . moduleNameString . moduleName) (nameModule_maybe (tyConName tc))
          -- Int8 … Int64, Word8 … Word64.
          sized prefix mods =
            listToMaybe [n | n <- [8, 16, 32, 64], is (prefix ++ show n) mods]
          intModules = ["GHC.Int", "GHC.Internal.Int"]
          wordModules = ["GHC.Word", "GHC.Internal.Word"]

-- | Types from GHC's own libraries (Char, Integer, Float, …), which are not
-- supported unless listed above.
fromBase :: TyCon -> Bool
fromBase tc = case nameModule_maybe (tyConName tc) of
  Just m -> any (\p -> u == p || (p ++ "-") `isPrefixOf` u) ["base", "ghc-internal", "ghc-prim", "ghc-bignum"]
    where
      u = unitString (moduleUnit m)
  Nothing -> False

adtFromTyCon :: TyCon -> Conv Adt
adtFromTyCon tc = do
  unless (null (tyConTyVars tc)) $ failConv "types with parameters are not supported yet"
  cons <- forM (tyConDataCons tc) $ \dc -> do
    unless (isVanillaDataCon dc) $ failConv (getOccString dc ++ ": GADTs and existential constructors are not supported")
    tys <- mapM (toTy . scaledThing) (dataConOrigArgTys dc)
    let labels = map (unpackFS . field_label . flLabel) (dataConFieldLabels dc)
    pure Con
      { conName = T.pack (getOccString dc)
      , conFields =
          if null labels
            then Positional tys
            else Record (zip (map T.pack labels) tys)
      }
  when (null cons) $ failConv "empty data types have no values to pass"
  pure Adt {adtName = T.pack (getOccString tc), adtCons = cons, adtGeneric = True}
