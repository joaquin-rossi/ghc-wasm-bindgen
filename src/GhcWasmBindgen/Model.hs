-- | Intermediate representation between "GhcWasmBindgen.Inspect" (the only
-- module that talks to the GHC API) and the two emitters.
module GhcWasmBindgen.Model
  ( Api (..)
  , Fn (..)
  , Adt (..)
  , Con (..)
  , Fields (..)
  , Ty (..)
  , Marshal (..)
  , marshal
  , isTextKey
  , isAllNullary
  , jsIdent
  , exportSymbol
  , isPlainIdent
  ) where

import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper)
import Data.Text (Text)
import qualified Data.Text as T

-- | Everything the export module exposes.
data Api = Api
  { apiModule :: Text
    -- ^ Haskell module name, e.g. @WasmApi@.
  , apiTypes :: [Adt]
    -- ^ Data types that cross the boundary, sorted by name (deterministic output).
  , apiFns :: [Fn]
    -- ^ Exported functions, sorted by name.
  }
  deriving (Eq, Show)

-- | An exported function.
data Fn = Fn
  { fnName :: Text
    -- ^ Haskell name.
  , fnArgs :: [Ty]
  , fnResult :: Ty
  }
  deriving (Eq, Show)

-- | A data type used by the API.
data Adt = Adt
  { adtName :: Text
  , adtCons :: [Con]
  , adtGeneric :: Bool
    -- ^ The generated Haskell module must derive @Generic@ (False when the core already has it).
  }
  deriving (Eq, Show)

-- | A data constructor.
data Con = Con
  { conName :: Text
  , conFields :: Fields
  }
  deriving (Eq, Show)

-- | A constructor's fields: named (record) or positional.
data Fields
  = Record [(Text, Ty)]
  | Positional [Ty]
  deriving (Eq, Show)

-- | Types that may appear in an exported signature.
data Ty
  = TDouble
  | TFloat
  | TInt
  | TWord
  | TIntN Int
    -- ^ Int8, Int16, Int32, Int64.
  | TWordN Int
    -- ^ Word8, Word16, Word32, Word64.
  | TInteger
  | TNatural
  | TBool
  | TChar
  | TString
  | TText
  | TDay
  | TUTCTime
  | TList Ty
  | TVector Ty
  | TNonEmpty Ty
  | TSet Ty
    -- ^ Set, HashSet, IntSet.
  | TMap Ty Ty
    -- ^ Map, HashMap.
  | TIntMap Ty
  | TMaybe Ty
  | TEither Ty Ty
  | TTuple [Ty]
  | TAdt Text
  deriving (Eq, Show)

-- | Map keys that aeson writes as object keys (text); any other key type
-- makes the map an array of pairs.
isTextKey :: Ty -> Bool
isTextKey = \case
  TDouble -> True
  TFloat -> True
  TInt -> True
  TWord -> True
  TIntN _ -> True
  TWordN _ -> True
  TInteger -> True
  TNatural -> True
  TBool -> True
  TChar -> True
  TString -> True
  TText -> True
  TDay -> True
  TUTCTime -> True
  _ -> False

-- | How a value of a top-level argument or result type crosses the JSFFI.
data Marshal
  = Native   -- ^ Double, Int, Bool: passed as JS numbers/booleans.
  | Str      -- ^ String: JSString, no JSON.
  | TextStr  -- ^ Text: JSString, no JSON.
  | Json     -- ^ Everything else: JSON in a JSString.
  deriving (Eq, Show)

-- | How a top-level argument or result of this type crosses the JSFFI.
marshal :: Ty -> Marshal
marshal = \case
  TDouble -> Native
  TInt -> Native
  TBool -> Native
  TString -> Str
  TText -> TextStr
  _ -> Json

-- | aeson's @allNullaryToStringTag@ (on by default): a type whose
-- constructors are all nullary is encoded as a bare string.
isAllNullary :: Adt -> Bool
isAllNullary = all nullary . adtCons
  where
    nullary c = case conFields c of
      Positional [] -> True
      Record [] -> True
      _ -> False

-- | A Haskell name as a JS/TS identifier: primes become underscores and
-- reserved words get a trailing underscore (@delete@ → @delete_@).
jsIdent :: Text -> Text
jsIdent n
  | m `elem` reserved = m <> "_"
  | otherwise = m
  where
    m = T.replace "'" "_" n

-- | Name of the wasm export (and therefore C symbol) for a function. The
-- prefix keeps user names like @log@ or @free@ from clashing with libc.
exportSymbol :: Fn -> Text
exportSymbol f = "bg_" <> T.replace "'" "_" (fnName f)

-- | Can be used unquoted as a property key.
isPlainIdent :: Text -> Bool
isPlainIdent t = case T.uncons t of
  Just (c, rest) -> start c && T.all part rest
  Nothing -> False
  where
    start c = isAsciiLower c || isAsciiUpper c || c == '_' || c == '$'
    part c = start c || (isAlphaNum c && c < '\x80')

reserved :: [Text]
reserved =
  [ "arguments", "await", "break", "case", "catch", "class", "const", "continue"
  , "debugger", "default", "delete", "do", "else", "enum", "eval", "export"
  , "extends", "false", "finally", "for", "function", "if", "implements"
  , "import", "in", "instanceof", "interface", "let", "new", "null", "package"
  , "private", "protected", "public", "return", "static", "super", "switch"
  , "this", "throw", "true", "try", "typeof", "var", "void", "while", "with"
  , "yield"
  ]
