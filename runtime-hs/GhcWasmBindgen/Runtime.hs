{-# LANGUAGE UndecidableInstances #-}

-- | Imported by the generated Haskell bindings. ADTs cross as JSON encoded with
-- 'options'; EmitJavaScript mirrors these options.
module GhcWasmBindgen.Runtime
  ( Bindgen (..)
  , Generic
  , ToJSON
  , FromJSON
  , ToJSONKey
  , FromJSONKey
  , JSString (..)
  , fromJSString
  , toJSString
  , fromJSText
  , toJSText
  , fromJS
  , toJS
  , Fallback
  , guarded
  ) where

import Control.Exception (SomeException, catch, displayException, evaluate)
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic, Rep)
import GHC.Wasm.Prim (JSString (..), fromJSString, toJSString)

-- | The tag key is @$tag@: it cannot be a Haskell field name, so it never
-- clashes with a record field.
options :: Options
options = defaultOptions {sumEncoding = TaggedObject "$tag" "contents", tagSingleConstructors = True}

-- | @deriving via Bindgen T instance ToJSON T@ gives @T@ instances using 'options'.
newtype Bindgen a = Bindgen a

instance (Generic a, GToJSON' Value Zero (Rep a), GToJSON' Encoding Zero (Rep a)) => ToJSON (Bindgen a) where
  toJSON (Bindgen a) = genericToJSON options a
  toEncoding (Bindgen a) = genericToEncoding options a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (Bindgen a) where
  parseJSON = fmap Bindgen . genericParseJSON options

toJS :: ToJSON a => a -> JSString
toJS = toJSString . TL.unpack . encodeToLazyText

fromJS :: FromJSON a => JSString -> a
fromJS = either errorWithoutStackTrace id . eitherDecodeStrictText . fromJSText

fromJSText :: JSString -> Text
fromJSText = T.pack . fromJSString

toJSText :: Text -> JSString
toJSText = toJSString . T.unpack

-- | An uncaught exception in a sync export kills the RTS, so every export
-- runs through here. The message goes to a slot that
-- ghc_wasm_bindgen_internal.js checks after each call, and JS gets a
-- placeholder it discards. Results are primitives or JSStrings, so WHNF
-- forces everything.
guarded :: Fallback a => a -> IO a
guarded x = evaluate x `catch` \(e :: SomeException) -> fallback <$ setError (toJSString (displayException e))

foreign import javascript unsafe "__exports.__ghcWasmBindgenError = $1"
  setError :: JSString -> IO ()

class Fallback a where
  fallback :: a

instance Fallback Double where fallback = 0
instance Fallback Int where fallback = 0
instance Fallback Bool where fallback = False
instance Fallback JSString where fallback = toJSString ""
