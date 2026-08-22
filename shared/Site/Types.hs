-----------------------------------------------------------------------------
-- | Types shared by every page of the site.
--
-- The app-global @context@ ('Ctx') carries the state every component on
-- every page cares about: the active language, the translation table and
-- the colour theme. They are changed from the top bar; every page component
-- opts in with @useContext = True@ so it re-renders when they change.
module Site.Types
  ( -- * Context
    Ctx (..)
  , Catalog
  , mkCtx
    -- * Language
  , Lang (..)
  , allLangs
  , langCode
  , langName
  , langFromCode
    -- * Theme
  , Theme (..)
  , themeCode
  , themeFromCode
    -- * Storage keys
  , langStorageKey
  , themeStorageKey
  ) where
-----------------------------------------------------------------------------
import Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | A translation table: for every language, a row of @(key, string)@.
--
-- Keys are strings so the table is a plain lookup table; "Site.I18n" builds
-- it from a closed ADT so GHC still checks that every language has every
-- string.
type Catalog = [ (Lang, [ (MisoString, MisoString) ]) ]
-----------------------------------------------------------------------------
-- | The app-global context. Read via 'Miso.Effect.getContext' and the first
-- argument of every @view@, mutated via 'Miso.Effect.modifyContext'.
-- Seeded on the client by 'Miso.misoWithContext', on the server by
-- 'Miso.setContext'.
data Ctx
  = Ctx
  { ctxLang    :: Lang
  -- ^ Active language (top-bar dropdown, persisted in localStorage)
  , ctxCatalog :: Catalog
  -- ^ Translation table used by 'Site.I18n.t' to render text nodes
  , ctxTheme   :: Theme
  -- ^ Light \/ dark (persisted in localStorage)
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
mkCtx :: Catalog -> Ctx
mkCtx catalog = Ctx EN catalog Light
-----------------------------------------------------------------------------
-- | Languages available from the top-bar dropdown.
data Lang
  = EN
  deriving (Show, Eq, Ord, Enum, Bounded)
-----------------------------------------------------------------------------
allLangs :: [Lang]
allLangs = [minBound .. maxBound]
-----------------------------------------------------------------------------
-- | BCP-47 tag, used for @\<html lang\>@ and localStorage.
langCode :: Lang -> MisoString
langCode = \case
  EN -> "en"
-----------------------------------------------------------------------------
-- | Endonym shown in the dropdown.
langName :: Lang -> MisoString
langName = \case
  EN -> "English"
-----------------------------------------------------------------------------
langFromCode :: MisoString -> Maybe Lang
langFromCode code = lookup code [ (langCode l, l) | l <- allLangs ]
-----------------------------------------------------------------------------
data Theme
  = Light
  | Dark
  deriving (Show, Eq)
-----------------------------------------------------------------------------
themeCode :: Theme -> MisoString
themeCode Light = "light"
themeCode Dark  = "dark"
-----------------------------------------------------------------------------
themeFromCode :: MisoString -> Maybe Theme
themeFromCode "light" = Just Light
themeFromCode "dark"  = Just Dark
themeFromCode _       = Nothing
-----------------------------------------------------------------------------
langStorageKey, themeStorageKey :: MisoString
langStorageKey  = "miso.lang"
themeStorageKey = "miso.theme"
-----------------------------------------------------------------------------
