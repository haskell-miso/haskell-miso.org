-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- | Client entry point (WASM / JavaScript). Hydrates whichever prerendered
-- page was loaded, seeding the app-global context with the translation
-- table, then takes over navigation.
module Main (main) where
-----------------------------------------------------------------------------
import Miso
import qualified Data.Map.Strict as M
-----------------------------------------------------------------------------
import Site (site)
import Site.I18n (catalog)
import Site.Types (mkCtx)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
-- | @scroll@ does not bubble, so it is delegated in the capture phase
-- (used by the docs sidebar to remember its scroll offset).
scrollEvents :: Events
scrollEvents = M.singleton "scroll" CAPTURE
-----------------------------------------------------------------------------
main :: IO ()
main = misoWithContext (defaultEvents <> keyboardEvents <> pointerEvents <> scrollEvents) (mkCtx catalog) site
-----------------------------------------------------------------------------
