{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- | Client-side routes for the whole site, derived generically with
-- "Miso.Router". Every page and every documentation sub-page is a URL.
--
-- Constructor order matters: the generic parser is first-match, so the
-- more specific @\/docs\/native\/:slug@ and @\/docs\/thinking\/:slug@ routes
-- come before @\/docs\/:slug@, which comes before the bare @\/docs@.
module Site.Route
  ( Route (..)
  , routeFromURI
  , docsPage
  , nativePage
  , thinkingPage
  , blogPost
  , routeHref
  , routeURI
  , pushRouteHref
  , allStaticRoutes
  ) where
-----------------------------------------------------------------------------
import GHC.Generics (Generic)
-----------------------------------------------------------------------------
import Miso.Router
#ifdef __MHS__
import Miso.Util.Parser (satisfy)
#endif
import Miso.Subscription.History (pushURI)
import Miso.String (MisoString)
import qualified Miso.String as MS
-----------------------------------------------------------------------------
data Route
  = DocsNative (Path "native") (Capture "slug" MisoString)
  -- ^ @\/docs\/native\/:slug@
  | DocsThinking (Path "thinking") (Capture "slug" MisoString)
  -- ^ @\/docs\/thinking\/:slug@
  | DocsPage (Capture "slug" MisoString)
  -- ^ @\/docs\/:slug@
  | Docs
  -- ^ @\/docs@
  | BlogPost (Capture "slug" MisoString)
  -- ^ @\/blog\/:slug@
  | Blog
  -- ^ @\/blog@
  | Examples
  -- ^ @\/examples@
  | Index
  -- ^ @\/@
  deriving stock (Show, Eq, Generic)
#ifndef __MHS__
  deriving anyclass Router
#endif
-----------------------------------------------------------------------------
#ifdef __MHS__
-- MicroHs has no GHC.Generics metadata, so the instance that @deriving
-- anyclass Router@ produces is written out: the constructor name, lowercased
-- and cut at its first upper-case letter, is the first path segment
-- (@docs@, @blog@, ...), and 'Index' is the root.
instance Router Route where
  fromRoute r = case r of
    DocsNative (Path p) (Capture s)   -> [toPath "docs", toPath p, toCapture s]
    DocsThinking (Path p) (Capture s) -> [toPath "docs", toPath p, toCapture s]
    DocsPage (Capture s)              -> [toPath "docs", toCapture s]
    Docs                              -> [toPath "docs"]
    BlogPost (Capture s)              -> [toPath "blog", toCapture s]
    Blog                              -> [toPath "blog"]
    Examples                          -> [toPath "examples"]
    Index                             -> [IndexToken]
  routeParser = routes
    [ DocsNative <$ path "docs" <*> (Path <$> path "native") <*> (Capture <$> capture)
    , DocsThinking <$ path "docs" <*> (Path <$> path "thinking") <*> (Capture <$> capture)
    , DocsPage <$ path "docs" <*> (Capture <$> capture)
    , Docs <$ path "docs"
    , BlogPost <$ path "blog" <*> (Capture <$> capture)
    , Blog <$ path "blog"
    , Examples <$ path "examples"
    , Index <$ satisfy (== IndexToken)
    ]
#endif
-----------------------------------------------------------------------------
-- | Parse a 'URI' into a 'Route'. Unknown routes are 'Nothing' (rendered as
-- the 404 page).
--
-- Static hosts serve @\/docs\/components\/@ (trailing slash) for
-- @docs\/components\/index.html@, so trailing slashes are stripped before
-- parsing.
routeFromURI :: URI -> Maybe Route
routeFromURI uri =
  case route (normalize uri) of
    Right r -> Just r
    Left _  -> Nothing
  where
    normalize u = u { uriPath = dropSlashes (uriPath u) }
    dropSlashes p
      | Just p' <- MS.stripSuffix "/" p = dropSlashes p'
      | otherwise = p
-----------------------------------------------------------------------------
docsPage :: MisoString -> Route
docsPage = DocsPage . Capture
-----------------------------------------------------------------------------
nativePage :: MisoString -> Route
nativePage = DocsNative (Path "native") . Capture
-----------------------------------------------------------------------------
thinkingPage :: MisoString -> Route
thinkingPage = DocsThinking (Path "thinking") . Capture
-----------------------------------------------------------------------------
blogPost :: MisoString -> Route
blogPost = BlogPost . Capture
-----------------------------------------------------------------------------
-- | The URL for a 'Route' (e.g. @\/docs\/components\/@).
--
-- Always with a trailing slash: the site is prerendered to
-- @<path>\/index.html@ and static hosts (GitHub Pages) 301 the unslashed
-- form to the slashed one. Emitting the slashed form everywhere means no
-- redirect on hard loads, one URL per page in analytics, and links that
-- match the page's canonical.
routeHref :: Route -> MisoString
routeHref = prettyURI . routeURI
-----------------------------------------------------------------------------
-- | 'toURI' with the trailing slash added (see 'routeHref').
routeURI :: Route -> URI
routeURI r = u { uriPath = slashed (uriPath u) }
  where
    u = toURI r
    -- The root's path is empty; 'prettyURI' renders it as @/@ already.
    slashed p
      | MS.null p = p
      | "/" `MS.isSuffixOf` p = p
      | otherwise = p <> "/"
-----------------------------------------------------------------------------
-- | Client-side navigation to a 'Route', pushing the slashed URL (unlike
-- miso's 'Miso.Subscription.History.pushRoute', which uses 'toURI').
pushRouteHref :: Route -> IO ()
pushRouteHref = pushURI . routeURI
-----------------------------------------------------------------------------
-- | Routes that are not parameterised by content (used by the prerenderer
-- together with the docs / blog tables).
allStaticRoutes :: [Route]
allStaticRoutes = [ Index, Docs, Blog, Examples ]
-----------------------------------------------------------------------------
