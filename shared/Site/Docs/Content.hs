-----------------------------------------------------------------------------
-- | The documentation table: every page in sidebar order, grouped, plus
-- lookups used by the docs shell, the search palette and the prerenderer.
module Site.Docs.Content
  ( allPages
  , groups
  , groupPages
  , groupKey
  , lookupRoute
  , neighbours
  , firstPage
  ) where
-----------------------------------------------------------------------------
import Miso.Router (Capture (..))
-----------------------------------------------------------------------------
import Site.Docs.Native
import Site.Docs.Thinking
import Site.Docs.Types
import Site.Docs.Web
import Site.I18n (Key (..))
import Site.Route
-----------------------------------------------------------------------------
-- | Every documentation page, in reading order.
allPages :: [DocPage]
allPages = concatMap groupPages groups
-----------------------------------------------------------------------------
groups :: [Group]
groups = [minBound .. maxBound]
-----------------------------------------------------------------------------
groupPages :: Group -> [DocPage]
groupPages = \case
  Start    -> startPages
  Concepts -> conceptPages
  Platform -> platformPages
  Native   -> nativePages
  Thinking -> thinkingPages
-----------------------------------------------------------------------------
-- | Translation key for a group's sidebar heading.
groupKey :: Group -> Key
groupKey = \case
  Start    -> DocsGroupStart
  Concepts -> DocsGroupConcepts
  Platform -> DocsGroupPlatform
  Native   -> DocsGroupNative
  Thinking -> DocsGroupThinking
-----------------------------------------------------------------------------
-- | The page a docs route refers to.
lookupRoute :: Route -> Maybe DocPage
lookupRoute = \case
  Docs -> Just firstPage
  DocsPage (Capture slug) ->
    find' (\p -> pageGroup p `notElem` [Native, Thinking] && pageSlug p == slug)
  DocsNative _ (Capture slug) ->
    find' (\p -> pageGroup p == Native && pageSlug p == slug)
  DocsThinking _ (Capture slug) ->
    find' (\p -> pageGroup p == Thinking && pageSlug p == slug)
  _ -> Nothing
  where
    find' f =
      case filter f allPages of
        (p:_) -> Just p
        []    -> Nothing
-----------------------------------------------------------------------------
firstPage :: DocPage
firstPage =
  case allPages of
    (p:_) -> p
    []    -> error "no documentation pages"
-----------------------------------------------------------------------------
-- | Previous and next page in reading order.
neighbours :: DocPage -> (Maybe DocPage, Maybe DocPage)
neighbours page = go Nothing allPages
  where
    go prev (p:rest)
      | same p page = (prev, case rest of { (n:_) -> Just n; [] -> Nothing })
      | otherwise = go (Just p) rest
    go _ [] = (Nothing, Nothing)
    same p q = pageGroup p == pageGroup q && pageSlug p == pageSlug q
-----------------------------------------------------------------------------
