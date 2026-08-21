-----------------------------------------------------------------------------
-- | The shape of a documentation page. Every page is a value of 'DocPage';
-- "Site.Docs.Content" collects them into the ordered table the sidebar,
-- search and prerenderer all read from.
module Site.Docs.Types
  ( DocPage (..)
  , Group (..)
  , pageRoute
  ) where
-----------------------------------------------------------------------------
import Miso.String (MisoString)
-----------------------------------------------------------------------------
import Site.Prose (Doc)
import Site.Route
-----------------------------------------------------------------------------
-- | Sidebar groups, in display order.
data Group
  = Start
  | Concepts
  | Platform
  | Native
  | Thinking
  deriving (Show, Eq, Ord, Enum, Bounded)
-----------------------------------------------------------------------------
data DocPage
  = DocPage
  { pageSlug     :: MisoString
  -- ^ URL segment, unique within its 'Group' namespace
  , pageGroup    :: Group
  , pageTitle    :: MisoString
  , pageBlurb    :: MisoString
  -- ^ One sentence, used for @\<meta description\>@ and search results
  , pageKeywords :: [MisoString]
  -- ^ Extra search terms
  , pageBody     :: [Doc]
  }
-----------------------------------------------------------------------------
-- | Native and "Thinking in miso" pages live under their own prefix.
pageRoute :: DocPage -> Route
pageRoute DocPage {..} =
  case pageGroup of
    Native   -> nativePage pageSlug
    Thinking -> thinkingPage pageSlug
    _        -> docsPage pageSlug
-----------------------------------------------------------------------------
