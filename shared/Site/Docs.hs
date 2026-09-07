-----------------------------------------------------------------------------
-- | The documentation shell: a sidebar plus the current page, which is
-- mounted as its own keyed 'Component' with @(+>)@. The shell receives the
-- current route as props from the root component, so it re-renders (and the
-- keyed page swaps) whenever navigation happens.
module Site.Docs
  ( DocsProps (..)
  , docsShell
  , docPageComponent
  ) where
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.JSON (withObject, (.:))
import           Miso.Lens
import           Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
import           Site.Docs.Content
import           Site.Docs.Types
import           Site.I18n
import           Site.Icons
import           Site.Prose (Nav (..), navigate)
import           Site.Route
import           Site.Types
-----------------------------------------------------------------------------
-- | The route currently being shown, supplied by the root as props.
newtype DocsProps = DocsProps Route
  deriving (Show, Eq)
-----------------------------------------------------------------------------
newtype DocsModel = DocsModel { _sidebarOpen :: Bool }
  deriving (Show, Eq)
-----------------------------------------------------------------------------
sidebarOpen :: Lens DocsModel Bool
sidebarOpen = lens _sidebarOpen $ \m b -> m { _sidebarOpen = b }
-----------------------------------------------------------------------------
data DocsAction
  = ToggleSidebar
  | CloseSidebar
  | DocsGo Route
  | SidebarScrolled Double
  | RestoreSidebarScroll
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | The sidebar's scroll offset is kept in sessionStorage so it survives a
-- page refresh (and re-entering the docs section) within the same tab.
sidebarScrollKey :: MisoString
sidebarScrollKey = "miso.docs.sidebar-scroll"
-----------------------------------------------------------------------------
scrollTopDecoder :: Decoder Double
scrollTopDecoder = Decoder
  { decodeAt = DecodeTarget ["target"]
  , decoder  = withObject "scroll" $ \o -> o .: "scrollTop"
  }
-----------------------------------------------------------------------------
docsShell :: Component Ctx DocsProps DocsModel DocsAction
docsShell = (component (DocsModel False) update view)
  { useContext = True
  , onPropsChanged = Just (\_ _ -> CloseSidebar)
  , mount = Just RestoreSidebarScroll
  }
  where
    update = \case
      ToggleSidebar -> sidebarOpen %= not
      CloseSidebar  -> sidebarOpen .= False
      DocsGo r      -> do
        sidebarOpen .= False
        io_ (pushRouteHref r)
      SidebarScrolled top ->
        io_ (setSessionStorage sidebarScrollKey (ms top))
      RestoreSidebarScroll -> io_ $ do
        saved <- getSessionStorage sidebarScrollKey
        case saved of
          Nothing  -> pure ()
          Just top -> do
            el <- getElementById "docs-sidebar"
            -- assigning the stored string coerces to a number in JS
            setField el "scrollTop" top

    view ctx (DocsProps route) m =
      let current = lookupRoute route
      in H.div_ [ P.class_ "docs" ]
          [ H.aside_
              [ P.id_ "docs-sidebar"
              , P.classList_ [ ("docs-sidebar", True), ("open", m ^. sidebarOpen) ]
              , on "scroll" scrollTopDecoder (\top _ _ -> SidebarScrolled top)
              ]
              [ H.nav_ [ P.class_ "docs-nav", P.aria_ "label" "Documentation" ]
                  (concatMap (sidebarGroup ctx current) groups)
              , H.a_
                  [ P.class_ "docs-nav-external"
                  , P.href_ "https://haddocks.haskell-miso.org/miso/Miso.html"
                  , P.target_ "_blank"
                  , P.rel_ "noopener"
                  ]
                  [ t ctx DocsHaddocks, " ↗" ]
              ]
          , H.div_
              [ P.classList_ [ ("docs-scrim", True), ("open", m ^. sidebarOpen) ]
              , E.onClick CloseSidebar
              ] []
          , H.div_ [ P.class_ "docs-main" ]
              [ H.button_
                  [ P.class_ "docs-sidebar-toggle", E.onClick ToggleSidebar, P.type_ "button" ]
                  [ iconMenu, t ctx DocsSidebarToggle ]
              , case current of
                  Just page -> pageKey page +> docPageComponent page
                  Nothing   -> "docs-missing" +> docPageComponent firstPage
              ]
          ]

    sidebarGroup ctx current g =
      [ H.h4_ [ P.class_ "docs-nav-group" ] [ t ctx (groupKey g) ]
      , H.ul_ []
          [ H.li_ []
              [ H.a_
                  [ P.href_ (routeHref (pageRoute p))
                  , P.classList_ [ ("active", isCurrent current p) ]
                  , E.onClickPrevent (DocsGo (pageRoute p))
                  ]
                  [ text (pageTitle p) ]
              ]
          | p <- groupPages g
          ]
      ]

    isCurrent (Just cur) p = pageGroup cur == pageGroup p && pageSlug cur == pageSlug p
    isCurrent Nothing _ = False
-----------------------------------------------------------------------------
pageKey :: DocPage -> MisoString
pageKey p = "doc-" <> ms (show (pageGroup p)) <> "-" <> pageSlug p
-----------------------------------------------------------------------------
-- | One documentation page as a component of its own.
docPageComponent :: DocPage -> Component Ctx () () Nav
docPageComponent page = (component () navigate view) { useContext = True }
  where
    (prev, next) = neighbours page
    view ctx () () =
      H.article_ [ P.class_ "doc" ]
        [ H.header_ [ P.class_ "doc-header" ]
            [ H.p_ [ P.class_ "doc-eyebrow" ] [ t ctx (groupKey (pageGroup page)) ]
            , H.h1_ [] [ text (pageTitle page) ]
            , if ctxLang ctx == EN
                then vfrag []
                else H.p_ [ P.class_ "doc-lang-note" ] [ t ctx DocsEnglishOnly ]
            ]
        , H.div_ [ P.class_ "doc-body" ] (pageBody page)
        , H.nav_ [ P.class_ "doc-pager", P.aria_ "label" "Pagination" ]
            [ pagerLink ctx "prev" DocsPrev prev
            , pagerLink ctx "next" DocsNext next
            ]
        ]

    pagerLink _ cls _ Nothing = H.span_ [ P.classes_ [ "doc-pager-link", "empty", cls ] ] []
    pagerLink ctx cls label (Just p) =
      H.a_
        [ P.classes_ [ "doc-pager-link", cls ]
        , P.href_ (routeHref (pageRoute p))
        , E.onClickPrevent (Go (pageRoute p))
        ]
        [ H.span_ [ P.class_ "doc-pager-label" ] [ t ctx label ]
        , H.span_ [ P.class_ "doc-pager-title" ] [ text (pageTitle p) ]
        ]
-----------------------------------------------------------------------------
