-----------------------------------------------------------------------------
{-# LANGUAGE QuasiQuotes #-}
-----------------------------------------------------------------------------
-- | The root component: top bar (persistent across pages), router dispatch
-- and footer. Pages are keyed child components, so navigating swaps them
-- with a proper unmount / mount.
module Site
  ( site
  , Model (..)
  , Action (..)
  , initialModel
  ) where
-----------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Control.Monad (void, when)
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust)
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.CSS as CSS
import           Miso.FFI.QQ (js)
import           Miso.JSON (FromJSON (..), withObject, (.:))
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.Lens
import           Miso.Router (Capture (..))
import           Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
import           Site.Blog
import           Site.Docs
import           Site.Examples
import           Site.Home
import           Site.I18n
import           Site.Icons
import           Site.Logo
import           Site.Route
import           Site.Search
import           Site.Styles
import           Site.Types
-----------------------------------------------------------------------------
data Model
  = Model
  { _uri      :: URI
  , _menuOpen :: Bool     -- mobile nav
  , _langOpen :: Bool     -- language dropdown
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
uri :: Lens Model URI
uri = lens _uri $ \m x -> m { _uri = x }
-----------------------------------------------------------------------------
menuOpen :: Lens Model Bool
menuOpen = lens _menuOpen $ \m x -> m { _menuOpen = x }
-----------------------------------------------------------------------------
langOpen :: Lens Model Bool
langOpen = lens _langOpen $ \m x -> m { _langOpen = x }
-----------------------------------------------------------------------------
initialModel :: URI -> Model
initialModel u = Model u False False
-----------------------------------------------------------------------------
data Action
  = Init
  | Prefs (Maybe Lang) (Maybe Theme)
  | HandleURI URI
  | Go Route
  | ToggleMenu
  | ToggleLang
  | CloseMenus
  | SetLang Lang
  | ToggleTheme
  deriving (Show, Eq)
-----------------------------------------------------------------------------
site :: URI -> Component Ctx () Model Action
site u = (component (initialModel u) updateSite viewSite)
  { subs = [ uriSub HandleURI ]
  , mount = Just Init
  , useContext = True
  , logLevel = DebugHydrate
  }
-----------------------------------------------------------------------------
updateSite :: Action -> Effect Ctx () Model Action
updateSite = \case
  -- On mount, sync the context with what the pre-paint script applied.
  -- The theme is read from the html element's data-theme attribute (not
  -- from localStorage): with no saved choice the script falls back to the
  -- OS prefers-color-scheme, and the context must agree with what is
  -- actually on screen or the first toggle would be a no-op.
  -- A ?lang= query parameter wins over the stored choice, so links can
  -- carry a language: https://haskell-miso.org/docs?lang=zh-Hans
  Init -> do
    u <- use uri
    let urlLang =
          case M.lookup "lang" (uriQueryString u) of
            Just (Just code) -> langFromCode code
            _ -> Nothing
    io $ do
      stored <- (>>= langFromCode) <$> getLocalStorage langStorageKey
      html <- jsg "document" ! "documentElement"
      attr <- fromJSVal =<< (html # "getAttribute" $ [ "data-theme" :: MisoString ])
      pure (Prefs (urlLang <|> stored) (attr >>= themeFromCode))
  Prefs mlang mtheme -> do
    modifyContext $ \ctx -> ctx
      { ctxLang  = maybe (ctxLang ctx) id mlang
      , ctxTheme = maybe (ctxTheme ctx) id mtheme
      }
    case mlang of
      Nothing -> pure ()
      Just l -> io_ $ do
        setLocalStorage langStorageKey (langCode l)
        applyLang l
  HandleURI u -> do
    old <- use uri
    uri .= u
    menuOpen .= False
    langOpen .= False
    -- Hash-only changes (in-page anchor links) keep the browser's own
    -- jump to the target element; resetting to the top would undo it.
    when (uriPath old /= uriPath u) $
      io_ scrollTop
    io_ (trackPage u)
  Go route -> do
    menuOpen .= False
    langOpen .= False
    io_ (pushRoute route)
  ToggleMenu -> do
    menuOpen %= not
    langOpen .= False
  ToggleLang -> langOpen %= not
  CloseMenus -> do
    langOpen .= False
    menuOpen .= False
  SetLang lang -> do
    langOpen .= False
    modifyContext $ \ctx -> ctx { ctxLang = lang }
    io_ $ do
      setLocalStorage langStorageKey (langCode lang)
      applyLang lang
  ToggleTheme -> do
    Ctx {..} <- getContext
    let theme = if ctxTheme == Dark then Light else Dark
    modifyContext $ \ctx -> ctx { ctxTheme = theme }
    io_ $ do
      setLocalStorage themeStorageKey (themeCode theme)
      applyTheme theme
-----------------------------------------------------------------------------
applyLang :: Lang -> IO ()
applyLang lang = do
  html <- jsg "document" ! "documentElement"
  void $ html # "setAttribute" $ ("lang" :: MisoString, langCode lang)
-----------------------------------------------------------------------------
applyTheme :: Theme -> IO ()
applyTheme theme = do
  html <- jsg "document" ! "documentElement"
  void $ html # "setAttribute" $ ("data-theme" :: MisoString, themeCode theme)
-----------------------------------------------------------------------------
-- | @behavior: instant@ bypasses the @scroll-behavior: smooth@ on <html>:
-- a smooth reset is cancelled when the route swap mutates the DOM
-- mid-animation, leaving the page scrolled partway down.
scrollTop :: IO ()
scrollTop = [js| window.scrollTo({ top: 0, left: 0, behavior: "instant" }); |]
-----------------------------------------------------------------------------
-- | Report a client-side navigation to GoatCounter. @__misoTrack@ is
-- defined in the prerendered \<head\> and no-ops when the counter script
-- is absent (or blocked).
trackPage :: URI -> IO ()
trackPage u = void (jsg1 "__misoTrack" (prettyURI u))
-----------------------------------------------------------------------------
viewSite :: Ctx -> () -> Model -> View Ctx Model Action
viewSite ctx () m =
  H.div_
    [ P.classList_ [ ("site", True), ("menu-open", m ^. menuOpen) ], E.onClick CloseMenus ]
    [ topbar ctx m
    , H.main_ [ P.class_ "page-root" ] [ dispatch ctx (routeFromURI (m ^. uri)) ]
    , footer ctx
    ]
-----------------------------------------------------------------------------
-- | Route dispatch. Every page is a keyed component, mounted with (+>).
dispatch :: Ctx -> Maybe Route -> View Ctx Model Action
dispatch ctx = \case
  Just Index -> "home" +> homePage
  Just Examples -> "examples" +> examplesPage
  Just Blog -> "blog" +> blogIndex
  Just (BlogPost (Capture slug)) ->
    case lookupPost slug of
      Just post -> ("post-" <> slug) +> blogPostPage post
      Nothing -> notFound ctx
  Just route@Docs -> docs route
  Just route@DocsPage {} -> docs route
  Just route@DocsNative {} -> docs route
  Just route@DocsThinking {} -> docs route
  Nothing -> notFound ctx
  where
    docs route = mountWithProps_ "docs" (DocsProps route) docsShell
-----------------------------------------------------------------------------
notFound :: Ctx -> View Ctx Model Action
notFound ctx =
  H.section_ [ P.class_ "not-found page" ]
    [ H.p_ [ P.class_ "not-found-code" ] [ "404" ]
    , H.h1_ [] [ t ctx NotFoundTitle ]
    , H.p_ [ P.class_ "lead" ] [ t ctx NotFoundBody ]
    , H.a_ [ P.classes_ [ "btn", "btn-primary" ], P.href_ "/", E.onClickPrevent (Go Index) ]
        [ t ctx NotFoundHome ]
    ]
-----------------------------------------------------------------------------
topbar :: Ctx -> Model -> View Ctx Model Action
topbar ctx m =
  H.header_ [ P.class_ "topbar" ]
    [ H.div_ [ P.class_ "topbar-inner" ]
        [ H.div_ [ P.class_ "brand-cluster" ]
            [ H.a_
                [ P.class_ "brand", P.href_ "/", E.onClickPrevent (Go Index), P.aria_ "label" "miso home" ]
                [ wordmark ]
            , "topbar-version" +> topbarVersion
            ]
        , H.nav_ [ P.class_ "topnav", P.aria_ "label" "Primary" ]
            [ navLink Docs NavDocs (isDocs current)
            , navLink Examples NavExamples (current == Just Examples)
            , navLink Blog NavBlog (isBlog current)
            , H.a_
                [ P.classes_ [ "topnav-link", "topnav-external" ]
                , P.href_ "https://ui.haskell-miso.org", P.target_ "_blank", P.rel_ "noopener" ]
                [ t ctx NavUI, iconExternal ]
            , H.a_
                [ P.classes_ [ "topnav-link", "topnav-external" ]
                , P.href_ "https://try.haskell-miso.org", P.target_ "_blank", P.rel_ "noopener" ]
                [ t ctx NavTry, iconExternal ]
            , H.a_
                [ P.classes_ [ "topnav-link", "topnav-external" ]
                , P.href_ "https://legacy.haskell-miso.org", P.target_ "_blank", P.rel_ "noopener" ]
                [ t ctx NavLegacy, iconExternal ]
            ]
        , H.div_ [ P.class_ "topbar-tools" ]
            [ "search" +> searchPalette
            , langMenu
            , H.button_
                [ P.class_ "tool-btn", P.type_ "button", E.onClick ToggleTheme
                , P.aria_ "label" (translate ctx NavTheme), P.title_ (translate ctx NavTheme) ]
                [ H.span_ [ P.class_ "theme-sun" ] [ iconSun ]
                , H.span_ [ P.class_ "theme-moon" ] [ iconMoon ]
                ]
            , H.a_
                [ P.classes_ [ "tool-btn", "tool-gh" ], P.href_ "https://github.com/haskell-miso"
                , P.target_ "_blank", P.rel_ "noopener", P.aria_ "label" "GitHub", P.title_ "GitHub" ]
                [ iconGitHub ]
            , H.a_
                [ P.classes_ [ "tool-btn", "tool-discord" ], P.href_ "https://discord.gg/QVDtfYNSxq"
                , P.target_ "_blank", P.rel_ "noopener", P.aria_ "label" "Discord", P.title_ "Discord" ]
                [ iconDiscord ]
            , H.a_
                [ P.classes_ [ "tool-btn", "tool-x" ], P.href_ "https://x.com/haskell_miso"
                , P.target_ "_blank", P.rel_ "noopener", P.aria_ "label" "X", P.title_ "X" ]
                [ iconX ]
            , H.button_
                [ P.classes_ [ "tool-btn", "menu-toggle" ], P.type_ "button"
                , E.onClickWithOptions stopPropagation ToggleMenu
                , P.aria_ "label" (translate ctx NavMenu), P.aria_ "expanded" (if m ^. menuOpen then "true" else "false") ]
                [ if m ^. menuOpen then iconClose else iconMenu ]
            ]
        ]
    ]
  where
    current = routeFromURI (m ^. uri)

    navLink route key active =
      H.a_
        [ P.classList_ [ ("topnav-link", True), ("active", active) ]
        , P.href_ (routeHref route)
        , E.onClickPrevent (Go route)
        ]
        [ t ctx key ]

    langMenu =
      H.div_ [ P.classList_ [ ("lang-menu", True), ("open", m ^. langOpen) ] ]
        [ H.button_
            [ P.classes_ [ "tool-btn", "lang-btn" ], P.type_ "button"
            , E.onClickWithOptions stopPropagation ToggleLang
            , P.aria_ "label" (translate ctx NavLanguage), P.aria_ "haspopup" "listbox"
            , P.aria_ "expanded" (if m ^. langOpen then "true" else "false")
            ]
            [ iconLanguage
            , H.span_ [ P.class_ "lang-code" ] [ text (shortCode (ctxLang ctx)) ]
            , iconChevron
            ]
          -- full-viewport click-catcher: page clicks land in child
          -- components, whose events never reach this component's handlers
        , H.div_ [ P.class_ "lang-scrim", E.onClick CloseMenus ] []
        , H.ul_
            [ P.class_ "lang-list", P.role_ "listbox"
              -- open / close with a typed transform + transition from Miso.CSS
            , CSS.style_
                [ CSS.opacity (if m ^. langOpen then 1 else 0)
                , CSS.transforms
                    (if m ^. langOpen
                       then [ CSS.translateY (CSS.px 0), CSS.scale 1 ]
                       else [ CSS.translateY (CSS.px (-6)), CSS.scale 0.98 ])
                , CSS.transition ("opacity 160ms " <> easeOut <> ", transform 160ms " <> easeOut)
                , CSS.pointerEvents (if m ^. langOpen then "auto" else "none")
                ]
            ]
            [ H.li_ [ key_ (langCode l) ]
                [ H.button_
                    [ P.classList_ [ ("lang-option", True), ("active", l == ctxLang ctx) ]
                    , P.type_ "button", P.role_ "option"
                    , E.onClickWithOptions stopPropagation (SetLang l)
                    ]
                    [ H.span_ [ P.class_ "lang-name" ] [ text (langName l) ]
                    , H.span_ [ P.class_ "lang-tag" ] [ text (langCode l) ]
                    ]
                ]
            | l <- allLangs
            ]
        ]

    shortCode l = ms (show l)

    isDocs = \case
      Just Docs -> True
      Just DocsPage {} -> True
      Just DocsNative {} -> True
      Just DocsThinking {} -> True
      _ -> False

    isBlog = \case
      Just Blog -> True
      Just BlogPost {} -> True
      _ -> False
-----------------------------------------------------------------------------
-- The latest miso release, fetched from GitHub on mount ----------------------
-----------------------------------------------------------------------------
newtype Release = Release MisoString
-----------------------------------------------------------------------------
instance FromJSON Release where
  parseJSON = withObject "release" $ \o -> Release <$> o .: "tag_name"
-----------------------------------------------------------------------------
data VersionAction
  = FetchVersion
  | GotVersion MisoString
  | VersionFailed
-----------------------------------------------------------------------------
topbarVersion :: Component Ctx () (Maybe MisoString) VersionAction
topbarVersion = (component Nothing update view) { mount = Just FetchVersion }
  where
    update = \case
      FetchVersion ->
        getJSON "https://api.github.com/repos/dmjio/miso/releases/latest" []
          (\r -> case body r of Release v -> GotVersion v)
          (\(_ :: Response MisoString) -> VersionFailed)
      GotVersion v  -> this .= Just v
      VersionFailed -> pure ()

    -- rendered (invisibly) even before the tag arrives; fades in next to
    -- the wordmark and stays hidden if the request fails
    view _ () version =
      H.a_
        [ P.classList_ [ ("topbar-version", True), ("show", isJust version) ]
        , P.href_ "https://github.com/dmjio/miso/releases/latest"
        , P.target_ "_blank", P.rel_ "noopener"
        , P.title_ "Latest release", P.aria_ "label" "Latest miso release"
        ]
        [ text (maybe "" ("v" <>) version) ]
-----------------------------------------------------------------------------
footer :: Ctx -> View Ctx Model Action
footer ctx =
  H.footer_ [ P.class_ "footer" ]
    [ H.div_ [ P.class_ "footer-inner" ]
        [ H.div_ [ P.class_ "footer-brand" ]
            [ wordmark
            , H.p_ [] [ t ctx FooterTagline ]
            , H.p_ [ P.class_ "footer-license" ]
                [ t ctx FooterLicense, " "
                , H.a_ [ P.href_ "https://github.com/dmjio", P.target_ "_blank", P.rel_ "noopener" ] [ "@dmjio" ]
                ]
            ]
        , column FooterDocs
            [ internal (docsPage "introduction") "Introduction"
            , internal (docsPage "your-first-component") "Your first Component"
            , internal (thinkingPage "overview") "Thinking in miso"
            , internal (nativePage "overview") "Native (Lynx)"
            , external "https://haddocks.haskell-miso.org/miso/Miso.html" "Haddocks"
            ]
        , column FooterCommunity
            [ external "https://github.com/dmjio/miso" "GitHub"
            , external "https://github.com/haskell-miso" "haskell-miso org"
            , external "https://discord.gg/QVDtfYNSxq" "Discord"
            , external "https://matrix.to/#/#haskell-miso:matrix.org" "Matrix"
            ]
        , column FooterMore
            [ internal Examples "Examples"
            , internal Blog "Blog"
            , external "https://ui.haskell-miso.org" "miso.ui"
            , external "https://lynxjs.haskell-miso.org" "miso-lynx"
            , external "https://try.haskell-miso.org" "Try miso"
            ]
        ]
    ]
  where
    column key links =
      H.div_ [ P.class_ "footer-col" ]
        [ H.h4_ [] [ t ctx key ]
        , H.ul_ [] [ H.li_ [] [ l ] | l <- links ]
        ]
    internal route label =
      H.a_ [ P.href_ (routeHref route), E.onClickPrevent (Go route) ] [ text label ]
    external href label =
      H.a_ [ P.href_ href, P.target_ "_blank", P.rel_ "noopener" ] [ text label ]
-----------------------------------------------------------------------------
