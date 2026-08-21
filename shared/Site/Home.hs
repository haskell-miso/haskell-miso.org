-----------------------------------------------------------------------------
-- | The landing page. The hero logo is its own component so that pointer
-- movement only re-renders that subtree; the counter demo is a real,
-- running miso component mounted with @(+>)@.
module Site.Home
  ( homePage
  ) where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Navigator (copyClipboard)
import           Control.Concurrent (threadDelay)
import qualified Miso.CSS as CSS
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.Lens
import           Miso.String (MisoString, ms)
import qualified Miso.String as MS
-----------------------------------------------------------------------------
import           Site.I18n
import           Site.Icons
import           Site.Logo
import           Site.Prose (Nav (..), navigate)
import           Site.Route
import           Site.Styles (logoTilt, easeOut)
import           Site.Syntax (haskell)
import           Site.Types
-----------------------------------------------------------------------------
homePage :: Component Ctx () () Nav
homePage = (component () navigate view) { useContext = True }
  where
    view ctx () () =
      H.div_ [ P.class_ "home" ]
        [ hero ctx
        , pillars ctx
        , codeSection ctx
        , features ctx
        , aiSection ctx
        , ecosystem ctx
        ]
-----------------------------------------------------------------------------
hero :: Ctx -> View Ctx () Nav
hero ctx =
  H.section_ [ P.class_ "hero" ]
    [ H.div_ [ P.class_ "hero-bg", P.aria_ "hidden" "true" ]
        [ H.div_ [ P.class_ "hero-grid" ] []
        , H.div_ [ P.class_ "hero-sparks" ]
            [ H.span_ [ P.classes_ [ "spark", "spark-h", "spark-" <> ms i ] ] [] | i <- [1 .. 4 :: Int] ]
        , H.div_ [ P.class_ "hero-sparks" ]
            [ H.span_ [ P.classes_ [ "spark", "spark-v", "spark-" <> ms i ] ] [] | i <- [5 .. 8 :: Int] ]
        , H.div_ [ P.classes_ [ "hero-blob", "hero-blob-a" ] ] []
        , H.div_ [ P.classes_ [ "hero-blob", "hero-blob-b" ] ] []
        ]
    , H.div_ [ P.class_ "hero-inner" ]
        [ "hero-logo" +> heroLogo
        , H.p_ [ P.class_ "hero-eyebrow" ] [ "🍜 ", t ctx HeroEyebrow, " 🍜" ]
        , H.h1_ [ P.class_ "hero-title" ] [ t ctx HeroTitle ]
        , H.p_ [ P.class_ "hero-subtitle" ] [ t ctx HeroSubtitle ]
        , H.div_ [ P.class_ "hero-actions" ]
            [ H.a_
                [ P.classes_ [ "btn", "btn-primary" ]
                , P.href_ (routeHref Docs)
                , E.onClickPrevent (Go Docs)
                ]
                [ t ctx HeroGetStarted, H.span_ [ P.class_ "btn-arrow" ] [ "→" ] ]
            , H.a_
                [ P.classes_ [ "btn", "btn-ghost" ]
                , P.href_ "https://github.com/dmjio/miso"
                , P.target_ "_blank", P.rel_ "noopener"
                ]
                [ iconGitHub, t ctx HeroGitHub ]
            ]
        , "hero-terminal" +> heroTerminal
        ]
    ]
-----------------------------------------------------------------------------
-- The getting-started terminal, with a copy-to-clipboard button --------------
-----------------------------------------------------------------------------
installCmds :: [MisoString]
installCmds =
  [ "git clone https://github.com/haskell-miso/miso-sampler && cd miso-sampler"
  , "nix develop .#wasm --command bash -c 'make all && make serve'"
  ]
-----------------------------------------------------------------------------
data TermAction = CopyCmds | Copied | CopyFailed | CopyDone
  deriving (Show, Eq)
-----------------------------------------------------------------------------
heroTerminal :: Component Ctx () Bool TermAction
heroTerminal = component False update view
  where
    update = \case
      CopyCmds   -> copyClipboard (MS.intercalate "\n" installCmds) Copied (const CopyFailed)
      Copied     -> do
        this .= True
        io (threadDelay 1600000 >> pure CopyDone)
      CopyFailed -> pure ()
      CopyDone   -> this .= False

    view _ () copied =
      H.div_ [ P.class_ "hero-install" ]
        [ H.button_
            [ P.classList_ [ ("hero-copy", True), ("copied", copied) ]
            , P.type_ "button", E.onClick CopyCmds
            , P.aria_ "label" "Copy commands", P.title_ "Copy"
            ]
            [ if copied then checkIcon else copyIcon ]
        , H.span_
            [ P.classList_ [ ("copy-toast", True), ("show", copied) ]
            , P.role_ "status"
            ]
            [ checkIcon, "Copied" ]
        , vfrag
            [ H.div_ [ P.class_ "hero-install-line", key_ cmd ]
                [ H.span_ [ P.class_ "hero-install-prompt" ] [ "$" ]
                , H.code_ [] [ text cmd ]
                ]
            | cmd <- installCmds
            ]
        ]
-----------------------------------------------------------------------------
-- The interactive logo -------------------------------------------------------
-----------------------------------------------------------------------------
data LogoModel
  = LogoModel
  { _pointer  :: (Double, Double)   -- normalised -1..1
  , _viewport :: (Int, Int)
  , _moved    :: Bool               -- has the pointer moved yet?
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
pointer :: Lens LogoModel (Double, Double)
pointer = lens _pointer $ \m x -> m { _pointer = x }
-----------------------------------------------------------------------------
viewport :: Lens LogoModel (Int, Int)
viewport = lens _viewport $ \m x -> m { _viewport = x }
-----------------------------------------------------------------------------
moved :: Lens LogoModel Bool
moved = lens _moved $ \m x -> m { _moved = x }
-----------------------------------------------------------------------------
data LogoAction
  = Measure
  | Measured Int Int
  | PointerAt (Double, Double)
  deriving (Show, Eq)
-----------------------------------------------------------------------------
heroLogo :: Component Ctx () LogoModel LogoAction
heroLogo = (component (LogoModel (0, 0) (1, 1) False) update view)
  { mount = Just Measure
  , subs =
      [ windowCoordsSub PointerAt
      , windowSub "resize" emptyDecoder (const Measure)
      ]
  }
  where
    update = \case
      Measure -> io (Measured <$> windowInnerWidth <*> windowInnerHeight)
      Measured w h -> viewport .= (max 1 w, max 1 h)
      PointerAt (x, y) -> do
        (w, h) <- use viewport
        let nx = clamp ((x / fromIntegral w) * 2 - 1)
            ny = clamp ((y / fromIntegral h) * 2 - 1)
        pointer .= (nx, ny)
        moved .= True

    clamp = max (-1) . min 1

    view _ () m =
      let (nx, ny) = m ^. pointer
          -- the glow drifts the opposite way for parallax
          glow =
            [ CSS.transforms [ CSS.translate3d (pxD (negate nx * 24)) (pxD (negate ny * 24)) (CSS.px 0) ]
            , CSS.transition_ "transform" (CSS.ms 520) easeOut
            ]
      in H.div_
          [ P.classList_ [ ("hero-logo", True), ("live", m ^. moved) ] ]
          [ H.div_ [ P.class_ "hero-logo-glow", CSS.style_ glow ] []
          , H.div_ [ P.class_ "hero-logo-float" ]
              [ H.div_ [ P.class_ "hero-logo-tilt", CSS.style_ (logoTilt nx ny) ]
                  [ H.div_ [ P.class_ "hero-logo-ring" ] []
                  , H.div_ [ P.classes_ [ "hero-logo-ring", "hero-logo-ring-b" ] ] []
                  , logoMarkGradient [ P.class_ "hero-logo-mark" ]
                  ]
              ]
          ]

    pxD :: Double -> MisoString
    pxD d = CSS.px (round d)
-----------------------------------------------------------------------------
-- Platform pillars ------------------------------------------------------------
-----------------------------------------------------------------------------
pillars :: Ctx -> View Ctx () Nav
pillars ctx =
  H.section_ [ P.class_ "pillars" ]
    [ pillar "web" iconGlobe PillarWeb [ t ctx PillarWebDesc ] Nothing
        [ P.href_ (routeHref (docsPage "installation")), E.onClickPrevent (Go (docsPage "installation")) ]
    , pillar "mobile" iconNative PillarMobile
        [ t ctx PillarMobileDesc, " "
        , H.a_ [ P.class_ "pillar-inline-link", P.href_ "https://lynxjs.org", P.target_ "_blank", P.rel_ "noopener" ] [ "LynxJS.org" ]
        , t ctx PillarMobileDescEnd, " "
        , H.a_
            [ P.class_ "pillar-inline-link"
            , P.href_ (routeHref (nativePage "overview"))
            , E.onClickPrevent (Go (nativePage "overview"))
            ] [ t ctx PillarMobileSee ]
        , t ctx PillarMobileSeeEnd
        ] Nothing
        [ P.href_ (routeHref (nativePage "overview")), E.onClickPrevent (Go (nativePage "overview")) ]
    , pillar "desktop" iconMonitor PillarDesktop [ t ctx PillarDesktopDesc ] (Just PillarComingSoon)
        [ P.href_ "https://github.com/lynx-family/lynxtron", P.target_ "_blank", P.rel_ "noopener" ]
    ]
  where
    -- The card itself is clickable via a stretched link, so the description
    -- can carry real inline links without nesting anchors.
    pillar cls icon title descViews badge attrs =
      H.article_ [ P.classes_ [ "pillar", "pillar-" <> cls ] ]
        [ H.a_ ( P.class_ "pillar-link" : attrs ) []
        , H.div_ [ P.class_ "pillar-top" ]
            [ H.div_ [ P.class_ "pillar-icon" ] [ icon ]
            , case badge of
                Just key -> H.span_ [ P.class_ "pillar-badge" ] [ t ctx key ]
                Nothing  -> vfrag []
            ]
        , H.h3_ [] [ t ctx title ]
        , H.p_ [] descViews
        , H.span_ [ P.class_ "pillar-more" ] [ "→" ]
        ]
-----------------------------------------------------------------------------
-- Code section ------------------------------------------------------------------
-----------------------------------------------------------------------------
codeSection :: Ctx -> View Ctx () Nav
codeSection ctx =
  H.section_ [ P.class_ "code-section" ]
    [ H.div_ [ P.class_ "section-head" ]
        [ H.h2_ [] [ t ctx CodeTitle, " 🍜" ]
        , H.p_ [] [ t ctx CodeSubtitle ]
        ]
    , H.div_ [ P.class_ "code-split" ]
        [ H.div_ [ P.class_ "code-pane" ]
            [ H.div_ [ P.class_ "code-pane-bar" ]
                [ H.span_ [ P.class_ "dot" ] [], H.span_ [ P.class_ "dot" ] [], H.span_ [ P.class_ "dot" ] []
                , H.span_ [ P.class_ "code-pane-title" ] [ "Main.hs" ]
                ]
            , haskell counterSource
            ]
        , H.div_ [ P.class_ "demo-pane" ]
            [ H.span_ [ P.class_ "demo-badge" ] [ H.span_ [ P.class_ "demo-dot" ] [], t ctx CodeLive ]
            , "counter-demo" +> counterDemo
            ]
        ]
    ]
-----------------------------------------------------------------------------
counterSource :: MisoString
counterSource = """
  import Miso (Component, Effect, View, component, text, startApp, defaultEvents)
  import Miso.Lens (this, (+=), (-=))
  import Miso.String (ms)
  import Miso.Html.Element (div_, button_)
  import Miso.Html.Event (onClick)

  data Action = Add | Subtract

  counter :: Component context props Int Action
  counter = component m u v
    where
      m :: Int
      m = 0

      u :: Action -> Effect context props Int Action
      u = \\case
        Add      -> this += 1
        Subtract -> this -= 1

      v :: context -> props -> Int -> View context Int Action
      v _ _ n = div_ []
        [ button_ [ onClick Subtract ] [ "−" ]
        , text (ms n)
        , button_ [ onClick Add ] [ "+" ]
        ]

  main :: IO ()
  main = startApp defaultEvents counter
  """
-----------------------------------------------------------------------------
data CounterAction = Add | Subtract | ResetCounter
  deriving (Show, Eq)
-----------------------------------------------------------------------------
counterDemo :: Component Ctx () Int CounterAction
counterDemo = component 0 update view
  where
    update = \case
      Add          -> this += 1
      Subtract     -> this -= 1
      ResetCounter -> this .= 0

    view _ () n =
      H.div_ [ P.class_ "counter" ]
        [ H.button_ [ P.class_ "counter-btn", E.onClick Subtract, P.aria_ "label" "decrement" ] [ "−" ]
        , H.span_ [ P.class_ "counter-value", key_ (ms n) ] [ text (ms n) ]
        , H.button_ [ P.class_ "counter-btn", E.onClick Add, P.aria_ "label" "increment" ] [ "+" ]
        , H.button_ [ P.class_ "counter-reset", E.onClick ResetCounter ] [ "reset" ]
        ]
-----------------------------------------------------------------------------
-- Features -------------------------------------------------------------------------
-----------------------------------------------------------------------------
features :: Ctx -> View Ctx () Nav
features ctx =
  H.section_ [ P.class_ "features" ]
    [ H.div_ [ P.class_ "section-head" ]
        [ H.h2_ [] [ t ctx FeaturesTitle ]
        , H.p_ [] [ t ctx FeaturesSubtitle ]
        ]
    , H.div_ [ P.class_ "feature-grid" ]
        [ feature (text "◇") FeatVdom FeatVdomDesc (docsPage "view-dsl")
        , feature (text "⧉") FeatComponents FeatComponentsDesc (docsPage "components")
        , feature (text "⚛") FeatReact FeatReactDesc (docsPage "context")
        , feature (text "</>") FeatDsl FeatDslDesc (docsPage "styles")
        , feature (text "⇶") FeatEvents FeatEventsDesc (docsPage "events")
        , feature (text "⟲") FeatSsr FeatSsrDesc (docsPage "html-and-prerendering")
        , feature (text "⌁") FeatRouter FeatRouterDesc (docsPage "routing")
        , feature (text "∞") FeatEffects FeatEffectsDesc (docsPage "effects")
        , feature (text "⇅") FeatNet FeatNetDesc (docsPage "subscriptions")
        , feature (text "{}") FeatFfi FeatFfiDesc (docsPage "javascript-edsl")
        , feature (text "↻") FeatReload FeatReloadDesc (docsPage "development")
        , feature iconNative FeatNative FeatNativeDesc (nativePage "overview")
        ]
    ]
  where
    feature glyph title desc route =
      H.a_
        [ P.class_ "feature", P.href_ (routeHref route), E.onClickPrevent (Go route) ]
        [ H.span_ [ P.class_ "feature-glyph", P.aria_ "hidden" "true" ] [ glyph ]
        , H.h3_ [] [ t ctx title ]
        , H.p_ [] [ t ctx desc ]
        ]
-----------------------------------------------------------------------------
-- Generative AI ----------------------------------------------------------------------
-----------------------------------------------------------------------------
aiSection :: Ctx -> View Ctx () Nav
aiSection ctx =
  H.section_ [ P.class_ "ai-section" ]
    [ H.div_ [ P.class_ "ai-card" ]
        [ H.span_ [ P.class_ "ai-sparkle", P.aria_ "hidden" "true" ] [ "✦" ]
        , H.h2_ [] [ t ctx AiTitle ]
        , H.p_ [] [ t ctx AiBody ]
        , H.a_
            [ P.class_ "ai-badge"
            , P.href_ "https://www.anthropic.com/claude", P.target_ "_blank", P.rel_ "noopener" ]
            [ "✦ ", t ctx AiBadge ]
        ]
    ]
-----------------------------------------------------------------------------
-- Ecosystem strip --------------------------------------------------------------------
-----------------------------------------------------------------------------
ecosystem :: Ctx -> View Ctx () Nav
ecosystem ctx =
  H.section_ [ P.class_ "ecosystem" ]
    [ H.div_ [ P.class_ "section-head" ]
        [ H.h2_ [] [ t ctx EcoTitle, " 🔋" ]
        , H.p_ [] [ t ctx EcoSubtitle ]
        ]
    , H.div_ [ P.class_ "eco-marquee" ]
        [ H.div_ [ P.class_ "eco-track" ] (map chip (chips ++ chips)) ]
    , H.div_ [ P.class_ "eco-cta" ]
        [ H.a_
            [ P.classes_ [ "btn", "btn-ghost" ], P.href_ (routeHref Examples), E.onClickPrevent (Go Examples) ]
            [ t ctx EcoAll, H.span_ [ P.class_ "btn-arrow" ] [ "→" ] ]
        ]
    ]
  where
    chip (emoji, name) =
      H.a_
        [ P.class_ "eco-chip", P.href_ ("https://github.com/haskell-miso/" <> name)
        , P.target_ "_blank", P.rel_ "noopener" ]
        [ H.span_ [] [ text emoji ], text name ]
    chips =
      [ ("♟️", "chess"), ("🃏", "solitaire"), ("🕹️", "flatris"), ("🔢", "2048"), ("👾", "space-invaders")
      , ("✅", "todo-mvc"), ("🖌️", "canvas2d"), ("🧊", "three-miso"), ("📊", "chartjs"), ("💅", "miso.ui")
      , ("🐈", "miso-lynx"), ("🌐", "router"), ("⚡", "websocket"), ("📷", "camera"), ("🍄", "mario")
      ]
