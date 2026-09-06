-----------------------------------------------------------------------------
-- | The landing page. The hero logo is its own component so that pointer
-- movement only re-renders that subtree; the counter demo is a real,
-- running miso component mounted with @(+>)@.
module Site.Home
  ( homePage
  ) where
-----------------------------------------------------------------------------
import           Miso
import           Miso.JSON (FromJSON (..), withObject, (.:))
import           Miso.Navigator (copyClipboard)
import           Control.Concurrent (threadDelay)
import           Data.Maybe (isJust)
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
        , tweetSection ctx
        , codeSection ctx
        , features ctx
        , timeline ctx
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
        , "hero-stars" +> heroStars
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
-- The GitHub star count, fetched client-side on mount ------------------------
-----------------------------------------------------------------------------
newtype Stars = Stars Int
-----------------------------------------------------------------------------
instance FromJSON Stars where
  parseJSON = withObject "repo" $ \o -> Stars <$> o .: "stargazers_count"
-----------------------------------------------------------------------------
data StarsAction
  = FetchStars
  | GotStars Int
  | StarsFailed
-----------------------------------------------------------------------------
heroStars :: Component Ctx () (Maybe Int) StarsAction
heroStars = (component Nothing update view) { mount = Just FetchStars }
  where
    update = \case
      FetchStars ->
        getJSON "https://api.github.com/repos/dmjio/miso" []
          (\r -> case body r of Stars n -> GotStars n)
          (\(_ :: Response MisoString) -> StarsFailed)
      GotStars n  -> this .= Just n
      StarsFailed -> pure ()

    -- rendered (invisibly) even before the count arrives, so the hero
    -- doesn't reflow when the badge fades in
    view _ () stars =
      H.a_
        [ P.classList_ [ ("hero-stars", True), ("show", isJust stars) ]
        , P.href_ "https://github.com/dmjio/miso"
        , P.target_ "_blank", P.rel_ "noopener"
        , P.aria_ "label" "Star miso on GitHub"
        ]
        [ iconStar
        , H.span_ [ P.class_ "hero-stars-count" ] [ text (maybe "" formatStars stars) ]
        ]

    -- 5843 -> "5.8k"
    formatStars :: Int -> MisoString
    formatStars n
      | n < 1000  = ms n
      | tenths == 0 = ms (n `div` 1000) <> "k"
      | otherwise   = ms (n `div` 1000) <> "." <> ms tenths <> "k"
      where
        tenths = (n `mod` 1000) `div` 100
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
    [ pillarCard [ "pillar-web" ] iconGlobe (t ctx PillarWeb) [ t ctx PillarWebDesc ] Nothing $
        stretchedLink [ P.href_ (routeHref (docsPage "installation")), E.onClickPrevent (Go (docsPage "installation")) ]
    , pillarCard [ "pillar-mobile" ] iconNative (t ctx PillarMobile)
        [ t ctx PillarMobileDesc, " "
        , H.a_ [ P.class_ "pillar-inline-link", P.href_ "https://lynxjs.org", P.target_ "_blank", P.rel_ "noopener" ] [ "LynxJS.org" ]
        , t ctx PillarMobileDescEnd, " "
        , H.a_
            [ P.class_ "pillar-inline-link"
            , P.href_ (routeHref (nativePage "overview"))
            , E.onClickPrevent (Go (nativePage "overview"))
            ] [ t ctx PillarMobileSee ]
        , t ctx PillarMobileSeeEnd
        ] Nothing $
        stretchedLink [ P.href_ (routeHref (nativePage "overview")), E.onClickPrevent (Go (nativePage "overview")) ]
    , "desktop-pillar" +> desktopPillar
    ]
-----------------------------------------------------------------------------
-- | A platform card. The card is clicked through a stretched overlay element
-- (the last argument), so the description can carry real inline links
-- without nesting one interactive element inside another.
pillarCard
  :: [MisoString]                   -- ^ classes, on top of @pillar@
  -> View Ctx model action          -- ^ icon
  -> View Ctx model action          -- ^ title
  -> [View Ctx model action]        -- ^ description
  -> Maybe (View Ctx model action)  -- ^ badge
  -> View Ctx model action          -- ^ the stretched click target
  -> View Ctx model action
pillarCard classes icon title descViews badge target =
  H.article_ [ P.classes_ ("pillar" : classes) ]
    [ target
    , H.div_ [ P.class_ "pillar-top" ]
        [ H.div_ [ P.class_ "pillar-icon" ] [ icon ]
        , case badge of
            Just b  -> H.span_ [ P.class_ "pillar-badge" ] [ b ]
            Nothing -> vfrag []
        ]
    , H.h3_ [] [ title ]
    , H.p_ [] descViews
    , H.span_ [ P.class_ "pillar-more" ] [ "→" ]
    ]
-----------------------------------------------------------------------------
stretchedLink :: [Attribute model action] -> View Ctx model action
stretchedLink attrs = H.a_ ( P.class_ "pillar-link" : attrs ) []
-----------------------------------------------------------------------------
-- The desktop card: Lynxtron has not shipped, so the card itself goes
-- nowhere (only the inline "Lynxtron" link in the description does).
-- Clicking it shakes it left and right instead — a head shaken "no". It is
-- its own component so that a shake only re-renders this one card.
-----------------------------------------------------------------------------
data ShakeAction = Shake
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | 'Nothing' until the card is first clicked, then the parity of the click.
type Shaking = Maybe Bool
-----------------------------------------------------------------------------
desktopPillar :: Component Ctx () Shaking ShakeAction
desktopPillar = (component Nothing update view) { useContext = True }
  where
    -- A CSS animation only restarts when its animation-name changes, so
    -- every click flips between two classes running identical keyframes
    -- ('Site.Styles.shakeNo'). Clicking mid-shake therefore starts it over
    -- rather than doing nothing.
    update Shake = this %= Just . maybe False not

    view ctx () shaking =
      pillarCard
        ("pillar-desktop" : shakeClass shaking)
        iconMonitor
        (t ctx PillarDesktop)
        [ H.a_
            [ P.class_ "pillar-inline-link"
            , P.href_ "https://lynxjs.org/next/lynxtron/"
            , P.target_ "_blank"
            , P.rel_ "noopener"
            ] [ "Lynxtron" ]
        , " ", t ctx PillarDesktopDesc
        ]
        (Just (t ctx PillarComingSoon))
        (H.button_
          [ P.class_ "pillar-link"
          , P.type_ "button"
          , P.aria_ "label" (translate ctx PillarDesktop <> " — " <> translate ctx PillarComingSoon)
          , E.onClick Shake
          ] [])

    shakeClass :: Shaking -> [MisoString]
    shakeClass = \case
      Nothing    -> []
      Just False -> [ "pillar-shake-a" ]
      Just True  -> [ "pillar-shake-b" ]
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
  import Miso
    ( Component, Effect, View, component
    , vfrag, text, startApp, defaultEvents
    )
  import Miso.Lens (this, (+=), (-=))
  import Miso.String (ms)
  import Miso.Html.Element (button_)
  import Miso.Html.Event (onClick)

  data Action = Add | Subtract

  counter
    :: Component context props Int Action
  counter = component m u v
    where
      m :: Int
      m = 0

      u :: Action
        -> Effect context props Int Action
      u = \\case
        Add      -> this += 1
        Subtract -> this -= 1

      v :: context
        -> props
        -> Int
        -> View context Int Action
      v _ _ n = vfrag
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
        , feature (text "0") FeatZeroDeps FeatZeroDepsDesc (docsPage "installation")
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
-- Timeline -----------------------------------------------------------------
-----------------------------------------------------------------------------
timeline :: Ctx -> View Ctx () Nav
timeline ctx =
  H.section_ [ P.class_ "timeline-section" ]
    [ H.div_ [ P.class_ "section-head" ]
        [ H.h2_ [] [ t ctx TimelineTitle ]
        , H.p_ [] [ t ctx TimelineSubtitle ]
        ]
    , H.div_ [ P.class_ "timeline" ]
        [ item "2016" "Launched"
            [ dlink (docsPage "view-dsl") "Virtual DOM"
            , dlink (docsPage "events") "Event delegation"
            , dlink (docsPage "html-and-prerendering") "Hydration"
            ]
        , milestone "8 years in production"
        , item "2024" "Components"
            [ dlink (docsPage "installation") "Web Assembly"
            , dlink (docsPage "internals") "TypeScript"
            , dlink (docsPage "components") "VComp"
            ]
        , item "2025" "React Summit"
            [ H.a_
                [ P.href_ "https://youtu.be/l2dByiwiQcM?si=3IghUTRryYAyb7SK&t=1712"
                , P.target_ "_blank", P.rel_ "noopener" ]
                [ "LynxJS: Unlock Native for More" ]
            , dlink (nativePage "overview") "Native mobile"
            ]
        , item "2026" "React API"
            [ dlink (docsPage "props") "Props"
            , dlink (docsPage "fragments") "Fragment"
            , dlink (docsPage "context") "Context"
            , dlink (nativePage "dual-thread") "Dual Thread"
            ]
        ]
    ]
  where
    item year title bullets =
      H.div_ [ P.class_ "timeline-item" ]
        [ H.span_ [ P.class_ "timeline-dot" ] []
        , H.span_ [ P.class_ "timeline-year" ] [ year ]
        , H.h3_ [] [ title ]
        , H.ul_ [] [ H.li_ [] [ bullet ] | bullet <- bullets ]
        ]
    milestone note =
      H.div_ [ P.class_ "timeline-item milestone" ]
        [ H.span_ [ P.class_ "timeline-dot" ] []
        , H.p_ [ P.class_ "timeline-note" ] [ note ]
        ]
    dlink route label =
      H.a_ [ P.href_ (routeHref route), E.onClickPrevent (Go route) ] [ label ]
-----------------------------------------------------------------------------
-- A word from the Lynx team ------------------------------------------------------
-----------------------------------------------------------------------------
-- | The Lynx team's post announcing Haskell support, rendered as a card.
-- The whole card is one link to the post on X.
tweetSection :: Ctx -> View Ctx () Nav
tweetSection ctx =
  H.section_ [ P.class_ "tweet-section" ]
    [ H.div_ [ P.class_ "section-head" ]
        [ H.h2_ [] [ t ctx TweetTitle, " 🔓" ]
        , H.p_ []
            [ t ctx TweetSubtitle, " "
            , H.a_ [ P.class_ "pillar-inline-link", P.href_ "https://lynxjs.org", P.target_ "_blank", P.rel_ "noopener" ] [ "LynxJS.org" ]
            ]
        ]
    , H.a_
        [ P.class_ "tweet-card"
        , P.href_ tweetUrl
        , P.target_ "_blank", P.rel_ "noopener"
        , P.aria_ "label" "Read the post from Lynx on X"
        ]
        [ H.div_ [ P.class_ "tweet-head" ]
            [ H.img_
                [ P.class_ "tweet-avatar", P.src_ "/assets/lynx/lynx-avatar.jpg", P.alt_ ""
                , P.width_ "48", P.height_ "48", P.loading_ "lazy"
                ]
            , H.div_ [ P.class_ "tweet-who" ]
                [ H.span_ [ P.class_ "tweet-name" ] [ "Lynx", iconVerified ]
                , H.span_ [ P.class_ "tweet-handle" ] [ "@LynxJS_org" ]
                ]
            , H.span_ [ P.class_ "tweet-x" ] [ iconX ]
            ]
        , H.p_ [ P.class_ "tweet-body" ]
            [ "Did you know? You can now build native mobile apps with Lynx in… "
            , H.strong_ [] [ "Haskell" ], "!"
            ]
        , H.p_ [ P.class_ "tweet-body" ] [ "Thanks to Miso 🍜" ]
        , H.div_ [ P.class_ "tweet-foot" ]
            [ H.time_ [ textProp "datetime" "2026-08-31" ] [ "August 31, 2026" ]
            , H.span_ [ P.class_ "tweet-cta" ] [ "View on X", H.span_ [ P.class_ "btn-arrow" ] [ "→" ] ]
            ]
        ]
    ]
  where
    tweetUrl = "https://x.com/LynxJS_org/status/2094571507418837260"
-----------------------------------------------------------------------------
-- Generative AI ----------------------------------------------------------------------
-----------------------------------------------------------------------------
aiSection :: Ctx -> View Ctx () Nav
aiSection ctx =
  H.section_ [ P.class_ "ai-section" ]
    [ H.div_ [ P.class_ "ai-card" ]
        [ H.span_ [ P.class_ "ai-sparkle", P.aria_ "hidden" "true" ] [ "✦" ]
        , H.h2_ [] [ t ctx AiTitle ]
        , H.p_ []
            [ "A simple API ideal for coding agents like "
            , H.a_ [ P.href_ "https://www.anthropic.com/claude", P.target_ "_blank", P.rel_ "noopener" ] [ "Claude" ]
            , ", "
            , H.a_ [ P.href_ "https://openai.com/codex", P.target_ "_blank", P.rel_ "noopener" ] [ "Codex" ]
            , ", and "
            , H.a_ [ P.href_ "https://www.kimi.com", P.target_ "_blank", P.rel_ "noopener" ] [ "Kimi K3" ]
            , ", etc."
            ]
        , H.a_
            [ P.class_ "ai-badge"
            , P.href_ "https://github.com/haskell-miso/haskell-miso.org", P.target_ "_blank", P.rel_ "noopener" ]
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
