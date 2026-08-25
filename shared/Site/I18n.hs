-----------------------------------------------------------------------------
-- | Site chrome translations.
--
-- The active 'Lang' and the translation table ('Catalog') both live on the
-- app-global context ('Site.Types.Ctx'). Any component renders a translated
-- text node with @'t' ctx key@, which looks the key up in the table for the
-- active language (falling back to English).
--
-- 'Key' is a closed ADT rather than a string so that GHC checks every
-- language defines every string; the table itself ('catalog') is built by
-- enumerating the keys.
module Site.I18n
  ( Key (..)
  , catalog
  , t
  , tr
  , translate
  ) where
-----------------------------------------------------------------------------
import Miso (View, text)
import Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
import Site.Types
-----------------------------------------------------------------------------
data Key
  -- top bar
  = NavDocs | NavBlog | NavExamples | NavUI | NavTry | NavLegacy | NavSearch | NavLanguage | NavTheme | NavGitHub | NavMenu
  -- hero
  | HeroEyebrow | HeroTitle | HeroSubtitle | HeroGetStarted | HeroGitHub | HeroDrag
  -- platform pillars
  | PillarWeb | PillarWebDesc | PillarMobile | PillarMobileDesc | PillarMobileDescEnd | PillarMobileSee | PillarMobileSeeEnd | PillarDesktop | PillarDesktopDesc | PillarComingSoon
  -- code section
  | CodeTitle | CodeSubtitle | CodeLive
  -- features
  | FeaturesTitle | FeaturesSubtitle
  | FeatVdom | FeatVdomDesc | FeatComponents | FeatComponentsDesc | FeatEvents | FeatEventsDesc
  | FeatSsr | FeatSsrDesc | FeatRouter | FeatRouterDesc | FeatEffects | FeatEffectsDesc
  | FeatFfi | FeatFfiDesc | FeatNative | FeatNativeDesc
  | FeatReact | FeatReactDesc | FeatNet | FeatNetDesc
  | FeatDsl | FeatDslDesc | FeatZeroDeps | FeatZeroDepsDesc
  -- anatomy (scroll scene)
  | AnatTitle | AnatSubtitle | AnatHint | AnatMain | AnatBg | AnatApp
  | AnatStage1 | AnatStage2 | AnatStage3 | AnatStage4 | AnatStage5
  -- generative AI
  | AiTitle | AiBody | AiBadge
  -- ecosystem strip
  | EcoTitle | EcoSubtitle | EcoAll
  -- footer
  | FooterTagline | FooterDocs | FooterCommunity | FooterMore | FooterLicense
  -- docs
  | DocsTitle | DocsIntro | DocsGroupStart | DocsGroupConcepts | DocsGroupPlatform | DocsGroupNative | DocsGroupThinking
  | DocsPrev | DocsNext | DocsEnglishOnly | DocsSidebarToggle | DocsHaddocks
  -- examples
  | ExTitle | ExSubtitle | ExLive | ExSource | ExStars
  | ExCatGames | ExCatBrowser | ExCatIntegrations | ExCatPatterns | ExCatLibraries | ExCatNative
  -- blog
  | BlogTitle | BlogSubtitle | BlogArchive | BlogRead | BlogBack
  -- search
  | SearchPlaceholder | SearchNoResults | SearchHint | SearchTitle
  -- 404
  | NotFoundTitle | NotFoundBody | NotFoundHome
  deriving (Show, Eq, Enum, Bounded)
-----------------------------------------------------------------------------
-- | The translation table that is placed on the context: one row per
-- language, one @(key, string)@ pair per 'Key'.
catalog :: Catalog
catalog =
  [ (lang, [ (ms (show key), tr lang key) | key <- [minBound .. maxBound] ])
  | lang <- allLangs
  ]
-----------------------------------------------------------------------------
-- | Render a translated text node by looking the key up in the context's
-- translation table.
t :: Ctx -> Key -> View context model action
t ctx = text . translate ctx
-----------------------------------------------------------------------------
-- | Look a key up in the context's table for the active language, falling
-- back to English, then to the key name.
translate :: Ctx -> Key -> MisoString
translate Ctx {..} key =
  case lookup ctxLang ctxCatalog >>= lookup name of
    Just s -> s
    Nothing ->
      case lookup EN ctxCatalog >>= lookup name of
        Just s -> s
        Nothing -> name
  where
    name = ms (show key)
-----------------------------------------------------------------------------
-- | The source of the table: a total function per language.
tr :: Lang -> Key -> MisoString
tr EN = en
-----------------------------------------------------------------------------
en :: Key -> MisoString
en = \case
  NavDocs -> "Docs"
  NavBlog -> "Blog"
  NavExamples -> "Examples"
  NavUI -> "UI"
  NavTry -> "Try"
  NavLegacy -> "Legacy"
  NavSearch -> "Search"
  NavLanguage -> "Language"
  NavTheme -> "Toggle theme"
  NavGitHub -> "GitHub"
  NavMenu -> "Menu"
  HeroEyebrow -> "A tasty Haskell UI framework"
  HeroTitle -> "The React framework for Haskell."
  HeroSubtitle -> "miso is a fast, composable Haskell library for building web and native user interfaces. Compiles to WebAssembly or JavaScript."
  HeroGetStarted -> "Get started"
  HeroGitHub -> "View on GitHub"
  HeroDrag -> "Move your pointer"
  PillarWeb -> "Web"
  PillarWebDesc -> "Compile to WebAssembly or JavaScript. Virtual DOM, event delegation, hydration and server-side rendering out of the box."
  PillarMobile -> "Mobile"
  PillarMobileDesc -> "Ship native iOS, Android and HarmonyOS apps via"
  PillarMobileDescEnd -> "."
  PillarMobileSee -> "See the native section"
  PillarMobileSeeEnd -> "."
  PillarDesktop -> "Desktop"
  PillarDesktopDesc -> "brings Lynx to the desktop, Electron-style. The same miso components will run there unchanged."
  PillarComingSoon -> "Coming soon"
  CodeTitle -> "Your first miso application"
  CodeSubtitle -> "A model, an update function and a view. That is a Component, and a whole application."
  CodeLive -> "Live"
  FeaturesTitle -> "What's in the box"
  FeaturesSubtitle -> "miso borrows the best ideas from React and Elm and gives them Haskell types."
  FeatVdom -> "Virtual DOM"
  FeatVdomDesc -> "A keyed diffing algorithm patches only what changed. Fragments, keyed lists and lifecycle hooks included."
  FeatComponents -> "Components"
  FeatComponentsDesc -> "Self-contained model, update and view. Nest them, pass props, share a global context, or send mail."
  FeatEvents -> "Event delegation"
  FeatEventsDesc -> "One listener on <body> routes capture and bubble phases through the virtual DOM to typed handlers."
  FeatSsr -> "Prerendering & hydration"
  FeatSsrDesc -> "Render a View to HTML on the server or at build time; the client hydrates instead of redrawing."
  FeatRouter -> "Type-safe routing"
  FeatRouterDesc -> "Derive a bidirectional router from a plain sum type. Links and parsers can never disagree."
  FeatEffects -> "Effects & subscriptions"
  FeatEffectsDesc -> "IO is scheduled, never run, inside update. Timers, sockets and browser events are subscriptions."
  FeatFfi -> "JavaScript EDSL"
  FeatFfiDesc -> "Talk to any browser API with a typed DSL or inline JavaScript quasi-quotes."
  FeatNative -> "Native mobile"
  FeatNativeDesc -> "Build iOS, Android and HarmonyOS applications. See the native section."
  FeatReact -> "React API"
  FeatReactDesc -> "Context, Fragments, Props and other familiar React concepts, implemented in Haskell."
  FeatNet -> "WebSockets, SSE & Fetch"
  FeatNetDesc -> "Typed access to the browser's networking: the Fetch API, WebSockets and server-sent events."
  FeatDsl -> "HTML, SVG & CSS DSLs"
  FeatDslDesc -> "Typed combinators for every HTML, SVG and MathML element, plus a structured CSS DSL with keyframes and media queries."
  FeatZeroDeps -> "0-dependency"
  FeatZeroDepsDesc -> "miso only depends on GHC boot packages that are delivered with the compiler distribution itself."
  AnatTitle -> "Anatomy of a native miso app"
  AnatSubtitle -> "Keep scrolling and the phone comes apart into the dual-thread architecture that runs miso on iOS and Android."
  AnatHint -> "scroll"
  AnatMain -> "main thread"
  AnatBg -> "background thread"
  AnatApp -> "your application: model · update · view"
  AnatStage1 -> "A miso app on your phone."
  AnatStage2 -> "It all lives in one iOS process."
  AnatStage3 -> "Lynx hosts two JavaScript interpreters: the main thread renders, the background thread thinks."
  AnatStage4 -> "Each interpreter loads the same bundle: JavaScript, CSS, assets."
  AnatStage5 -> "And inside the JS: two GHC runtimes, one application, one miso."
  AiTitle -> "Generative AI ready"
  AiBody -> "miso itself was built without AI tools. But its DSL, thanks to its simplicity, is a great fit for agentic coding tools like Claude or Codex."
  AiBadge -> "Agent-friendly by design · 100% miso"
  EcoTitle -> "Batteries included"
  EcoSubtitle -> "Games, browser API demos, integrations and libraries, all written in miso."
  EcoAll -> "Browse all examples"
  FooterTagline -> "A tasty Haskell UI framework for web, mobile and desktop."
  FooterDocs -> "Documentation"
  FooterCommunity -> "Community"
  FooterMore -> "More"
  FooterLicense -> "BSD-3 licensed. © 2016–2026"
  DocsTitle -> "Documentation"
  DocsIntro -> "Everything from your first Component to the native dual-thread runtime."
  DocsGroupStart -> "Getting started"
  DocsGroupConcepts -> "Core concepts"
  DocsGroupPlatform -> "Platform"
  DocsGroupNative -> "Native (mobile)"
  DocsGroupThinking -> "Thinking in miso"
  DocsPrev -> "Previous"
  DocsNext -> "Next"
  DocsEnglishOnly -> "Documentation is currently available in English only."
  DocsSidebarToggle -> "Browse docs"
  DocsHaddocks -> "API reference (Haddock)"
  ExTitle -> "Examples"
  ExSubtitle -> "Real applications built with miso, from games to browser API demos to full libraries. Every repository lives in the haskell-miso GitHub organisation."
  ExLive -> "Live demo"
  ExSource -> "Source"
  ExStars -> "stars"
  ExCatGames -> "Games"
  ExCatBrowser -> "Browser APIs"
  ExCatIntegrations -> "Integrations"
  ExCatPatterns -> "Patterns"
  ExCatLibraries -> "Libraries & tooling"
  ExCatNative -> "Native"
  BlogTitle -> "Blog"
  BlogSubtitle -> "Notes from the miso maintainers."
  BlogArchive -> "Older posts live on blog.haskell-miso.org"
  BlogRead -> "Read"
  BlogBack -> "All posts"
  SearchPlaceholder -> "Search the docs…"
  SearchNoResults -> "No results for"
  SearchHint -> "↑ ↓ to navigate · ↵ to open · esc to close"
  SearchTitle -> "Search"
  NotFoundTitle -> "Page not found"
  NotFoundBody -> "There is nothing at this address. The page may have moved."
  NotFoundHome -> "Back home"
-----------------------------------------------------------------------------
