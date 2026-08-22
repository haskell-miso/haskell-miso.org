-----------------------------------------------------------------------------
-- | Documentation pages for the web runtime. The prose follows the module
-- documentation of "Miso" (each top-level haddock section is a page here).
module Site.Docs.Web
  ( startPages
  , conceptPages
  , platformPages
  ) where
-----------------------------------------------------------------------------
import Miso ((+>))
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
-----------------------------------------------------------------------------
import Site.Demos
import Site.Docs.Types
import Site.Prose
import Site.Route
-----------------------------------------------------------------------------
startPages :: [DocPage]
startPages =
  [ introduction
  , installation
  , firstComponent
  , mvu
  ]
-----------------------------------------------------------------------------
conceptPages :: [DocPage]
conceptPages =
  [ components
  , viewDsl
  , textAndFragments
  , keys
  , events
  , attributes
  , effects
  , context
  , props
  , communication
  , subscriptions
  , stateAndLenses
  ]
-----------------------------------------------------------------------------
platformPages :: [DocPage]
platformPages =
  [ routing
  , htmlAndSsr
  , javascriptEdsl
  , canvas
  , misoString
  , json
  , styles
  , development
  , internals
  ]
-----------------------------------------------------------------------------
-- Getting started
-----------------------------------------------------------------------------
introduction :: DocPage
introduction = DocPage
  { pageSlug = "introduction"
  , pageGroup = Start
  , pageTitle = "Introduction"
  , pageBlurb = "What miso is, the problems it solves, and where its ideas come from."
  , pageKeywords = [ "overview", "react", "elm", "virtual dom", "features" ]
  , pageBody =
    [ lead
      [ c "miso", " is a library for building web and native user interface applications. It provides a "
      , a "https://react.dev" "React", "-like programming experience for a simple Haskell dialect that emphasises "
      , b "performance", ", ", b "purity", ", ", b "simplicity", ", ", b "extensibility", " and ", b "composability", "."
      ]
    , h2 "What miso addresses"
    , para [ "miso covers the common areas of web development so that you can spend your time on your application:" ]
    , ul
      [ [ b "DOM manipulation", " — miso uses a ", a "https://en.wikipedia.org/wiki/Virtual_DOM" "Virtual DOM"
        , " with a diffing algorithm that is responsible for all DOM modification and ", c "Component", " lifecycle hooks." ]
      , [ b "Event delegation", " — all event listeners are attached to a top-level element (typically ", c "<body>", "). "
        , "When raised, events are routed through the virtual DOM to Haskell handlers that change application state. Both the "
        , c "capture", " and ", c "bubble", " phases are virtualised." ]
      , [ b "Prerendering", " — the server delivers HTML before the JavaScript or WebAssembly bootstraps. Instead of an initial draw, "
        , "the client populates the virtual DOM from the real DOM (\"hydration\"), which avoids a redraw and helps SEO. "
        , c "Miso.Html.Render", " renders on the server; ", c "miso", " hydrates on the client." ]
      , [ b "Components", " — a ", c "Component", " is a self-contained miso application bundling its state, the logic that "
        , "updates it and a function that renders it. Components nest to arbitrary depth." ]
      , [ b "Custom renderers", " — the underlying DOM operations are abstracted so a different rendering engine can be plugged in. "
        , "This is how ", goto (nativePage "overview") [ "miso targets mobile devices" ], " through Lynx." ]
      , [ b "Lifecycle hooks", " — ", c "mount", " and ", c "unmount", " on components, ", c "onCreated", " and ", c "onDestroyed"
        , " on elements. Commonly used for component communication and third-party JavaScript integration." ]
      , [ b "State management", " — component ", c "model", " state is manipulated with ", c "Miso.Lens", " or ", c "Miso.State", "." ]
      , [ b "HTTP / cookies", " — ", c "Miso.Fetch", " wraps the Fetch API and ", c "Miso.Cookie", " the CookieStore API, each with an "
        , "asynchronous callback-based ", c "Effect", " and a synchronous ", c "_", "-suffixed ", c "IO", " variant." ]
      ]
    , h2 "Architecture"
    , ul
      [ [ b "React", " — miso implements a subset of the React architecture internals: components, lifecycle hooks, virtual DOM, event delegation, "
        , "along with the Fragment, Props and Context API features." ]
      , [ b "Elm", " — miso also implements the Elm architecture (model–view–update) and the ", c "mailbox", " communication pattern." ]
      ]
    , h2 "Native (mobile)"
    , para
      [ "Beyond the browser, miso targets native mobile devices by driving the ", a "https://lynxjs.org" "Lynx", " runtime instead of the DOM. "
      , "The same MVU model, ", c "Component", " API, event delegation and virtual-DOM diffing carry over unchanged — only the element vocabulary differs. "
      , "The native backend is gated behind the ", c "native", " cabal flag; web and WASM builds are unaffected. See the "
      , goto (nativePage "overview") [ "native section" ], " for the dual-thread architecture."
      ]
    , h2 "Where to next"
    , ul
      [ [ goto (docsPage "installation") [ "Installation" ], " — set up the WASM toolchain and build the sample app." ]
      , [ goto (docsPage "your-first-component") [ "Your first Component" ], " — the counter, line by line." ]
      , [ goto (thinkingPage "overview") [ "Thinking in miso" ], " — how to structure a real application." ]
      ]
    ]
  }
-----------------------------------------------------------------------------
installation :: DocPage
installation = DocPage
  { pageSlug = "installation"
  , pageGroup = Start
  , pageTitle = "Installation"
  , pageBlurb = "Get a WebAssembly (or JavaScript) toolchain with nix and build the sample application."
  , pageKeywords = [ "setup", "nix", "wasm", "ghcjs", "cabal", "flake", "toolchain" ]
  , pageBody =
    [ lead
      [ "miso applications compile to WebAssembly (the primary target) with the GHC WASM backend, or to JavaScript with the GHC JS backend. "
      , "The quickest way to a working toolchain is the ", a "https://github.com/dmjio/miso/blob/master/flake.nix" "miso flake", "."
      ]
    , h2 "1. Build and serve the sampler"
    , para [ "With ", a "https://nixos.org" "nix", " installed (flakes enabled), clone the ", a "https://github.com/haskell-miso/miso-sampler" "sampler", " and run it — the flake provides the whole WASM toolchain (", c "wasm32-wasi-cabal", ", ", c "wasm32-wasi-ghc", ", ", c "http-server", ", ", c "ghciwatch", "):" ]
    , shCopy "install-copy" """
      $ git clone https://github.com/haskell-miso/miso-sampler
      $ cd miso-sampler
      $ nix develop .#wasm --command bash -c 'make all && make serve'
      """
    , para
      [ "Then open ", a "http://localhost:8080" "http://localhost:8080", ". ", c "make all", " runs ", c "wasm32-wasi-cabal build", ", copies ", c "static/", " to ", c "public/", ", generates the JS FFI glue with ", c "post-link.mjs", " and shrinks the ", c ".wasm", " with ", c "wasm-opt", ". "
      , c "make watch", " starts ", c "ghciwatch", " with the WASM browser GHCi for hot reload. A JavaScript-backend shell (", c "javascript-unknown-ghcjs-ghc", ") is available as ", c ".#ghcjs", ", and a plain GHC shell as the default." ]
    , h2 "2. Wire up your own project"
    , para [ "A miso executable is an ordinary cabal executable. For WASM builds it is linked as a reactor and exports ", c "hs_start", ":" ]
    , pre """
      executable app
        main-is: Main.hs
        build-depends: base, miso
        if arch(wasm32)
          ghc-options:
            -no-hs-main
            -optl-mexec-model=reactor
            "-optl-Wl,--export=hs_start"
          cpp-options: -DWASM
      """
    , para [ "and ", c "Main.hs", " exports its entry point when compiled for WASM:" ]
    , hs """
      {-# LANGUAGE CPP #-}
      module Main where

      import Miso

      #ifdef WASM
      foreign export javascript "hs_start" main :: IO ()
      #endif

      main :: IO ()
      main = startApp defaultEvents app
      """
    , para
      [ "The ", c "index.js", " loader instantiates the module with a WASI shim and calls ", c "hs_start", ". Copy it from the "
      , a "https://github.com/haskell-miso/miso-sampler/tree/master/static" "sampler's static/ folder", " to begin."
      ]
    , h2 "Cabal flags"
    , table [ "Flag", "Purpose" ]
      [ [ [ c "ssr" ],              [ "Enable when rendering ", c "View", "s to HTML on a server: ", c "text", " HTML-encodes and ", c "hydrateModel", " is honoured." ] ]
      , [ [ c "template-haskell" ], [ "Expose ", c "Miso.Lens.TH", " (", c "makeLenses", ", ", c "makeClassy", ")." ] ]
      , [ [ c "native" ],           [ "Enable the Lynx native backend (", c "Miso.Native", ")." ] ]
      , [ [ c "production" ],       [ "Use the minified JavaScript runtime." ] ]
      ]
    ]
  }
-----------------------------------------------------------------------------
firstComponent :: DocPage
firstComponent = DocPage
  { pageSlug = "your-first-component"
  , pageGroup = Start
  , pageTitle = "Your first Component"
  , pageBlurb = "A counter: model, update, view — and how to run it with startApp or hydrate with miso."
  , pageKeywords = [ "counter", "startApp", "component", "hello world", "App", "mount" ]
  , pageBody =
    [ lead
      [ "The core type of miso is ", c "Component", ". To define one, use the ", c "component", " smart constructor. "
      , "Below is a simple counter." ]
    , hs """
      module Main where

      import           Miso
      import           Miso.Lens
      import qualified Miso.Html.Element  as H
      import qualified Miso.Html.Event    as HE
      import qualified Miso.Html.Property as HP

      -- The four Component type parameters:
      --   context - the type of the global context
      --   props   - the props inherited from the parent
      --   Int     - the type of the Component model
      --   Action  - the action that updates the model
      counter
        :: Component context props Int Action
      counter = component m u v
        where
          -- | Initial model value
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
          v _context _props x = vfrag
            [ H.button_
                [ HE.onClick Add, HP.id_ "add" ]
                [ "+" ]
            , text (ms x)
            , H.button_
                [ HE.onClick Subtract, HP.id_ "subtract" ]
                [ "-" ]
            ]

      main :: IO ()
      main = startApp defaultEvents counter

      data Action
        = Add
        | Subtract
        deriving (Eq, Show)
      """
    , demo "The counter, running" counterSource ("demo-counter" +> counterDemo)
    , para
      [ "Four type parameters follow every ", c "Component", ": the global ", c "context", " (shared by the whole tree), the ", c "props"
      , " passed by the parent, the component's own ", c "model", " and the ", c "action", " type its ", c "update", " consumes. "
      , "A top-level application fixes ", c "context", " and ", c "props", " to ", c "()", "; the ", c "App", " synonym spells that out:" ]
    , hs """
      type App model action
        = Component () () model action

      startApp
        :: Eq model
        => Events
        -> App model action
        -> IO ()
      """
    , h2 "Running it"
    , para
      [ "We recommend ", c "startApp", " as the starting point — it sets up event listeners, performs the initial draw and assumes ", c "<body>", " is empty. "
      , "The ", c "miso", " function (and ", c "prerender", ") assume ", c "<body>", " has already been populated by the result of ", c "view"
      , ": instead of drawing, ", c "miso", " hydrates. If the structures do not match it falls back to drawing from scratch." ]
    , hs """
      main :: IO ()
      main = miso defaultEvents $ \\uri -> counter
      -- hydrate a prerendered page
      """
    , h2 "Doing something on mount"
    , para [ "It is possible to execute an initial action when a ", c "Component", " is first mounted with the ", c "mount", " hook (and, similarly, ", c "unmount", "):" ]
    , hs """
      data Action = Init | Add | Subtract

      main :: IO ()
      main = startApp defaultEvents
        counter { mount = Just Init }

      update
        :: Action
        -> Effect context props Int Action
      update = \\case
        Init -> io_ (consoleLog "hello world!")
        ...
      """
    , note
      [ c "startApp", " and ", c "miso", " always infer ", c "context", " as ", c "()", ". Use ", c "startAppWithContext", " (or ", c "misoWithContext", " when hydrating) to seed a non-trivial context — see "
      , goto (docsPage "context") [ "Context" ], "." ]
    ]
  }
-----------------------------------------------------------------------------
mvu :: DocPage
mvu = DocPage
  { pageSlug = "model-view-update"
  , pageGroup = Start
  , pageTitle = "Model–View–Update"
  , pageBlurb = "The Elm architecture as miso implements it: a left fold over actions."
  , pageKeywords = [ "mvu", "elm", "architecture", "model", "view", "update", "fold" ]
  , pageBody =
    [ lead
      [ "The ", c "Component", " API adheres to the ", a "https://elm-lang.org" "Elm", " MVU (model–view–update) interface. "
      , "It is similar to a left fold over ", c "action", "s: the ", c "model", " is updated by ", c "update", " and rendered by ", c "view", "." ]
    , figure
      [ pre """
        actions:  Add    Add  Subtract
                   │      │      │
                   ▼      ▼      ▼
        update: 0 ───▶ 1 ───▶ 2 ───▶ 1 ───▶ ...
                │      │      │      │
                ▼      ▼      ▼      ▼
        view:   ▢      ▢      ▢      ▢
                (virtual DOM, diffed & patched)
        """ ]
      [ "Every action folds into the model; every model is rendered exactly once." ]
    , ul
      [ [ b "model", " — any user-defined type. An ", c "Eq", " instance is required (we recommend the derived one): miso only redraws when the model actually changed." ]
      , [ b "view", " — the templating function that constructs a new virtual DOM (or HTML when rendering on the server). It is pure: ", c "context -> props -> model -> View", "." ]
      , [ b "update", " — describes how the model evolves in response to actions raised by the application. It takes any ", c "action", ", updates the model and optionally schedules ", c "IO", "." ]
      ]
    , h2 "Why it works"
    , para
      [ "Because ", c "view", " is a pure function of state, there is exactly one place where state changes (", c "update", ") and one place where the UI is described (", c "view", "). "
      , "Effects never run inside ", c "update", " — they are ", em "scheduled", " and their results come back as new actions, so the fold never observes partial state. "
      , "This is what makes miso applications easy to reason about, easy to test and trivial to prerender." ]
    , para [ "Continue with ", goto (docsPage "components") [ "Components" ], " to see how many small MVU loops compose into an application, or read ", goto (thinkingPage "overview") [ "Thinking in miso" ], " for a guided walkthrough." ]
    ]
  }
-----------------------------------------------------------------------------
-- Core concepts
-----------------------------------------------------------------------------
components :: DocPage
components = DocPage
  { pageSlug = "components"
  , pageGroup = Concepts
  , pageTitle = "Components"
  , pageBlurb = "The Component record, composition with (+>), keys and the mount / unmount lifecycle."
  , pageKeywords = [ "component", "+>", "mount_", "mountWithProps_", "lifecycle", "unmount", "VComp", "SomeComponent" ]
  , pageBody =
    [ lead
      [ "A ", c "Component", " bundles a model, the ", c "update", " that evolves it and the ", c "view", " that renders it — plus everything around the edges: subscriptions, lifecycle hooks, a mailbox. "
      , "Components nest, forming a typed UI tree." ]
    , note
      [ "Always build components with the ", c "component", " smart constructor — ", c "component m u v", " — rather than the ", c "Component", " record constructor directly. It fills every other field with sane defaults; override the ones you need with record-update syntax, e.g. ", c "(component m u v) { subs = [...] }", "." ]
    , h2 "The record"
    , para [ "The ", c "component", " smart constructor fills in sane defaults; you override fields with record update syntax:" ]
    , hs """
      data Component context props model action = Component
        { model :: model
          -- initial model
        , hydrateModel :: Maybe (IO model)
          -- optional model to hydrate from (SSR)
        , update
            :: action
            -> Effect context props model action
        , view
            :: context
            -> props
            -> model
            -> View context model action
        , useContext :: Bool
          -- re-render when the global context changes
        , subs :: [ Sub action ]
          -- long-running subscriptions
        , styles :: [ CSS ]
          -- dev only: append <style>/<link> to <head>
        , scripts :: [ JS ]
          -- dev only: append <script> to <head>
        , mountPoint :: Maybe MountPoint
          -- defaults to <body>
        , logLevel :: LogLevel
          -- Off | DebugHydrate | DebugEvents | DebugAll
        , mailbox :: Value -> Maybe action
          -- receive mail from other components
        , eventPropagation :: Bool
          -- let events bubble past this component
        , mount :: Maybe action
          -- action dispatched on mount
        , unmount :: Maybe action
          -- action dispatched on unmount
        , onPropsChanged
            :: Maybe (props -> props -> action)
        }
      """
    , h2 "Composition"
    , para
      [ "Components can contain other components. This is accomplished through the mounting combinator ", c "(+>)", ", which encodes a typed component hierarchy. "
      , "All components in a tree share the same global ", c "context", " type." ]
    , hs """
      (+>)
        :: (Eq context, Eq model)
        => MisoString
        -> Component context () model action
        -> View context parentModel parentAction
      key +> comp = VComp (SomeComponent (Just (toKey key)) () comp)
      """
    , para [ "Practically, using this combinator looks like:" ]
    , hs """
      viewModel
        :: context
        -> props
        -> Int
        -> View context Int Action
      viewModel _ _ _ =
        H.div_ [ HP.id_ "container" ]
          [ "counter" +> counter ]
      """
    , para
      [ "The ", c "\"counter\"", " string is a unique ", c "Key", " that identifies the component at runtime. Keys matter when diffing two components: "
      , "when intentionally replacing a component it is important to specify a new key, otherwise the old one will not be unmounted." ]
    , para
      [ "It is possible to mount a component with ", c "mount_", ", which avoids specifying a key, but this should only be used when you are certain the component will never be diffed against another component. "
      , "When in doubt, use ", c "(+>)", " and key your component. To pass ", goto (docsPage "props") [ "props" ], " use ", c "mountWithProps_", " (keyed) or ", c "mountWithProps", "." ]
    , h2 "Lifecycle hooks"
    , para [ "Components are mounted during diffing. All components are equipped with ", c "mount", " and ", c "unmount", " hooks, allowing custom actions to be dispatched in response to lifecycle events:" ]
    , hs """
      child :: Component ctx () Model Action
      child = (component m u v)
        { mount   = Just Connect
        , unmount = Just Disconnect
        }
      """
    , para [ "Element nodes have their own hooks (", c "onCreated", ", ", c "onDestroyed", ", …) — see ", goto (docsPage "view-dsl") [ "the View DSL" ], "." ]
    , demo "mount, unmount, subs and the mailbox" lifecycleSource ("demo-lifecycle" +> lifecycleDemo)
    , para [ "Toggle the clock: the child's ", c "mount", " and ", c "unmount", " actions run, its ", c "subs", " start and stop with it, and it reports back through the parent's ", c "mailbox", ". Note the ", c "\"clock\" +> clock", " key — that is what makes the diff mount and unmount rather than patch." ]
    , h2 "The View type"
    , para [ "The ", c "View", " is a rose tree of nodes, mutually recursive with ", c "Component", " through ", c "view", ":" ]
    , hs """
      data View context model action
        = VNode Namespace Tag [Attribute model action] [View context model action] DirectEvents
        | VText (Maybe Key) MisoString
        | VComp (SomeComponent context)
        | forall props. VCompStatic (StaticPtr (SomeStaticComponent props context)) props
        | VFrag (Maybe Key) [View context model action]

      data SomeComponent context
        = forall model action props. (Eq context, Eq model, Eq props)
        => SomeComponent (Maybe Key) props (Component context props model action)
      """
    , para
      [ c "VNode", " and ", c "VText", " map one-to-one onto the physical DOM. ", c "VComp", " and ", c "VFrag", " are abstract (they live only in the virtual DOM). "
      , "The existential ", c "SomeComponent", " is what allows embedding polymorphic components in a ", c "View", ". ", c "VCompStatic", " carries a static pointer to its constructor and is used by the "
      , goto (nativePage "static-mounting") [ "native dual-thread runtime" ], "." ]
    ]
  }
-----------------------------------------------------------------------------
viewDsl :: DocPage
viewDsl = DocPage
  { pageSlug = "view-dsl"
  , pageGroup = Concepts
  , pageTitle = "The View DSL"
  , pageBlurb = "Element nodes, the smart constructors in Miso.Html.Element and element lifecycle hooks."
  , pageKeywords = [ "VNode", "node", "vnode", "div_", "html", "svg", "mathml", "onCreated", "onDestroyed", "highlight.js" ]
  , pageBody =
    [ lead
      [ "A ", c "VNode", " represents a DOM element — the most common kind of virtual DOM node. It carries a ", c "Namespace", ", a tag name, a list of ", c "Attribute", " values and a list of child ", c "View", "s." ]
    , hs """
      VNode HTML "div" [ HP.id_ "container" ] [ "Hello, world!" ]
      """
    , para [ "In practice you rarely construct ", c "VNode", " directly. Use the element smart constructors from ", c "Miso.Html.Element", ", which fix the namespace and tag for you:" ]
    , hs """
      H.div_    [ HP.id_ "container" ]        [ "Hello, world!" ]
      H.button_ [ HE.onClick DoSomething ]    [ "Click me" ]
      H.h1_     [ HP.className "title" ]      [ text (ms pageTitle) ]
      """
    , para [ "For elements not covered by ", c "Miso.Html.Element", ", use ", c "node", " (or its synonym ", c "vnode", ") directly:" ]
    , hs """
      node HTML "details" []
        [ node HTML "summary" [] [ "More info" ] ]
      """
    , para
      [ "SVG and MathML elements use the ", c "SVG", " and ", c "MATHML", " namespaces and are covered by ", c "Miso.Svg.Element", " and ", c "Miso.Mathml.Element", ". "
      , "Unlike ", c "VComp", " and ", c "VFrag", ", a ", c "VNode", " has a one-to-one correspondence with a physical DOM element." ]
    , api
      [ ("node",  [ "raw constructor — namespace, tag, attributes, children" ])
      , ("vnode", [ "synonym for ", c "node" ])
      , ("Miso.Html.Element", [ "every HTML element (", c "div_", ", ", c "span_", ", ", c "input_", " …)" ])
      , ("Miso.Svg.Element", [ "SVG graphics, animation and container elements" ])
      , ("Miso.Mathml.Element", [ "MathML elements" ])
      ]
    , h2 "Lifecycle hooks"
    , para [ "Like components, elements expose lifecycle hooks:" ]
    , ul
      [ [ c "onBeforeCreated" ]
      , [ c "onCreated", " / ", c "onCreatedWith" ]
      , [ c "onBeforeDestroyed", " / ", c "onBeforeDestroyedWith" ]
      , [ c "onDestroyed" ]
      ]
    , para [ "These are useful for initialising and tearing down third-party libraries, as in this example using ", a "https://highlightjs.org" "highlight.js", ":" ]
    , hs """
      {-# LANGUAGE QuasiQuotes      #-}
      {-# LANGUAGE MultilineStrings #-}

      import Miso
      import Miso.FFI.QQ (js)

      data Action = Highlight DOMRef

      update
        :: Action
        -> Effect context props model Action
      update = \\case
        Highlight domRef ->
          io_ [js| hljs.highlightElement(${domRef}) |]

      view
        :: context
        -> props
        -> model
        -> View context model Action
      view _ _ _ =
        H.code_ [ onCreatedWith Highlight ]
          [ \"\"\"
            function addOne (x) { return x + 1; }
            \"\"\"
          ]
      """
    , para [ "As a convention, the ", c "*With", " variant of a lifecycle hook (e.g. ", c "onCreatedWith", ") provides the target ", c "DOMRef", " to the callback." ]
    , h2 "The smart constructors, at a glance"
    , api
      [ ("node, vnode", [ "build a ", c "VNode" ])
      , ("text, vtext", [ "build a ", c "VText", " — see ", goto (docsPage "text-and-fragments") [ "Text & fragments" ] ])
      , ("component", [ "build a ", c "VComp" ])
      , ("fragment, vfrag, fragment_, vfrag_", [ "build a ", c "VFrag" ])
      , ("(+>)", [ "key and mount a child ", c "Component" ])
      ]
    ]
  }
-----------------------------------------------------------------------------
textAndFragments :: DocPage
textAndFragments = DocPage
  { pageSlug = "text-and-fragments"
  , pageGroup = Concepts
  , pageTitle = "Text & fragments"
  , pageBlurb = "VText nodes (HTML encoding, keyed text) and VFrag — grouping siblings without a wrapper element."
  , pageKeywords = [ "VText", "text", "textRaw", "text_", "textKey", "VFrag", "fragment", "vfrag_", "IsString" ]
  , pageBody =
    [ lead
      [ "A ", c "VText", " represents a DOM text node; a ", c "VFrag", " groups siblings without a wrapper element, like React's ", c "<></>", ". "
      , "Both participate in keyed reconciliation." ]
    , h2 "Text nodes"
    , para [ "The simplest way to produce a ", c "VText", " is via the ", c "IsString", " instance on ", c "View", ". String literals inside a child list are automatically promoted to text nodes:" ]
    , hs """
      H.div_ [] [ "Hello, world!" ]
      """
    , para [ "For dynamic content, use the ", c "text", " smart constructor with a ", c "MisoString", ":" ]
    , hs """
      H.div_ [] [ text (ms userName) ]
      """
    , h3 "HTML encoding"
    , para
      [ "When compiling with the ", c "ssr", " flag, ", c "text", " automatically HTML-encodes its argument — ", c "<", ", ", c ">", ", ", c "&", ", ", c "\"", " and ", c "'", " become entities. "
      , "This prevents accidental XSS when rendering user-supplied strings on the server. To embed trusted, pre-rendered content without escaping use ", c "textRaw", "; it is a no-op on the client and bypasses encoding on the server." ]
    , hs """
      text    "<b>bold</b>"   -- SSR output: &lt;b&gt;bold&lt;/b&gt;
      textRaw "<b>bold</b>"   -- server and client: <b>bold</b>
      """
    , h3 "Concatenating and keying"
    , para [ c "text_", " accepts a list of strings and joins them with a single space. A ", c "VText", " may also carry a ", c "Key", " (", c "textKey", ", ", c "textKey_", "): keyed text nodes take part in the same reconciliation as keyed elements, so a stable key prevents unnecessary text-node replacement when sibling order changes." ]
    , hs """
      H.div_ [] [ text_ [ "Hello", "world" ] ]
      -- renders: Hello world

      renderItem
        :: Item
        -> View context model Action
      renderItem item =
        H.li_ []
          [ textKey (itemId item) (itemLabel item) ]
      """
    , api
      [ ("text",     [ "single string, HTML-encoded on the server" ])
      , ("vtext",    [ "synonym for ", c "text" ])
      , ("textRaw",  [ "single string, never HTML-encoded" ])
      , ("text_",    [ "list of strings joined with a space" ])
      , ("textKey",  [ "single keyed string" ])
      , ("textKey_", [ "list of keyed strings joined with a space" ])
      ]
    , h2 "Fragments"
    , para [ c "VFrag", " groups sibling nodes without a wrapper element in the DOM, analogous to the React Fragment API and the browser's ", c "DocumentFragment", ":" ]
    , hs """
      -- Renders two <li> elements as direct
      -- siblings, no enclosing element
      fragment
        [ H.li_ [] [ "Item A" ]
        , H.li_ [] [ "Item B" ]
        ]

      -- Keyed fragment — survives reordering
      -- without full teardown / remount
      vfrag_ "my-key"
        [ H.li_ [] [ "Item A" ]
        , H.li_ [] [ "Item B" ]
        ]
      """
    , para
      [ "Fragments may be nested. The differ recurses into nested fragments and processes them as if they were a flat sequence of sibling DOM nodes, so nesting carries no runtime cost beyond the constructor allocation. "
      , "Empty fragments in child lists are erased before diffing and are therefore a no-op." ]
    , api
      [ ("fragment",  [ "unkeyed fragment" ])
      , ("vfrag",     [ "unkeyed fragment (alias)" ])
      , ("fragment_", [ "keyed fragment" ])
      , ("vfrag_",    [ "keyed fragment (alias, infix-friendly: ", c "\"key\" `vfrag_` [...]", ")" ])
      ]
    ]
  }
-----------------------------------------------------------------------------
keys :: DocPage
keys = DocPage
  { pageSlug = "keys"
  , pageGroup = Concepts
  , pageTitle = "Keys"
  , pageBlurb = "What keys mean for diffing, identity and preserving DOM references across updates."
  , pageKeywords = [ "key_", "keyProp", "reconciliation", "diff", "identity", "animation" ]
  , pageBody =
    [ lead [ "A ", c "Key", " is a unique identifier used to optimise diffing. Virtual DOM nodes can be keyed with ", c "key_", ". Keys have multiple meanings in miso (as in React)." ]
    , h2 "Keys optimise child-list diffing"
    , para
      [ "When two lists of elements are diffed and all of them have unique keys, diffing large child lists is much faster. This optimisation fires automatically when ", em "all", " elements in a child list carry unique keys — unless every node in the list is keyed, it will not." ]
    , h2 "Keys compare two identical nodes"
    , para
      [ "If two ", c "VNode", "s (or two ", c "VComp", "s) are being compared and their keys differ, the old node is destroyed and a new one created. Otherwise the underlying DOM node is kept and its properties diffed. "
      , "For components, differing keys trigger the ", c "unmount", " phase for the old component and the ", c "mount", " phase for the new one; the DOM reference is replaced." ]
    , h2 "Keys preserve the DOM reference"
    , para
      [ "Because a stable key keeps the same DOM node in place, CSS animations on that node are not interrupted by re-renders. Without a key the differ may recreate the node, resetting any in-progress animation. "
      , "Assigning a stable key to an animated element guarantees the animation runs to completion." ]
    , h2 "Usage"
    , para [ "See the ", c "key_", " property, and smart constructors like ", c "textKey_", ", ", c "vfrag_", " and ", c "(+>)", ":" ]
    , hs """
      H.ul_ []
        [ H.li_ [ key_ "key-1" ] [ "a" ]
        , H.li_ [ key_ "key-2" ] [ "b" ]
        , "key-3" +> counter
        , textKey "key-4" "text here"
        , vfrag_ "key-5" [ "foo", "bar" ]
        ]
      """
    ]
  }
-----------------------------------------------------------------------------
events :: DocPage
events = DocPage
  { pageSlug = "events"
  , pageGroup = Concepts
  , pageTitle = "Events"
  , pageBlurb = "Event delegation through <body>, the Events map, defining handlers with on, and decoding events."
  , pageKeywords = [ "onClick", "defaultEvents", "keyboardEvents", "Decoder", "on", "onCapture", "capture", "bubble", "delegation", "valueDecoder" ]
  , pageBody =
    [ lead [ "By default all events are delegated through ", c "<body>", ". miso supports both the ", c "capture", " and ", c "bubble", " phases of browser events, and applications can handle either." ]
    , h2 "Using events"
    , para
      [ "miso exposes ", c "defaultEvents", " for convenience — commonly used events that are listened for on ", c "<body>", " and routed through the ", c "View", " to the virtual DOM node that raised them. "
      , "Other groups are exposed as conveniences too (", c "keyboardEvents", ", ", c "mouseEvents", ", ", c "pointerEvents", ", ", c "touchEvents", ", …). "
      , "All events required by all your components must be combined when running the application:" ]
    , hs """
      main =
        startApp
          (defaultEvents <> keyboardEvents <> touchEvents)
          app

      touchEvents :: Events
      touchEvents = M.fromList
        [ ("touchstart",  BUBBLE)
        , ("touchcancel", BUBBLE)
        , ("touchmove",   BUBBLE)
        , ("touchend",    BUBBLE)
        ]
      """
    , note [ c "defaultEvents", " contains ", c "blur", ", ", c "change", ", ", c "click", ", ", c "contextmenu", ", ", c "dblclick", ", ", c "focus", ", ", c "input", ", ", c "select", " and ", c "submit", ". Using ", c "onKeyDown", " without listening for ", c "keydown", " is the classic mistake — enable ", c "DebugEvents", " to catch it." ]
    , h2 "Defining event handlers"
    , para
      [ "Define your own handlers with the ", c "on", " combinator. By default this defines an event in the ", c "BUBBLE", " phase; see ", c "onCapture", " for the ", c "CAPTURE", " phase and ", c "onWithOptions", " for ", c "preventDefault", " / ", c "stopPropagation", ". "
      , c "Miso.Html.Event", " has many predefined events." ]
    , hs """
      onChangeWith
        :: (MisoString -> DOMRef -> action)
        -> Attribute model action
      onChangeWith = on "change" valueDecoder
      """
    , para [ "The ", c "*With", " variant of an event (e.g. ", c "onChangeWith", ") provides the target ", c "DOMRef", " to the callback." ]
    , h2 "Decoding events"
    , para [ "After an event is raised, information is extracted from it with a ", c "Decoder", ". Many common decoders are available in ", c "Miso.Event.Decoder", "." ]
    , hs """
      data Decoder a = Decoder
        { decoder :: Value -> Parser a
          -- Miso.JSON parser
        , decodeAt :: DecodeTarget
          -- path into the event object
        }

      -- | A custom Decoder for the `value`
      -- property of an event target.
      valueDecoder :: Decoder MisoString
      valueDecoder = Decoder {..}
        where
          decodeAt = DecodeTarget ["target"]
          decoder =
            withObject "target" $ \\o -> o .: "value"
      """
    , para [ "A decoder that reads several fields, used with ", c "on", ":" ]
    , hs """
      clickDecoder :: Decoder (Int, Int)
      clickDecoder = Decoder
        { decodeAt = DecodeTarget []
        , decoder = withObject "click" $ \\o -> do
            ox <- o .: "offsetX"
            oy <- o .: "offsetY"
            pure (floor ox, floor oy)
        }

      view =
        H.canvas_
          [ on "click" clickDecoder $ \\(x, y) _ _ ->
              Clicked x y
          ]
          []
      """
    , h2 "Try it"
    , demo "Built-in handlers and a custom decoder" eventsSource ("demo-events" +> eventsDemo)
    ]
  }
-----------------------------------------------------------------------------
attributes :: DocPage
attributes = DocPage
  { pageSlug = "attributes"
  , pageGroup = Concepts
  , pageTitle = "Attributes & properties"
  , pageBlurb = "The Attribute type, the smart constructors, custom properties and the property-vs-attribute rule."
  , pageKeywords = [ "Attribute", "Property", "prop", "textProp", "boolProp", "className", "classList", "style_", "key_" ]
  , pageBody =
    [ lead [ "The ", c "Attribute", " type carries everything that can be attached to a DOM element:" ]
    , hs """
      data Attribute model action
        = Property MisoString Value
          -- DOM property (key/value)
        | ClassList [MisoString]
          -- CSS class list
        | On (model -> Sink action -> ...)
          -- fully-applied event handler
        | OnStatic
            (StaticPtr (EventHandler model action))
          -- static handler, rebuilt on the
          -- main thread (dual-thread)
        | Styles (Map MisoString MisoString)
          -- inline style map
      """
    , para [ "In practice you never construct these directly. Use the smart constructors from ", c "Miso.Html.Property", ", ", c "Miso.Html.Event", ", ", c "Miso.Property", " and ", c "Miso.CSS", ":" ]
    , hs """
      H.div_
        [ HP.id_ "container"
          -- textProp "id"
        , HP.className "card"
          -- ClassList
        , HP.classList_ [ ("active", isActive) ]
          -- ClassList, conditional
        , HP.disabled_
          -- boolProp "disabled" True
        , HE.onClick MyAction
          -- On event handler
        , CSS.style_ [ CSS.display "flex" ]
          -- Styles map
        ]
        []
      """
    , h2 "Custom properties"
    , para [ "Use ", c "prop", " (or the typed variants ", c "textProp", ", ", c "boolProp", ", ", c "intProp", ", ", c "doubleProp", ", ", c "objectProp", ") from ", c "Miso.Property", " to set arbitrary DOM properties:" ]
    , hs """
      prop "data-index" (42 :: Int)
      -- sets element.data-index = 42

      textProp "placeholder" "Search…"
      -- sets element.placeholder

      boolProp "checked" True
      -- sets element.checked = true
      """
    , para
      [ "Note that DOM ", em "properties", " and HTML ", em "attributes", " are distinct. miso tries to set properties on the DOM node object (e.g. ", c "node.checked", ") first, then falls back to setting the HTML attribute (", c "setAttribute(\"checked\", …)", "). "
      , "This matches what the browser exposes in JavaScript and avoids common pitfalls with boolean attributes." ]
    , h2 "Keys"
    , para [ c "key_", " (and its alias ", c "keyProp", ") attaches a reconciliation key to any element. See ", goto (docsPage "keys") [ "Keys" ], " for details." ]
    , hs """
      data Item = Item { itemId, itemLabel :: MisoString }

      H.li_ [ key_ (itemId item) ] [ text (itemLabel item) ]
      """
    , h2 "Try it"
    , demo "Conditional classes, structured styles and properties" attrsSource ("demo-attrs" +> attrsDemo)
    ]
  }
-----------------------------------------------------------------------------
effects :: DocPage
effects = DocPage
  { pageSlug = "effects"
  , pageGroup = Concepts
  , pageTitle = "Effects"
  , pageBlurb = "The Effect monad: mutating the model, scheduling asynchronous and synchronous IO, and the Sink."
  , pageKeywords = [ "Effect", "io", "io_", "sync", "withSink", "Sink", "issue", "batch", "RWS", "ComponentInfo", "MonadState" ]
  , pageBody =
    [ lead
      [ "The ", c "Effect", " type is used to mutate the ", c "model", " over time in response to ", c "action", "s. It also allows ", c "IO", " to be ", em "scheduled", " for evaluation by the miso scheduler. "
      , c "IO", " is never evaluated inside ", c "Effect", ", it is only scheduled — there is no ", c "MonadIO", " instance." ]
    , para [ c "Effect", " is defined as an ", c "RWS", ":" ]
    , hs """
      type Effect context props model action
        = RWS
            (ComponentInfo context props)
            [Schedule context action]
            model
            ()
      """
    , ul
      [ [ "The ", b "Reader", " portion is ", c "ComponentInfo", ": ", c "ask", ", ", c "asks", " and ", c "Miso.Lens.view", " read its fields (the current ", c "ComponentId", ", the parent id, the ", c "DOMRef", " the component is mounted on, ", c "props", ", ", c "context", ")." ]
      , [ "The ", b "Writer", " portion schedules ", c "IO", ". ", c "tell", " creates a ", c "Schedule", " that runs according to its ", c "Synchronicity", "; see ", c "withSink", "." ]
      , [ "The ", b "State", " portion is the ", c "model", ": ", c "get", ", ", c "put", ", ", c "modify", " and the ", c "MonadState", " lens operators from ", c "Miso.Lens", "." ]
      ]
    , h2 "Asynchronous IO"
    , api
      [ ("io", [ "introduce asynchronous IO whose result is dispatched as an action; ", c "io_", " discards the result." ])
      , ("withSink", [ "the core function from which most other combinators are defined — gives access to the event ", c "Sink", ". The scheduler attaches exception handlers to all IO." ])
      , ("issue", [ "dispatch an action asynchronously (", c "batch", " for several)." ])
      , ("tell", [ "for maximum flexibility the ", c "MonadWriter", " instance schedules raw ", c "Schedule", "s." ])
      ]
    , hs """
      update
        :: Action
        -> Effect ctx props Model Action
      update = \\case
        FetchUser uid ->
          io (GotUser <$> lookupUser uid)
          -- async, the result becomes an action
        Log msg ->
          io_ (consoleLog msg)
          -- async, fire and forget
        Tick ->
          withSink $ \\sink -> forkTimer (sink Tock)
      """
    , h2 "Synchronous IO"
    , para [ c "sync", " forces the scheduler to evaluate IO synchronously (", c "sync_", " discards the result). It is recommended to use ", c "io", " by default — ", c "sync", " ", em "will", " block the scheduler. Reserve it for cheap reads such as ", c "localStorage", " or measuring a ", c "DOMRef", "." ]
    , h2 "The Sink"
    , hs """
      type Sink action = action -> IO ()
      """
    , para [ "A ", c "Sink", " writes any action to the global event queue. Subscriptions receive one; ", c "withSink", " hands you the current component's." ]
    , h2 "Managing model state"
    , para
      [ "Any ", c "MonadState", " function may be used to manipulate the model — ", c "get", ", ", c "put", ", ", c "modify", " — plus the lens operators (", c ".=", ", ", c "%=", ", ", c "+=", " …) from ", c "Miso.Lens", ". See ", goto (docsPage "state-and-lenses") [ "State & lenses" ], "." ]
    , h2 "Try it"
    , demo "io and io_: scheduling asynchronous work" effectsSource ("demo-effects" +> effectsDemo)
    ]
  }
-----------------------------------------------------------------------------
context :: DocPage
context = DocPage
  { pageSlug = "context"
  , pageGroup = Concepts
  , pageTitle = "Context"
  , pageBlurb = "The global context: one value shared by the whole component tree, read in view and update, changed with modifyContext."
  , pageKeywords = [ "context", "useContext", "startAppWithContext", "modifyContext", "putContext", "getContext", "setContext", "theme", "i18n" ]
  , pageBody =
    [ lead
      [ c "context", " is miso's analogue of ", a "https://react.dev/learn/passing-data-deeply-with-context" "React Context", ": a single, global value shared by ", em "every", " component in the tree, without threading it through props at each level. "
      , "This very site keeps its language table and colour theme there." ]
    , para [ "Contrast the three pieces of state a component sees, by scope:" ]
    , ul
      [ [ b "model", " — private to a single component." ]
      , [ b "props", " — passed from a parent to its immediate child." ]
      , [ b "context", " — global; the same value is visible to the whole tree." ]
      ]
    , para
      [ "This is why ", c "context", " is a type parameter on both ", c "Component", " and ", c "View", ": it is threaded through the entire tree so that every nested component — reachable via ", c "SomeComponent", " — is statically guaranteed to agree on ", em "one", " context type. There is exactly one live context value per application." ]
    , h2 "Seeding"
    , api
      [ ("startAppWithContext", [ "the client entry point, replaces ", c "startApp", "." ])
      , ("misoWithContext / prerenderWithContext", [ "the hydrating counterparts of ", c "miso", " / ", c "prerender", "." ])
      , ("setContext", [ "seeds the value directly. Needed for server-side rendering, where a ", c "View", " is serialised without starting the runtime." ])
      , ("liveWithContext / reloadWithContext", [ "context-aware variants of ", c "live", " / ", c "reload", " for interactive (GHCi) development." ])
      ]
    , h2 "Reading"
    , para [ "The current context is the ", b "first argument", " of every component's ", c "view", ", so any component — however deeply nested — reads it synchronously during render:" ]
    , hs """
      view
        :: context
        -> props
        -> model
        -> View context model action
      view ctx _props _model = ...
      """
    , para [ "Inside ", c "update", " it is readable in the ", c "Effect", " monad, just like props — use ", c "getContext", " (or ", c "Miso.Lens.view", " with the ", c "context", " lens):" ]
    , hs """
      update Toggle = do
        ctx <- getContext
        ...
      """
    , h2 "Updating"
    , para [ "Mutate the context with ", c "modifyContext", " (or ", c "putContext", " to replace it):" ]
    , hs """
      update Toggle =
        modifyContext $ \\theme ->
          if theme == Light then Dark else Light
      """
    , h2 "Re-rendering on change"
    , para
      [ "When the context value changes (per its ", c "Eq", " instance), every component with ", c "useContext = True", " is re-rendered against the new value. ", c "useContext", " defaults to ", c "False", ", so components opt in:" ]
    , hs """
      child = (component m u v) { useContext = True }
      """
    , note
      [ c "useContext", " controls whether a component ", em "reacts", " to context changes, not whether it may ", em "change", " the context. A component (typically the top-level one) can call ", c "modifyContext", " with ", c "useContext = False", "; it simply won't re-render in response. "
      , "Set ", c "useContext = True", " on precisely those components whose ", c "view", " depends on the context." ]
    , h2 "Try it"
    , demo "Reading and changing the site's context" contextSource ("demo-context" +> contextDemo)
    , para [ "The first component opts in with ", c "useContext = True", " and re-renders on every change; the second does not, so it keeps showing whatever the context was when it mounted. The button really does change the site's theme — the top bar's toggle and this demo share one value." ]
    , h2 "Example: this website"
    , para [ "The site's context is a record with the active language, a translation table and the theme. The top bar's dropdown calls ", c "modifyContext", "; every page component has ", c "useContext = True", " and renders text nodes by looking keys up in the table:" ]
    , hs """
      data Ctx = Ctx
        { ctxLang    :: Lang
        , ctxCatalog :: Catalog
        , ctxTheme   :: Theme
        } deriving Eq

      t :: Ctx -> Key -> View Ctx model action
      t ctx key = text (translate ctx key)

      update (SetLang l) = do
        modifyContext (\\ctx -> ctx { ctxLang = l })
        io_ (setLocalStorage "miso.lang" (langCode l))
      """
    ]
  }
-----------------------------------------------------------------------------
props :: DocPage
props = DocPage
  { pageSlug = "props"
  , pageGroup = Concepts
  , pageTitle = "Props"
  , pageBlurb = "Read-only data passed from a parent to a child at mount time, with mountWithProps_ and getProps."
  , pageKeywords = [ "props", "mountWithProps_", "getProps", "onPropsChanged", "parent", "child" ]
  , pageBody =
    [ lead
      [ "Inspired by ", a "https://react.dev/learn/passing-props-to-a-component" "React props", ", miso allows a parent component to pass read-only data down to a child via ", em "props", ". "
      , "Props are synchronous: when they change in the parent, the child re-renders." ]
    , h2 "Props vs component-local state"
    , ul
      [ [ b "model", " — component-local state, owned and mutated exclusively by the component through its ", c "update", ". No other component can write to it directly." ]
      , [ b "props", " — data ", em "inherited", " from the parent. Props flow downward and are read-only from the child's perspective; the parent decides what to pass at mount time." ]
      ]
    , para
      [ "This mirrors React's distinction between ", c "useState", " and the props a function component receives." ]
    , h3 "When to use props"
    , para
      [ "Props suit ", em "metadata", " — contextual or configuration data the child needs to know about but should not own: a display name, a theme token, a locale, a read-only identifier. "
      , "If the data drives the child's own business logic — counters it increments, form fields it edits, async state it manages — it belongs in the child's ", c "model", ". Prefer props for \"what the child should know\" and the model for \"what the child should do\"." ]
    , h2 "Props in view and update"
    , para [ c "view", " always takes props as its second argument; top-level applications have no parent, so props are ", c "()", ":" ]
    , hs """
      view
        :: context
        -> props
        -> model
        -> View context model action
      """
    , para [ "Use ", c "getProps", " inside ", c "Effect", " (or ", c "Miso.Lens.view props", ") to read the current value:" ]
    , hs """
      update = \\case
        SomeAction -> do
          p <- getProps
          io_ (consoleLog (ms (show p)))
      """
    , h2 "Passing props to a child"
    , para [ "Use ", c "mountWithProps_", " (keyed) or ", c "mountWithProps", " (unkeyed) in the parent's ", c "view", ":" ]
    , hs """
      mountWithProps_
        :: (Eq context, Eq model, Eq props)
        => MisoString
        -> props
        -> Component context props model action
        -> View context parentModel parentAction
      """
    , h2 "Example: child reading parent-supplied props"
    , hs """
      -- The props type: what the parent
      -- shares with the child
      newtype Greeting = Greeting MisoString
        deriving (Eq)

      child
        :: Component () Greeting () ChildAction
      child = component () updateChild viewChild
        where
          viewChild
            :: ()
            -> Greeting
            -> ()
            -> View () () ChildAction
          viewChild _ (Greeting g) _ =
            H.div_ []
              [ text ("Hello, " <> g <> "!") ]

          updateChild
            :: ChildAction
            -> Effect () Greeting () ChildAction
          updateChild = \\case
            ReadGreeting -> do
              Greeting g <- getProps
              io_ (consoleLog g)

      -- Parent component: owns the greeting,
      -- passes it to the child as props
      parentComp :: App ParentModel ParentAction
      parentComp =
        component (ParentModel "World") noop viewParent
        where
          viewParent
            :: ()
            -> ()
            -> ParentModel
            -> View () ParentModel ParentAction
          viewParent _ _ (ParentModel g) =
            mountWithProps_ "child" (Greeting g) child

      newtype ParentModel = ParentModel MisoString
        deriving (Eq)

      data ChildAction = ReadGreeting
      data ParentAction
      """
    , ul
      [ [ "Props flow from parent to child explicitly via ", c "mountWithProps_", "; the child's context is the shared global context." ]
      , [ c "getProps", " inside the child's ", c "update", " yields a ", c "Greeting", ". The child only sees what the parent chose to share." ]
      , [ "The root ", c "App", " always has ", c "context ~ ()", " and ", c "props ~ ()", "; no plumbing is needed for ", c "startApp", "." ]
      , [ "The ", c "onPropsChanged", " hook dispatches an action with the previous and current props whenever they change." ]
      ]
    , h2 "Try it"
    , demo "Props flowing from a parent to a child" propsSource ("demo-props" +> propsDemo)
    ]
  }
-----------------------------------------------------------------------------
communication :: DocPage
communication = DocPage
  { pageSlug = "communication"
  , pageGroup = Concepts
  , pageTitle = "Communication"
  , pageBlurb = "Four ways components exchange data: props, context, the asynchronous mailbox and PubSub."
  , pageKeywords = [ "mail", "mailbox", "checkMail", "broadcast", "mailParent", "PubSub", "publish", "subscribe", "ComponentId" ]
  , pageBody =
    [ lead [ "miso provides four mechanisms for components to exchange data." ]
    , ul
      [ [ b "Props", " — synchronous, parent-to-child read-only data passed at mount time; see ", goto (docsPage "props") [ "Props" ], "." ]
      , [ b "Context", " — global data shared by the entire tree; any component can mutate it via ", c "modifyContext", " and opt in to re-renders with ", c "useContext", "; see ", goto (docsPage "context") [ "Context" ], "." ]
      , [ b "Mailbox", " — message passing via ", c "mail", ", ", c "broadcast", ", ", c "checkMail", "; any component can send a JSON ", c "Value", " to any other by ", c "ComponentId", "." ]
      , [ b "PubSub", " (", c "Miso.PubSub", ") — publish / subscribe for fan-out messaging across unrelated components." ]
      ]
    , h2 "The mailbox"
    , para [ "Every component has a mailbox — a slot that receives ", c "Value", " messages sent by other components. Messages are dispatched asynchronously via the event queue." ]
    , h3 "Sending"
    , api
      [ ("mail componentId msg", [ "send to a specific ", c "ComponentId", " (obtained via ", c "ask", " inside ", c "Effect", ")" ])
      , ("mailParent msg", [ "send to the direct parent" ])
      , ("mailChildren msg", [ "send to all immediate children" ])
      , ("mailAncestors msg", [ "walk up the hierarchy, delivering to every ancestor" ])
      , ("mailDescendants msg", [ "walk down the hierarchy, delivering to every descendant" ])
      , ("broadcast msg", [ "deliver to every mounted component except the sender" ])
      ]
    , h3 "Receiving with checkMail"
    , para [ "Wire up the ", c "mailbox", " field with ", c "checkMail", ", which handles JSON parsing and routes to success / error actions:" ]
    , hs """
      data Action
        = ReceivedMsg MyMsg
        | MailError   MisoString

      myComp
        :: Component context props model Action
      myComp = (component m u v)
        { mailbox = checkMail ReceivedMsg MailError }
      """
    , h3 "Looking up a ComponentId"
    , hs """
      update = \\case
        SendMsg targetId ->
          io_ (mail targetId ("hello" :: MisoString))
        GetMyId -> do
          info <- ask
          let myId = _componentInfoId info
          ...
      """
    , h2 "PubSub"
    , para [ c "Miso.PubSub", " provides topics: a component ", c "subscribe", "s to a topic (receiving messages as actions) and any component may ", c "publish", " to it. It is the right tool when the sender does not know who is listening." ]
    , hs """
      -- a typed topic; Note has ToJSON / FromJSON
      notifications :: Topic Note
      notifications = topic "notifications"

      update = \\case
        Init ->
          subscribe notifications Notified NotifyError
        Notify n ->
          io_ (publish notifications n)
        Notified n -> ...
          -- n :: Note
        NotifyError _ ->
          pure ()
      """
    , h2 "Try it"
    , demo "Mailbox and PubSub between siblings" mailSource ("demo-mail" +> mailDemo)
    , para [ "The publisher does two things on send: it ", c "publish", "es to a topic the subscriber joined on mount, and it ", c "mailParent", "s the parent, which relays with ", c "mailChildren", " — so each message arrives at the subscriber twice, once by each route." ]
    ]
  }
-----------------------------------------------------------------------------
subscriptions :: DocPage
subscriptions = DocPage
  { pageSlug = "subscriptions"
  , pageGroup = Concepts
  , pageTitle = "Subscriptions"
  , pageBlurb = "Long-running sources of actions: subs, startSub / stopSub and createSub."
  , pageKeywords = [ "Sub", "subs", "startSub", "stopSub", "createSub", "timer", "websocket", "onLineSub", "rAFSub" ]
  , pageBody =
    [ lead
      [ "A ", c "Sub", " is any long-running operation that is external to a component but that can write to the component's ", c "Sink", ". Subs come in two flavours: the static ", c "subs", " list and dynamic subs via ", c "startSub", " / ", c "stopSub", "." ]
    , hs """
      type Sub action = Sink action -> IO ()
      """
    , h2 "subs"
    , hs """
      main :: IO ()
      main = startApp defaultEvents app { subs = [ timerSub ] }

      timerSub :: Sub Action
      timerSub sink = forever $ threadDelay 100000 >> sink Log

      data Action = Log
      """
    , para [ "The ", c "subs", " field contains subs that exist for the lifetime of the component. When it unmounts, they are stopped and their resources finalised. Here is a real one from ", c "Miso.Subscription.OnLine", ":" ]
    , hs """
      onLineSub :: (Bool -> action) -> Sub action
      onLineSub f sink = createSub acquire release sink
        where
          release (cb1, cb2) = do
            windowRemoveEventListener "online"  cb1
            windowRemoveEventListener "offline" cb2
          acquire = do
            cb1 <- windowAddEventListener "online"
              (const $ sink (f True))
            cb2 <- windowAddEventListener "offline"
              (const $ sink (f False))
            pure (cb1, cb2)
      """
    , h2 "startSub / stopSub"
    , para [ "At times it is necessary to dynamically create a sub in response to an event (e.g. starting a ", c "Miso.WebSocket", " connection when a user logs in):" ]
    , hs """
      update = \\case
        StartTimer -> startSub ("timer" :: MisoString) timerSub
        StopTimer  -> stopSub "timer"
        Log        -> io_ (consoleLog "log")
        where
          timerSub :: Sub Action
          timerSub sink = forever $ threadDelay 100000 >> sink Log
      """
    , h2 "createSub"
    , para
      [ c "Miso.Subscription.Util.createSub", " builds a sub using the ", c "bracket", " pattern, ensuring listeners are unregistered when the component unmounts. Use it only when custom event listeners are required; the ", c "Miso.Subscription", " modules cover the usual suspects:" ]
    , api
      [ ("Miso.Subscription.Mouse", [ "global pointer position" ])
      , ("Miso.Subscription.Keyboard", [ "arrows, WASD, arbitrary key sets" ])
      , ("Miso.Subscription.Window", [ "resize, scroll, any window event via ", c "windowSub" ])
      , ("Miso.Subscription.History", [ c "uriSub", " / ", c "routerSub", " for navigation" ])
      , ("Miso.Subscription.RAF", [ c "rAFSub", " — ", c "requestAnimationFrame", " ticks for 60 FPS animation" ])
      , ("Miso.Subscription.OnLine", [ c "navigator.onLine" ])
      ]
    , h2 "Try it"
    , demo "A dynamic subscription with startSub / stopSub" subsSource ("demo-subs" +> subsDemo)
    ]
  }
-----------------------------------------------------------------------------
stateAndLenses :: DocPage
stateAndLenses = DocPage
  { pageSlug = "state-and-lenses"
  , pageGroup = Concepts
  , pageTitle = "State & lenses"
  , pageBlurb = "Miso.Lens: the bundled lens library, MonadState operators and three ways to generate lenses."
  , pageKeywords = [ "lens", "Miso.Lens", "makeLenses", "this", "+=", ".=", "%=", "Generic", "field", "OverloadedLabels" ]
  , pageBody =
    [ lead [ "miso bundles a lightweight lens library in ", c "Miso.Lens", " to minimise dependencies and payload size. Any lens library (optics, lens) also works — ", c "Miso.Lens", " is not required." ]
    , h2 "Basic operations"
    , hs """
      view l              -- read a field (MonadReader)
      set  l v            -- write a field
      over l f            -- modify a field
      r ^. l              -- infix read
      r & l .~ v          -- infix write
      """
    , h2 "MonadState operators (inside Effect)"
    , hs """
      l .= v    -- set a field
      l %= f    -- modify a field
      l += n    -- increment a numeric field
      l -= n    -- decrement
      l *= n    -- multiply
      """
    , h2 "this — the identity lens"
    , para [ "When the model ", em "is", " the field (e.g. the model is a plain ", c "Int", "), use ", c "this", ":" ]
    , hs """
      update = \\case
        Increment -> this += 1
        Decrement -> this -= 1
      """
    , h2 "Generating lenses"
    , para [ "Three approaches, pick one:" ]
    , h3 "Template Haskell"
    , hs """
      {-# LANGUAGE TemplateHaskell #-}
      import Miso.Lens.TH (makeLenses)

      data Model = Model { _count :: Int, _name :: MisoString }
      makeLenses ''Model

      update = \\case
        Increment -> count += 1
        Rename n  -> name .= n
      """
    , h3 "Generics"
    , para [ c "Miso.Lens.Generic.field", " / ", c "HasLens", " derive lenses at compile time using ", c "GHC.Generics", " — no splice required. Needs ", c "TypeApplications", " and, optionally, ", c "OverloadedLabels", " for the ", c "#field", " shorthand:" ]
    , hs """
      {-# LANGUAGE DataKinds, DeriveGeneric #-}
      {-# LANGUAGE OverloadedLabels        #-}
      {-# LANGUAGE TypeApplications        #-}
      import GHC.Generics (Generic)
      import Miso.Lens.Generic (field)

      data Model = Model
        { count :: Int
        , name  :: MisoString
        } deriving (Eq, Generic)

      update = \\case
        Increment -> field @"count" += 1
          -- via TypeApplications
        Rename n  -> #name .= n
          -- via OverloadedLabels
      """
    , h3 "Hand-written"
    , hs """
      name :: Lens Person MisoString
      name = lens _name $ \\p n -> p { _name = n }
      """
    , h2 "Try it"
    , demo "Updating a record model through lenses" lensSource ("demo-lens" +> lensDemo)
    ]
  }
-----------------------------------------------------------------------------
-- Platform
-----------------------------------------------------------------------------
routing :: DocPage
routing = DocPage
  { pageSlug = "routing"
  , pageGroup = Platform
  , pageTitle = "Routing"
  , pageBlurb = "Miso.Router: a reversible, type-safe client-side router derived from a sum type."
  , pageKeywords = [ "Router", "Miso.Router", "Capture", "QueryParam", "routerSub", "uriSub", "pushRoute", "href_", "prettyRoute", "URI" ]
  , pageBody =
    [ lead
      [ c "Miso.Router", " provides a reversible, type-safe client-side router. A ", c "Route", " type encodes URL structure; the ", c "Router", " class converts between routes and ", c "URI", " values in both directions. "
      , "Use it with ", c "routerSub", " or ", c "uriSub", " to react to browser navigation. This site's own routing is written with it." ]
    , h2 "Defining a Router with Generics"
    , para [ "Derive ", c "Router", " via ", c "GHC.Generics", " — constructor names become path segments (camel-case uses only the first hump; ", c "Index", " is the root). Use ", c "Capture", ", ", c "Path", ", ", c "QueryParam", " and ", c "QueryFlag", " as fields to describe the URL shape:" ]
    , hs """
      {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
      import GHC.Generics
      import Miso.Router

      data Route
        = Index
          -- "/"
        | About
          -- "/about"
        | Product
            (Capture "id" Int)
            (QueryParam "tab" MisoString)
          -- "/product/42?tab=info"
        deriving stock (Show, Eq, Generic)
        deriving anyclass Router
      """
    , para [ "The router is ", em "reversible", " — ", c "prettyRoute", " re-serialises any route back to a URL:" ]
    , hs """
      prettyRoute (Product (Capture 42) (QueryParam (Just "info")))
      -- "/product/42?tab=info"
      """
    , note [ "The generic parser is first-match, so declare more specific constructors before more general ones (e.g. ", c "DocsPage (Capture \"slug\" MisoString)", " before ", c "Docs", "). This site's ", c "Route", " does exactly that." ]
    , h2 "Defining a Router manually"
    , hs """
      data Route = Product Int

      instance Router Route where
        routeParser = routes
          [ Product <$> (path "product" *> capture) ]
        fromRoute (Product n) =
          [ toPath "product", toCapture n ]
      """
    , h2 "Subscribing to URI changes"
    , para [ c "routerSub", " listens to ", c "popstate", " events and delivers the parsed route (or a ", c "RoutingError", ") to ", c "update", ":" ]
    , hs """
      app = (component m u v) { subs = [ routerSub HandleRoute ] }

      update = \\case
        HandleRoute (Right Index) -> page .= HomePage
        HandleRoute (Right About) -> page .= AboutPage
        HandleRoute (Left _)      -> page .= NotFound
      """
    , para [ c "uriSub", " is the lower-level variant — it delivers the raw ", c "URI", " without parsing." ]
    , h2 "Navigating programmatically"
    , hs """
      pushURI    uri    -- push a raw URI onto the History stack
      pushRoute  route  -- push a typed route (serialised via Router)
      replaceURI uri    -- replace the current history entry
      back              -- go back one entry
      forward           -- go forward one entry
      """
    , h2 "Type-safe links in view"
    , para [ c "href_", " (from ", c "Miso.Router", ") produces a type-safe ", c "href", " from any route. Pair it with ", c "onClickPrevent", " to navigate client-side while keeping a real link for the browser and crawlers:" ]
    , hs """
      let product10 =
            Product (Capture 10) (QueryParam Nothing)
      in H.a_
           [ href_ product10
           , onClickPrevent (Go product10)
           ]
           [ "Go to product 10" ]
      """
    ]
  }
-----------------------------------------------------------------------------
htmlAndSsr :: DocPage
htmlAndSsr = DocPage
  { pageSlug = "html-and-prerendering"
  , pageGroup = Platform
  , pageTitle = "HTML & prerendering"
  , pageBlurb = "Rendering Views to HTML with ToHtml, static and dynamic prerendering, hydration with miso and hydrateModel."
  , pageKeywords = [ "ToHtml", "toHtml", "ssr", "server", "prerender", "hydration", "hydrateModel", "servant-miso-html", "static site" ]
  , pageBody =
    [ lead
      [ "miso's ", c "View", " type doubles as an HTML serialiser via the ", c "ToHtml", " class in ", c "Miso.Html.Render", ". Build a ", c "View", " with the normal DSL and render it to a lazy ", c "ByteString", " on the server — or, as this website does, at build time." ]
    , hs """
      class ToHtml a where
        toHtml :: a -> L.ByteString

      pageHtml :: L.ByteString
      pageHtml = toHtml $
        H.div_ [ HP.id_ "root" ] [ "Hello, world!" ]
      """
    , para [ "Instances are provided for ", c "View", " and ", c "[View]", ". Servant users can serve them directly with ", a "https://github.com/haskell-miso/servant-miso-html" "servant-miso-html", ", which provides an ", c "HTML", " content type for ", c "View", " and ", c "Component", " values:" ]
    , hs """
      import Servant.Miso.Html (HTML)

      type Home = "home"
        :> Get '[HTML]
             (Component context props model action)

      type About = "about"
        :> Get '[HTML] (View context model action)

      type Contact = "contact"
        :> Get '[HTML] [View context model action]
      """
    , h2 "Prerendering"
    , para
      [ "Prerendering is delivering HTML from a web server (or a static host) before the client loads and draws anything. It comes in two flavours: ", em "static", " prerendering assumes no model state needs to be shared between server and client; ", em "dynamic", " prerendering uses ", c "hydrateModel", " to share it." ]
    , h3 "Static prerendering"
    , para [ "miso provides ", c "prerender", " and ", c "miso", " for static prerendering. Any page can be generated from a ", c "View", " with ", c "toHtml", "; on the client, pass the matching component to ", c "miso", " (instead of ", c "startApp", ") so it hydrates the markup rather than redrawing:" ]
    , hs """
      main :: IO ()
      main = prerender defaultEvents $
        (component () noop view)
          { logLevel = DebugPrerender }
        where
          view _ _ () = "hello world"
      """
    , para [ "With the payload and HTML delivered together, the console shows:" ]
    , pre "[DEBUG_HYDRATE] Successfully prerendered page"
    , h3 "Dynamic prerendering"
    , para
      [ "Dynamic prerendering shares model state so the client hydrates from a meaningful initial state rather than a blank model. The ", c "-fssr", " flag must be enabled when compiling the server. "
      , "The ", c "hydrateModel", " field is ", c "Maybe (IO model)", ": when set, the action runs once at hydration time to produce the initial model; a typical pattern embeds the model as JSON in the response and reads it back through the JS DSL:" ]
    , hs """
      myComp :: App Model Action
      myComp =
        (component defaultModel updateModel viewModel)
          { hydrateModel = Just $ do
              val <- jsg "window" ! "__initialModel__"
              fromJSValUnchecked val
          }

      -- On the server, populate
      -- window.__initialModel__ alongside
      -- the rendered HTML:
      serverView
        :: context
        -> props
        -> Model
        -> View context Model Action
      serverView _ _ m =
        H.div_ []
          [ H.script_ []
              ("window.__initialModel__ = " <> encode m)
          , appView m
          ]
      """
    , para [ "When ", c "hydrateModel", " is ", c "Nothing", " the static ", c "model", " field is used instead — equivalent to static prerendering." ]
    , h2 "How this site does it"
    , para
      [ "haskell-miso.org has no server. A small ", c "prerender", " executable, compiled with vanilla GHC and ", c "-fssr", ", walks every route, calls ", c "toHtml", " on the root component and writes ", c "index.html", " files into ", c "public/", ". "
      , "The WASM bundle then hydrates whichever page was loaded with ", c "misoWithContext", " and takes over navigation with ", c "Miso.Router", "." ]
    ]
  }
-----------------------------------------------------------------------------
javascriptEdsl :: DocPage
javascriptEdsl = DocPage
  { pageSlug = "javascript-edsl"
  , pageGroup = Platform
  , pageTitle = "JavaScript EDSL & FFI"
  , pageBlurb = "Miso.DSL for jsaddle-style interop, marshalling with ToJSVal / FromJSVal, and the js quasi-quoter."
  , pageKeywords = [ "DSL", "jsg", "!", "#", "ToJSVal", "FromJSVal", "QuasiQuotes", "js", "inline javascript", "FFI", "Miso.FFI" ]
  , pageBody =
    [ lead [ c "Miso.DSL", " provides a JavaScript DSL inspired by ", a "https://hackage.haskell.org/package/jsaddle" "jsaddle", " for interacting with the browser from Haskell. It works identically on the WASM and JS backends." ]
    , h2 "Key operators"
    , api
      [ ("(!)",  [ "property access: ", c "obj ! \"key\"", " reads ", c "obj.key" ])
      , ("(#)",  [ "method call: ", c "obj # \"method\" $ args", " calls ", c "obj.method(args)" ])
      , ("jsg",  [ "access a global JS variable by name" ])
      , ("jsgf", [ "call a global JS function by name with arguments" ])
      ]
    , hs """
      -- Read document.body.children.length
      document <- jsg "document"
      len :: Int <- fromJSValUnchecked =<<
        (document ! "body" ! "children" ! "length")

      -- Call console.log("hello")
      console <- jsg "console"
      console # "log" $ [ "hello" :: MisoString ]
      """
    , h2 "Marshalling"
    , para [ c "ToJSVal", " converts Haskell values to ", c "JSVal", " for passing into JavaScript; ", c "FromJSVal", " converts back. ", c "fromJSValUnchecked", " throws on failure; ", c "fromJSVal", " is the safe ", c "Maybe", " variant." ]
    , h2 "Inline JavaScript (QuasiQuotation)"
    , para
      [ c "Miso.FFI.QQ", " provides the ", c "js", " quasi-quoter for embedding JavaScript directly in Haskell source. Any Haskell binding in scope can be interpolated with ", c "${varName}", " — miso uses the binding's ", c "ToJSVal", " instance to marshal it at runtime." ]
    , hs """
      {-# LANGUAGE QuasiQuotes #-}
      import Miso.FFI.QQ (js)

      update
        :: Action
        -> Effect context props model Action
      update = \\case
        Log msg -> io_ [js| console.log(${msg}) |]

      data Action = Log MisoString
      """
    , h3 "Returning values from JavaScript"
    , para [ "The return type is inferred from the call site via ", c "FromJSVal", ". Use an explicit annotation or a ", c "do", "-binding to drive inference:" ]
    , hs """
      factorial :: Int -> IO Int
      factorial n = [js|
        let x = 1;
        for (let i = 1; i <= ${n}; i++) { x *= i; }
        return x;
      |]
      """
    , para [ "Haskell variables referenced inside the quoter must be in scope at the splice site; the compiler reports an error if a ", c "${name}", " has no binding." ]
    , h2 "Miso.FFI"
    , para [ c "Miso.FFI", " collects typed wrappers for common browser calls: ", c "consoleLog", ", ", c "getElementById", ", ", c "focus", ", ", c "windowInnerWidth", ", ", c "addEventListener", ", callbacks (", c "asyncCallback", "), ", c "fetch", ", ", c "scrollIntoView", ", ", c "requestFullscreen", " and more. Mutable JS collections live in ", c "Miso.Data.Array", ", ", c "Miso.Data.Map", " and ", c "Miso.Data.Set", "." ]
    ]
  }
-----------------------------------------------------------------------------
canvas :: DocPage
canvas = DocPage
  { pageSlug = "canvas"
  , pageGroup = Platform
  , pageTitle = "Canvas"
  , pageBlurb = "2D canvas support: the Canvas monad, embedding a canvas in the view, drawing commands and animation loops."
  , pageKeywords = [ "canvas", "Miso.Canvas", "fillRect", "arc", "rAFSub", "requestAnimationFrame", "three-miso", "game" ]
  , pageBody =
    [ lead [ "miso has full 2D and 3D canvas support via ", c "Miso.Canvas", ". See also the ", a "https://github.com/haskell-miso/canvas2d" "canvas2d", " example and ", a "https://github.com/haskell-miso/three-miso" "three-miso", " for Three.js integration." ]
    , h2 "The Canvas monad"
    , para [ "Drawing commands run in the ", c "Canvas", " monad, a ", c "ReaderT", " over the raw ", c "CanvasContext2D", ":" ]
    , hs """
      type Canvas a = ReaderT CanvasContext2D IO a
      """
    , h2 "Embedding a canvas in the view"
    , para [ "Use the ", c "canvas", " smart constructor. It takes an ", em "init", " callback (runs once on mount, returns state) and a ", em "draw", " callback (runs on every render with the current state). Capture the current model in the draw closure:" ]
    , hs """
      canvas
        [ HP.width_ "800", HP.height_ "480" ]
        (\\_ -> pure ())
        -- init: called once on canvas initialisation
        (\\() -> drawScene myModel)
        -- draw: called on each diff
      """
    , para [ c "canvas_", " is the variant that threads no init state at all." ]
    , h2 "Drawing commands"
    , hs """
      drawScene :: Model -> Canvas ()
      drawScene model = do
        clearRect (0, 0, 800, 480)
        fillStyle (color (RGB 30 144 255))
        beginPath ()
        arc (400, 240, 50, 0, 2 * pi)
        fill ()
        font "24px sans-serif"
        fillText ("Score: " <> ms (score model), 10, 30)
      """
    , para
      [ "Available primitives include ", c "clearRect", ", ", c "fillRect", ", ", c "strokeRect", ", ", c "beginPath", ", ", c "closePath", ", ", c "moveTo", ", ", c "lineTo", ", ", c "arc", ", ", c "arcTo", ", ", c "fill", ", ", c "stroke", ", ", c "fillText", ", ", c "drawImage", ". Style setters: ", c "fillStyle", ", ", c "strokeStyle", ", ", c "lineWidth", ", ", c "font", "." ]
    , warn [ c "Miso.Canvas.color", " (builds a ", c "StyleArg", ") and ", c "Miso.CSS.color", " collide when both modules are imported unqualified. Qualify ", c "Miso.CSS", " when also using ", c "Miso.Canvas", "." ]
    , h2 "Animation loop"
    , para [ "For smooth 60 FPS canvas animations, use ", c "rAFSub", " from ", c "Miso.Subscription.RAF", " instead of a manual ", c "threadDelay", " loop. It hooks into ", c "requestAnimationFrame", " and delivers a ", c "DOMHighResTimeStamp", " each frame:" ]
    , hs """
      data Action = Tick Double

      main :: IO ()
      main = startApp defaultEvents
        comp { subs = [ rAFSub Tick ] }
      """
    , h2 "Try it"
    , demo "Orbits: rAFSub driving the Canvas monad" canvasSource ("demo-canvas" +> canvasDemo)
    ]
  }
-----------------------------------------------------------------------------
misoString :: DocPage
misoString = DocPage
  { pageSlug = "misostring"
  , pageGroup = Platform
  , pageTitle = "MisoString"
  , pageBlurb = "miso's canonical string type: JSString on the client, Text on the server, and how to convert."
  , pageKeywords = [ "MisoString", "ms", "toMisoString", "fromMisoString", "JSString", "Text", "misoString", "QuasiQuoter" ]
  , pageBody =
    [ lead [ c "MisoString", " is miso's canonical string type, chosen to minimise copying between the Haskell and JavaScript heaps." ]
    , ul
      [ [ b "JS / WASM backends", " — ", c "MisoString", " is ", c "JSString", ", a direct reference to a JavaScript string; no marshalling cost when passing to the DOM or FFI." ]
      , [ b "Server / vanilla GHC", " (", c "-fssr", ") — ", c "MisoString", " is ", c "Data.Text", "." ]
      ]
    , para [ "Use ", c "MisoString", " anywhere you would otherwise reach for ", c "String", " or ", c "Text", " in a miso application. See ", c "Miso.String", " for the full API (it re-exports the underlying module wholesale)." ]
    , h2 "Converting to MisoString"
    , para [ c "ms", " (shorthand for ", c "toMisoString", ") converts any type with a ", c "ToMisoString", " instance:" ]
    , hs """
      ms "hello"          -- String -> MisoString
      ms (42 :: Int)      -- Int    -> MisoString
      ms (3.14 :: Double) -- Double -> MisoString
      ms myText           -- Text   -> MisoString
      """
    , para [ "Instances exist for ", c "String", ", strict and lazy ", c "Text", ", ", c "ByteString", ", ", c "Int", ", ", c "Word", ", ", c "Double", ", ", c "Float", " and ", c "Char", "." ]
    , h2 "Converting from MisoString"
    , para [ c "fromMisoString", " parses back into another type (throwing on failure); ", c "fromMisoStringEither", " is the safe variant:" ]
    , hs """
      fromMisoString "42"     :: Int     -- 42
      fromMisoString "3.14"   :: Double  -- 3.14
      fromMisoStringEither s  :: Either String Int
      """
    , h2 "Multiline literals"
    , para [ "GHC's ", c "MultilineStrings", " extension (9.12+) works directly with ", c "MisoString", " — this site uses it for all of its code samples:" ]
    , hs """
      {-# LANGUAGE MultilineStrings #-}

      snippet :: MisoString
      snippet = \"\"\"
        line one
        line two
        \"\"\"
      """
    , para [ "On older compilers, ", c "Miso.String.QQ", " provides the ", c "misoString", " quasi-quoter for the same purpose." ]
    ]
  }
-----------------------------------------------------------------------------
json :: DocPage
json = DocPage
  { pageSlug = "json"
  , pageGroup = Platform
  , pageTitle = "JSON"
  , pageBlurb = "Miso.JSON: a microaeson-style JSON library specialised to MisoString, with generic ToJSON / FromJSON."
  , pageKeywords = [ "JSON", "Miso.JSON", "ToJSON", "FromJSON", "encode", "decode", "withObject", ".:", "aeson", "miso-aeson" ]
  , pageBody =
    [ lead
      [ c "Miso.JSON", " is a ", a "https://hackage.haskell.org/package/microaeson" "microaeson", "-inspired JSON library specialised to ", c "MisoString", ". On the JS / WASM backends it delegates encoding and decoding to the JavaScript runtime (", c "JSON.stringify", " / ", c "JSON.parse", ") for performance; on the server (", c "ssr", ") it uses a pure Haskell implementation. "
      , "It is used internally by ", c "Miso.Event.Decoder", ", ", c "Miso.Fetch", " and ", c "Miso.WebSocket", "." ]
    , h2 "Value"
    , hs """
      data Value
        = Number Double
        | Bool   Bool
        | String MisoString
        | Array  [Value]
        | Object Object
        | Null
      """
    , h2 "Encoding and decoding"
    , hs """
      encode value
      -- uses the JS runtime on the client,
      -- pure on the server

      encodePure value
      -- always the pure Haskell implementation

      decode s            :: Maybe a
      eitherDecode s      :: Either MisoString a
      """
    , h2 "ToJSON / FromJSON"
    , para [ "Derive instances via ", c "GHC.Generics", ":" ]
    , hs """
      {-# LANGUAGE DeriveGeneric #-}
      import GHC.Generics
      import Miso.JSON

      data User = User
        { name :: MisoString
        , age  :: Int
        } deriving (Generic)

      instance ToJSON User
      instance FromJSON User
      """
    , para [ "Use ", c "genericToJSON", " / ", c "genericParseJSON", " with ", c "Options", " to customise field and constructor names; ", c "camelTo2", " converts ", c "camelCase", " to ", c "snake_case", ":" ]
    , hs """
      instance ToJSON User where
        toJSON = genericToJSON defaultOptions
          { fieldLabelModifier = camelTo2 '_' }
      """
    , h2 "Building and parsing objects"
    , hs """
      -- Build
      object
        [ "name" .= ms "Alice"
        , "age" .= (30 :: Int)
        ]

      -- Parse (inside a withObject
      -- callback or event decoder)
      withObject "User" $ \\o -> User
        <$> o .:  "name"
            -- required field
        <*> o .:  "age"

      o .:? "nickname"
      -- optional field → Maybe a

      o .:! "nickname"
      -- optional field, explicit null → Maybe a

      p .!= "anon"
      -- default for a Maybe parser
      """
    , h2 "Pretty printing"
    , hs """
      encodePretty value
      -- indented with defConfig (2-space indent)

      encodePretty' config value
      -- custom Config
      """
    , h2 "Try it"
    , demo "Round-tripping a record with generic instances" jsonSource ("demo-json" +> jsonDemo)
    , h2 "miso-aeson"
    , para [ "Prefer aeson? The ", a "https://github.com/haskell-miso/miso-aeson" "miso-aeson", " package bridges aeson's ", c "ToJSON", " / ", c "FromJSON", " instances with miso's event decoder and fetch API, so existing instances work without rewriting." ]
    , h2 "aeson polyfill"
    , para
      [ H.span_ [ P.class_ "soon-badge" ] [ "Coming soon" ]
      , " — an ", b "aeson polyfill", " behind an ", c "aeson", " cabal flag (", c "-faeson", "): building miso with it defines every operator and class exported by ", c "Miso.JSON", " (", c "ToJSON", ", ", c "FromJSON", ", ", c ".:", ", ", c ".=", ", ", c "withObject", ", ", c "encode", ", ", c "decode", ", …) in terms of ", a "https://hackage.haskell.org/package/aeson" "Data.Aeson", ". "
      , "Existing aeson code — and its instances — will work with miso unchanged, with no bridge package and no import churn: keep importing ", c "Miso.JSON", " and flip the flag." ]
    ]
  }
-----------------------------------------------------------------------------
styles :: DocPage
styles = DocPage
  { pageSlug = "styles"
  , pageGroup = Platform
  , pageTitle = "Styles"
  , pageBlurb = "Three ways to style: the Miso.CSS DSL, inline strings and external stylesheets."
  , pageKeywords = [ "css", "Miso.CSS", "style_", "styleInline_", "stylesheet", "tailwind", "color", "=:" ]
  , pageBody =
    [ lead [ "miso does not prescribe a single CSS strategy. Three approaches work out of the box." ]
    , h2 "1. Structured DSL (Miso.CSS)"
    , para [ c "style_", " takes a list of ", c "Style", " values (", c "(MisoString, MisoString)", " pairs). miso manages the individual properties on the ", c "DOMRef", ", merging and diffing them efficiently:" ]
    , hs """
      import qualified Miso.CSS as CSS
      import           Miso.CSS.Color (Color (..))

      H.div_
        [ CSS.style_
            [ CSS.display "flex"
            , CSS.flexDirection "column"
            , CSS.backgroundColor (RGB 30 30 30)
            , CSS.color (RGB 255 255 255)
            ]
        ]
        []
      """
    , para [ "Custom properties can be constructed with the ", c "=:", " operator (re-exported from ", c "Miso.Util", "):" ]
    , hs """
      "user-select" =: "none"
      """
    , h2 "2. Inline string (styleInline_)"
    , para [ "For simple or dynamic style strings, ", c "styleInline_", " sets the element's ", c "style", " attribute as a raw string:" ]
    , hs """
      CSS.styleInline_ "display:flex; gap:8px; padding:16px"
      """
    , h2 "3. External stylesheets"
    , para
      [ "Link external CSS files from the ", c "<head>", " via the ", c "styles", " field on ", c "Component", " (development only — see ", goto (docsPage "development") [ "Development" ], "), or include them in your HTML template directly. "
      , "This is the most common approach for production apps using Tailwind, Bootstrap, etc. See ", a "https://ui.haskell-miso.org" "miso-ui", " for a larger example." ]
    , h2 "Stylesheets from Haskell"
    , para [ c "Miso.CSS", " can also express whole stylesheets (", c "sheet_", ", ", c "selector_", ") and the ", c "Sheet", " constructor of ", c "CSS", " lets you attach one to a component during development. Colours come from ", c "Miso.CSS.Color", " (", c "RGB", ", ", c "RGBA", ", ", c "HSL", ", named colours)." ]
    ]
  }
-----------------------------------------------------------------------------
development :: DocPage
development = DocPage
  { pageSlug = "development"
  , pageGroup = Platform
  , pageTitle = "Development & debugging"
  , pageBlurb = "Dev-time styles and scripts, live reloading in the WASM browser GHCi, and the debug log levels."
  , pageKeywords = [ "development", "live", "reload", "ghciwatch", "hot reload", "DebugAll", "DebugEvents", "DebugHydrate", "logLevel", "scripts", "styles" ]
  , pageBody =
    [ lead [ "When developing interactively it is possible to append ", c "styles", " and ", c "scripts", " to the ", c "<head>", " of the page when a component mounts. This is a convenience meant only for development — guard it behind a flag." ]
    , hs """
      main :: IO ()
      main = startApp defaultEvents app
       where
         app = counter
      #ifdef INTERACTIVE
           { scripts =
               [ Src
                   "https://code.jquery.com/jquery.min.js"
                   (False :: CacheBust)
               ]
           , styles =
               [ Href
                   "https://cdn.example.com/bootstrap.min.css"
                   (False :: CacheBust)
               ]
           }
      #endif
      """
    , h2 "Live reloading"
    , para
      [ "The WASM backend ships a browser-mode GHCi. Run ", c "make watch", " in the sample app: ", c "ghciwatch", " reloads on save and calls ", c ":main", ", and ", c "Miso.Reload.live", " (used instead of ", c "startApp", " under the ", c "INTERACTIVE", " flag) clears the page and remounts. "
      , c "liveWithContext", " does the same for apps that seed a context; when a component's model can be recovered by key it is preserved across reloads." ]
    , sh """
      $ make watch
      # ghciwatch \\
      #   --after-startup-ghci :main \\
      #   --after-reload-ghci :main \\
      #   --watch *.hs \\
      #   --command 'wasm32-wasi-cabal repl app \\
      #     -finteractive \\
      #     --repl-options="-fghci-browser \\
      #       -fghci-browser-port=8080"'
      """
    , h2 "Debugging"
    , para
      [ "Sometimes things go wrong. Using ", c "onClick", " without listening for the ", c "click", " event is a common error that cannot be caught statically. Enable ", c "DebugAll", " to detect these; currently debugging event delegation and page hydration is supported." ]
    , api
      [ ("DebugHydrate", [ "warn if the structure or properties of the DOM and the virtual DOM differ during prerendering" ])
      , ("DebugEvents",  [ "warn if an event cannot be routed to the handler that raised it, or a handler is used for an event nobody listens for" ])
      , ("DebugAll",     [ "both of the above" ])
      ]
    , hs """
      counter { logLevel = DebugAll }
      """
    ]
  }
-----------------------------------------------------------------------------
internals :: DocPage
internals = DocPage
  { pageSlug = "internals"
  , pageGroup = Platform
  , pageTitle = "Internals"
  , pageBlurb = "How the event queue, scheduler, Waiter, delegation and diffing fit together."
  , pageKeywords = [ "internals", "scheduler", "event queue", "Waiter", "diff", "Miso.Diff", "Miso.Delegate", "runtime" ]
  , pageBody =
    [ lead [ "Internally miso uses a global event queue and a scheduler to process all events raised by components throughout the lifetime of an application. Events are processed in FIFO order, batched by the component that raised them." ]
    , ul
      [ [ b "Event queue", " — all actions dispatched via a ", c "Sink", " (from event handlers, subscriptions or ", c "io", " callbacks) are enqueued and drained by the scheduler." ]
      , [ b "Scheduler", " — pulls actions off the queue one batch at a time, runs ", c "update", " for each, collects the resulting IO and executes it. Rendering (VDOM diff + patch) is triggered after each batch." ]
      , [ b "Waiter", " — a synchronisation primitive (", c "Miso.Concurrent", ") that blocks the scheduler thread until new work arrives, avoiding busy-waiting." ]
      , [ b "Event delegation", " — rather than attaching listeners to individual nodes, miso attaches a single capture and a single bubble listener to ", c "<body>", ". Incoming events are routed through the virtual DOM tree to the matching handler, minimising listener churn when the VDOM is patched." ]
      , [ b "VDOM diffing", " — the algorithm in ", c "Miso.Diff", " compares old and new ", c "View", " trees and emits the minimal set of DOM mutations. Keyed children significantly speed up child-list reconciliation." ]
      ]
    , h2 "The TypeScript runtime"
    , para
      [ "The DOM operations live in a small TypeScript runtime (", c "ts/", " in the repository, bundled into ", c "js/miso.js", "). It implements the drawing context interface that ", c "renderApp", " lets you swap — the native backend provides an alternative implementation on top of Lynx's element API. "
      , "The runtime has a bun test suite with high coverage; ", c "npm test", " runs it." ]
    ]
  }
-----------------------------------------------------------------------------
