-----------------------------------------------------------------------------
-- | Documentation for the native (Lynx) backend. The prose follows the
-- module documentation of "Miso.Native", one section per page.
module Site.Docs.Native
  ( nativePages
  ) where
-----------------------------------------------------------------------------
import Miso ((+>))
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
-----------------------------------------------------------------------------
import Site.Anatomy (anatomyScene)
import Site.SeqDiagram
-----------------------------------------------------------------------------
import Site.Docs.Types
import Site.Prose
import Site.Route
-----------------------------------------------------------------------------
nativePages :: [DocPage]
nativePages =
  [ overview
  , dualThread
  , staticMounting
  , effectsAndThreads
  , mainThreadEvents
  , mainThreadState
  , platformApis
  , minimalComponent
  ]
-----------------------------------------------------------------------------
overview :: DocPage
overview = DocPage
  { pageSlug = "overview"
  , pageGroup = Native
  , pageTitle = "miso native"
  , pageBlurb = "Target iOS and Android by driving the Lynx runtime with the same components you write for the web."
  , pageKeywords = [ "native", "mobile", "lynx", "ios", "android", "Miso.Native", "-fnative", "miso-lynx" ]
  , pageBody =
    [ lead
      [ "Your program runs in two JS interpreters at once: the ", em "main thread", " (MTS), which owns the drawing facilities, and the ", em "background thread", " (BTS), which owns the ", a "https://lynxjs.org/guide/use-native-modules.html" "native modules", ". "
      , "miso runs on both and keeps them in sync. See ", goto (nativePage "dual-thread") [ "the dual-thread architecture" ], "." ]
    , para
      [ "Miso.Native targets ", b "native mobile devices", " by driving the ", a "https://lynxjs.org" "Lynx", " runtime instead of the browser DOM. "
      , "The same MVU programming model, Component API, event delegation, and virtual-DOM diffing carry over unchanged from the web; only the element vocabulary differs (view_ and text_ instead of div_ and span_), "
      , "and rendering goes through Lynx's ", a "https://lynxjs.org/api/engine/element-api" "element PAPI", " instead of a browser DOM." ]
    , para [ "This module is the native analogue of the ", c "miso", " / ", c "startApp", " entry points: ", c "native", " (and ", c "nativeWithContext", ") boot a root component onto the Lynx runtime." ]
    , "anatomy" +> anatomyScene
    , h2 "Enabling native"
    , para
      [ "The native backend is gated behind the ", c "native", " cabal flag. It must be enabled to bring ", c "Miso.Native", " and the ", c "Miso.Native.*", " element / event / FFI modules into scope (build with ", c "-fnative", "). "
      , "Web / WASM builds are unaffected — all cross-thread machinery lives behind the ", c "NATIVE", " CPP guard." ]
    , sh """
      $ nix develop github:dmjio/miso#native
      $ cabal build -f native \\
          --with-compiler=javascript-unknown-ghcjs-ghc \\
          --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg
      """
    , para [ "The JavaScript output is bundled for Lynx with ", a "https://lynxjs.org/rspeedy" "rspeedy", " and loaded by the Lynx Explorer app or your own iOS / Android shell. The ", a "https://github.com/haskell-miso/miso-lynx" "miso-lynx", " repository has the tooling, and ", a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", " a gallery of native components." ]
    , h2 "Building a bundle with Nix"
    , para
      [ "miso's flake exports everything needed to produce a ", c "main.lynx.bundle", " reproducibly. ", c "miso.lib.${system}.ghcNative", " is the GHC-JS package set with ", c "miso-native", " preinstalled — ", c "callCabal2nix", " your own app into it — and ", c "pkgs.mkLynxBundle", " (also exported as ", c "miso.lib.${system}.mkLynxBundle", ") turns that JS derivation into a Lynx bundle:" ]
    , hs """
      {
        inputs.miso.url = "github:dmjio/miso";

        outputs = { miso, ... }:
          let system = "x86_64-linux";
              # or aarch64-darwin, …
              lib = miso.lib.${system};
          in {
            packages.${system}.bundle = lib.mkLynxBundle {
              name = "my-app-bundle";
              jsDrv =
                lib.ghcNative.callCabal2nix "my-app" ./. { };
              exeName = "my-app";
              styles = ./styles.css;
            };
          };
      }
      """
    , para
      [ c "nix build .#bundle", " then leaves ", c "result/main.lynx.bundle", " ready to load in the Lynx Explorer app. For interactive work, ", c "nix develop github:dmjio/miso#native", " provides the GHC-JS compiler (the ", c "ghcNative", " package set), ", c "rspeedy", " and ", c "bun", "." ]
    , para
      [ "To ship a standalone iOS app, the ", a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", " example includes an ", c "ios/", " folder: copy the bundle the Nix derivation produced (the ", c "/nix/store", " output symlinked at ", c "./result", ") into the assets in ", c "ios/", " and build with Xcode. See ", a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", " for more information." ]
    , h2 "Gallery"
    , para
      [ "The UI stack miso drives natively, in the flesh — screens from Lynx's example apps. The "
      , a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", " repository builds a gallery of these components in miso." ]
    , H.figure_ [ P.class_ "lynx-gallery" ]
        [ H.div_ [ P.class_ "lynx-gallery-grid" ]
            [ H.img_ [ P.src_ "/assets/lynx/lynx-demo-card.webp", P.alt_ "An image card with a like button, built on Lynx", P.loading_ "lazy" ]
            , H.img_ [ P.src_ "/assets/lynx/lynx-demo-gallery.webp", P.alt_ "A two-column waterfall gallery built on Lynx, scrolling", P.loading_ "lazy" ]
            , H.img_ [ P.src_ "/assets/lynx/lynx-demo-swiper.webp", P.alt_ "A swiper carousel with a page indicator, built on Lynx", P.loading_ "lazy" ]
            ]
        , H.figcaption_ []
            [ "Screens from the ", a "https://lynxjs.org" "Lynx", " examples (CC BY 4.0) — see "
            , a "https://github.com/haskell-miso/miso-lynx-gallery" "haskell-miso/miso-lynx-gallery", " for the miso versions." ]
        ]
    , h2 "In this section"
    , ul
      [ [ goto (nativePage "dual-thread") [ "The dual-thread architecture" ], " — BTS vs MTS, what crosses the boundary, the instant first frame." ]
      , [ goto (nativePage "static-mounting") [ "Static mounting" ], " — why constructors cross threads as static pointers." ]
      , [ goto (nativePage "effects-and-threads") [ "Effects, subscriptions and threads" ], " — ", c "runOnBG", ", ", c "runOnMain", " and guarding subs." ]
      , [ goto (nativePage "main-thread-events") [ "Main-thread events" ], " — low-latency ", c "*Main", " handlers." ]
      , [ goto (nativePage "main-thread-state") [ "Main-thread-local state" ], " — ", c "MainThreadRef", " and ", c "eachFrame", "." ]
      , [ goto (nativePage "platform-apis") [ "Platform APIs" ], " — which APIs exist on which thread." ]
      , [ goto (nativePage "minimal-component") [ "A minimal native component" ], " — the counter, natively." ]
      ]
    ]
  }
-----------------------------------------------------------------------------
dualThread :: DocPage
dualThread = DocPage
  { pageSlug = "dual-thread"
  , pageGroup = Native
  , pageTitle = "The dual-thread architecture"
  , pageBlurb = "Lynx runs your app on a background thread and a main thread; here is how miso maps onto both."
  , pageKeywords = [ "BTS", "MTS", "background thread", "main thread", "mts", "bts", "web", "first frame", "patches", "nodeId" ]
  , pageBody =
    [ lead [ "Lynx runs your application across ", b "two threads", ", and miso maps onto both:" ]
    , ul
      [ [ b "BTS", " — the ", em "background thread", " (\"background thread script\"). This is where your application ", em "logic", " lives. Everything runs here ", b "by default", ": the ", c "update", " function, event handling, ", c "Effect", " scheduling and ", em "all", " virtual-DOM diffing." ]
      , [ b "MTS", " — the ", em "main thread", " (\"main thread script\"). This thread owns the actual element tree and ", em "rendering", ". It is where the pixels land. It is also available as a low-latency escape hatch for performance-critical event handling (see ", goto (nativePage "main-thread-events") [ "Main-thread events" ], ")." ]
      ]
    , para
      [ "The ", b "same Haskell bundle runs on both threads", "; the native runtime selects the BTS or MTS drawing context per thread from a global flag, so there is no renderer to register — ", c "native", " starts the app directly. "
      , "The guiding principle: ", b "everything originates on the BTS", ". The MTS is a rendering surface that the BTS drives across the thread boundary." ]
    , h2 "Knowing which thread you are on"
    , para
      [ "Lynx builds the bundle with rspeedy, which compiles the sources ", em "twice", ", once per thread, inlining a compile-time constant (", c "__BACKGROUND__", ") that distinguishes the two. That constant surfaces in Haskell as three top-level ", c "Bool", "s in ", c "Miso.Runtime", " (re-exported from ", c "Miso", "):" ]
    , api
      [ ("mts", [ c "True", " when this execution context is the Lynx main thread." ])
      , ("bts", [ c "True", " when this context is the Lynx background thread." ])
      , ("web", [ c "True", " for a plain web / WASM build (neither Lynx thread)." ])
      ]
    , para [ "Exactly one is ", c "True", " and the value is invariant for the lifetime of a JS context, so the runtime computes it once and caches it. Runtime code branches on ", c "mts", " / ", c "bts", " to decide where work runs (e.g. the scheduler suppresses the paint step on the MTS, which keeps only a read-only model replica)." ]
    , h2 "What crosses the thread boundary, and how"
    , para [ "Because logic (BTS) and rendering (MTS) live on different threads, miso synchronises them by shipping messages across the boundary. This is largely invisible, but understanding it explains the API constraints below." ]
    , ul
      [ [ b "Initial draw", " — the very first draw happens ", b "on the MTS itself", ", and does ", b "not", " rely on the BTS diffing a tree and transferring patches. The root component is booted from a ", c "StaticPtr", " (via ", c "native", "), so the MTS reconstructs it from the pointer's ", c "StaticKey", " alone and renders the first frame locally (Lynx's instant first frame). Only ", em "after", " this does the cross-thread patch protocol take over: every subsequent diff runs on the BTS and ships patches to the MTS." ]
      , [ b "Subsequent component mounts", " — when the BTS ", c "view", " mounts a child, that mount is synchronised to the MTS asynchronously using ", em "static mounting", ": the child is wrapped in a ", c "static", " pointer so only its ", c "StaticKey", " — not a closure — crosses the boundary. See ", goto (nativePage "static-mounting") [ "Static mounting" ], "." ]
      , [ b "State synchronisation", " — the BTS owns the shared model and ships it to the MTS as it changes (JSON-serialised, hence the ", c "ToJSON", " / ", c "FromJSON", " constraints on native mounting combinators), so main-thread ", c "*MainWith", " handlers observe an eventually-consistent copy. A child's initial props ride the static-mount payload, but props and the global context are ", b "not", " re-synced on later changes: after the first frame they stay background-thread-only (matching ReactLynx)." ]
      , [ b "Events", " — events raised on the MTS are, by default, forwarded to the BTS where ", c "update", " runs. Cross-thread handlers are carried as an ", c "EventHandler", " embedded with ", c "event . static (…)", " so the peer thread can rebuild the handler from its ", c "StaticKey", "." ]
      ]
    , h2 "The lifecycle, in sequence"
    , figure
      [ seqDiagram [ "MTS", "BTS", "miso runtime" ]
          [ Self 0 False "initial draw (instant first frame)"
          , Self 0 False "register BTS + MTS events"
          , Arrow 2 1 False "initialize component tree"
          , Arrow 1 0 False "component mount / model hydration"
          , Self 0 False "receive / process MTS event"
          , Self 0 False "receive BTS event"
          , Arrow 0 1 False "forward BTS event (e.g. tap)"
          , Arrow 1 2 False "update state / perform effects"
          , Self 2 False "native module access"
          , Arrow 2 1 False "diff + create patches"
          , Arrow 1 0 False "forward patches"
          , Self 0 False "element update"
          , Arrow 0 1 True "runOnBG action — update runs on the BTS"
          , Arrow 1 0 True "runOnMain action — update runs on the MTS"
          , Self 0 True "onMain handler — handled on the MTS, no round-trip"
          , Arrow 0 1 True "on handler — event forwarded, update on the BTS"
          ]
      ]
      [ "The dual-thread ", c "Component", " initialization and lifecycle. The ", c "runOnBG", " / ", c "runOnMain"
      , " effects and the ", c "on", " / ", c "onMain", " handlers (highlighted) are how work hops between the threads." ]
    , h2 "First-frame rendering (instant first frame)"
    , para
      [ "The MTS painting frame one itself is Lynx's ", em "instant first frame", ": the user sees UI without waiting for a background render and patch round-trip. Meanwhile the BTS boots the ", em "same", " root and builds the identical virtual-DOM tree in lockstep — with ", b "deterministic nodeId parity", ", so both threads address the same elements — but ", b "suppresses its own create-patches", " for that first frame, since the MTS already painted them. "
      , "A single global ", c "initialDraw", " latch governs this on both threads; ", c "native", " clears it once the whole root mount has finished." ]
    , para
      [ "After that handover the responsibilities are fixed, mirroring ReactLynx: the ", b "BTS is the sole diff / paint authority", " — it runs ", c "update", ", diffs and ships patches — while the ", b "MTS only applies those patches", " (and runs main-thread scripts / handlers). The MTS never diffs or repaints from the scheduler again; this is why the shared model is BTS-owned and why nothing you do on the MTS should try to redraw declaratively." ]
    ]
  }
-----------------------------------------------------------------------------
staticMounting :: DocPage
staticMounting = DocPage
  { pageSlug = "static-mounting"
  , pageGroup = Native
  , pageTitle = "Static mounting"
  , pageBlurb = "Component constructors cross the thread boundary as static pointers: mountStatic_, vcomp and VCompStatic."
  , pageKeywords = [ "static", "StaticPointers", "mountStatic_", "mountStaticWithProps", "mountStaticUseContext", "VCompStatic", "StaticKey", "vcomp" ]
  , pageBody =
    [ lead
      [ "Because component constructors, event handlers and effects may need to be reconstructed on the ", em "other", " thread, native miso threads them across the boundary as ", c "static", " pointers rather than closures. This requires the ", c "StaticPointers", " language extension." ]
    , para [ "The root component is mounted with ", c "mountStatic_", " wrapped in ", c "static", ":" ]
    , hs """
      {-# LANGUAGE StaticPointers #-}
      module Main where

      import Miso
      import Miso.Native

      main :: IO ()
      main = native nativeEvents (static (mountStatic_ app))
      """
    , para [ "Child components are embedded in a ", c "view", " the same way, with ", c "vcomp", ":" ]
    , hs """
      view _ _ _ =
        view_ []
          [ vcomp () (static (mountStatic_ child)) ]
      """
    , warn
      [ b "Static-pointer limitation. ", "A ", c "static", " form may only close over ", em "top-level, closed", " bindings — it cannot capture local variables. This is why component constructors and main-thread handlers are supplied as references to top-level definitions, with any runtime data (props, decoded event payloads) shipped separately as serialised values rather than captured in a closure." ]
    , h2 "VCompStatic"
    , para
      [ c "VCompStatic", " is the ", c "View", " constructor behind ", c "vcomp", ". Unlike ", c "VComp", " it carries a ", c "StaticPtr", " to its component constructor, giving the mount a stable, cross-thread-resolvable identity (a ", c "StaticKey", ") instead of relying on a manually-supplied ", c "Key", ". "
      , "This is what lets the MTS independently reconstruct a mirror of a component mounted on the BTS, including ones mounted after the initial frame, and is also how actions dispatched from a main-thread handler get routed back to the correct component on the BTS." ]
    , para
      [ "The ", c "StaticKey", " itself serves as the mount's identity, so there is no need for ", c "(+>)", " or a manual key — use ", c "vcomp", " / ", c "vcomp_", " together with ", c "mountStatic_", " (or ", c "mountStaticWithProps", " / ", c "mountStaticUseContext", ") to build a ", c "VCompStatic", "." ]
    , note
      [ "Under ", c "NATIVE", ", ", c "(+>)", ", ", c "mount_", " and ", c "mountWithProps_", " build a ", c "VComp", " with no ", c "StaticKey", ". A component mounted that way as part of the ", em "initial", " frame is fine, but if it is mounted ", em "later", " — inside a list or behind a conditional — the MTS never registers a mirror for it, and any main-thread handler inside that subtree silently fails to dispatch. Use ", c "vcomp", " with ", c "mountStaticWithProps", " for anything that may mount after the first frame." ]
    ]
  }
-----------------------------------------------------------------------------
effectsAndThreads :: DocPage
effectsAndThreads = DocPage
  { pageSlug = "effects-and-threads"
  , pageGroup = Native
  , pageTitle = "Effects, subscriptions and threads"
  , pageBlurb = "runOnBG / runOnMain dispatch an action to the thread that should handle it; subs run on both threads unless guarded."
  , pageKeywords = [ "runOnBG", "runOnMain", "issue", "Sub", "when bts", "when mts", "websocket", "eachFrame" ]
  , pageBody =
    [ lead
      [ "Because an ", c "IO", " closure can't cross the thread boundary (only JSON-serialised actions can), cross-thread work is expressed as ", em "dispatching an action", " to the thread that should handle it. Two combinators do this:" ]
    , api
      [ ("runOnBG action", [ "run ", c "action", "'s ", c "update", " on the ", b "background", " thread (BTS). Used by a main-thread event handler that needs to change shared state, since the BTS solely owns the model." ])
      , ("runOnMain action", [ "run ", c "action", "'s ", c "update", " on the ", b "main", " thread (MTS). Used by a BTS effect that needs an imperative main-thread operation (see ", c "Miso.Native.MainThread", ")." ])
      ]
    , para
      [ "Each ships only the given action to the target thread (or dispatches it locally when already there), where its ", c "update", " runs exactly once. Sibling effects in the current ", c "update", " are unaffected, and nothing is double-executed. Off the native runtime both are an ordinary local dispatch, equivalent to ", c "issue", "." ]
    , h2 "Subscriptions and threads"
    , para
      [ "A ", c "Sub", " is dynamic — it is just a ", c "Sink action -> IO ()", " run in a forked thread — and a component's subs are started on ", b "every thread it mounts on", ". So a sub runs on ", b "both the BTS and the MTS", " (once each), and each copy dispatches into its own thread's scheduler." ]
    , para
      [ "Because a sub is ordinary runtime IO — unlike a ", c "static", " event handler, whose thread is fixed at compile time — it selects its own thread at runtime with the ", c "mts", " / ", c "bts", " booleans. This is the dynamic analogue of a handler's ", c "*Main", " variant:" ]
    , hs """
      -- background-only: open the socket once, feed the model
      wsSub sink = when bts (websocketConnect "wss://…" sink)

      -- main-thread-only: drive an imperative animation
      animSub _ = when mts (eachFrame step)
      """
    , warn
      [ b "Guard anything that must be single-owned. ", "Without a ", c "bts", " / ", c "mts", " gate a stateful sub double-runs — two websocket connections, a timer ticking on both threads — so pin such subs to one thread. The no-op fork on the other thread returns immediately." ]
    ]
  }
-----------------------------------------------------------------------------
mainThreadEvents :: DocPage
mainThreadEvents = DocPage
  { pageSlug = "main-thread-events"
  , pageGroup = Native
  , pageTitle = "Main-thread events"
  , pageBlurb = "Per-handler thread affinity: *Main variants run synchronously on the MTS for gesture- and scroll-linked work."
  , pageKeywords = [ "onTapMain", "onTouchMoveMain", "onMain", "event", "static", "MainWith", "setStyleProperty", "gesture", "latency" ]
  , pageBody =
    [ lead
      [ b "Thread affinity is per-handler, not per-event-name. ", "Any given event can be handled on ", em "either", " thread; the choice is made at each handler, so the same event (say ", c "tap", ") may run on the BTS for one element and the MTS for another. "
      , "The ", b "default is the BTS", " — a plain ", c "onTap", " handler runs on the background thread. Opting a handler into the MTS is explicit; nothing runs on the main thread unless you ask for it." ]
    , para
      [ "By default an event handler runs on the BTS: the event is forwarded from the MTS, ", c "update", " runs on the BTS, the model changes, and the resulting diff is shipped back to the MTS to paint. That round-trip is fine for most interactions but adds latency for gesture- and scroll-linked animation." ]
    , para
      [ "For those cases, handlers have ", b "*Main-suffixed variants", " (e.g. ", c "onTapMain", ", ", c "onTouchMoveMain", ") that run ", b "synchronously on the MTS", " — no VDOM diff, no patches, no BTS round-trip. Such a handler is ", em "imperative", ": it mutates the target element directly through the helpers in ", c "Miso.Native.MainThread", " (e.g. ", c "setStyleProperty", "). The ", c "*MainWith", " variants additionally hand the handler the current model and the target ", c "DOMRef", "." ]
    , para [ "Because a main-thread handler must be reconstructed on the MTS, it is an ", c "EventHandler", " embedded with ", c "event . static", " — so main-thread event handlers require ", c "StaticPointers", ":" ]
    , hs """
      {-# LANGUAGE StaticPointers #-}

      view _ _ _ =
        view_ [ event (static (onTapMain HandleTap)) ] []
      """
    , para [ "The same ", c "static", " capture limitation applies: an ", c "onTapMain", " handler refers to a top-level action / function; runtime data reaches the handler via the decoded event payload, not a captured closure." ]
    , h2 "The generic primitives: on and onMain"
    , para [ "The per-element ", c "on*", " / ", c "on*Main", " helpers are sugar over two combinators, and the ", em "same", " ", c "(eventName, decoder, toAction)", " works with either — that is how one event is captured on whichever thread you choose, per handler:" ]
    , ul
      [ [ c "on name decoder toAction", " → a plain ", c "Attribute", " that runs on the ", b "BTS", ". No ", c "static", ": a background handler is reconstructed nowhere else, so it may close over the enclosing ", c "view", "." ]
      , [ c "onMain name decoder toAction", " → an ", c "EventHandler", " that runs on the ", b "MTS", ", embedded with ", c "event . static", "." ]
      ]
    , hs """
      -- same `tap` event, one handler per thread:

      -- on the BTS
      view_
        [ on "tap" emptyDecoder (\\_ _ _ -> Grow) ]
        children

      -- on the MTS
      view_
        [ event (static (onMain "tap" emptyDecoder onTapMain)) ]
        children
      """
    , para [ "The ", c "Attribute", "-versus-", c "EventHandler", "+", c "static", " split ", em "is", " the mechanism: only the main-thread handler has to cross to the MTS by ", c "StaticKey", ", which is why ", c "onMain", " (and every ", c "*Main", " helper) needs ", c "StaticPointers", " while ", c "on", " does not. ", c "onMainWithOptions", " exposes ", c "Phase", " / ", c "Options", " for the MTS variant, mirroring ", c "onWithOptions", "." ]
    , h2 "Reaching the model (and why it is passed, not captured)"
    , para
      [ "A static main-thread handler ", em "cannot", " close over the model, props or context from the enclosing ", c "view", " — those are local bindings, which ", c "static", " forbids. So rather than capture them, the ", c "*MainWith", " variants ", b "pass the model as an argument", " to the handler, giving imperative MTS code the state it needs without a BTS round-trip. "
      , "Note this is the main thread's own copy of the model: it is populated on the MTS eventually consistently from the BTS, so a handler may observe a value slightly behind the latest BTS state." ]
    , h3 "Props and context are not on the main thread"
    , para
      [ "Unlike the model, a component's props and the app-global context are ", b "not", " mirrored to the MTS at all (matching ReactLynx, where React state is background-thread-only). They live solely on the BTS; the MTS keeps only its boot values, so ", c "getProps", " / ", c "getContext", " inside a main-thread handler would read stale data. "
      , "If a main-thread handler needs a prop or context value, fold it into the model or carry it in the dispatched action payload." ]
    , h3 "Ownership caveat"
    , para
      [ "A property you drive imperatively from the MTS must not ", em "also", " be written declaratively by the BTS ", c "view", " for the same element: both threads write the shared element tree through the same PAPI with no arbitration, so one will clobber the other. Keep a single owner per ", c "(element, property)", " — typically compositor properties like ", c "transform", " / ", c "opacity", " that the view leaves alone." ]
    ]
  }
-----------------------------------------------------------------------------
mainThreadState :: DocPage
mainThreadState = DocPage
  { pageSlug = "main-thread-state"
  , pageGroup = Native
  , pageTitle = "Main-thread-local state"
  , pageBlurb = "MainThreadRef: an IORef for state that lives only on the MTS, paired with eachFrame for vsync-coalesced animation."
  , pageKeywords = [ "MainThreadRef", "mainThreadRef", "readMainThreadRef", "writeMainThreadRef", "modifyMainThreadRef", "eachFrame", "NOINLINE", "drag", "fling" ]
  , pageBody =
    [ lead
      [ "A main-thread handler is imperative and must not write the BTS-owned model: shared state changes belong on the background thread, so dispatch them with ", c "runOnBG", ". But gestures and scroll-linked animation often need mutable state that lives ", em "only", " on the MTS — the current drag offset, a fling velocity, whether a follow loop is active. "
      , "For that, use a ", c "MainThreadRef", ", a thin ", c "IORef", " wrapper for main-thread-only state (the analogue of ReactLynx's ", c "MainThreadRef", "):" ]
    , hs """
      dragRef :: MainThreadRef Int
      dragRef = mainThreadRef 0
      {-# NOINLINE dragRef #-}
      """
    , para
      [ c "mainThreadRef", " allocates the underlying cell as a CAF via ", c "unsafePerformIO", ", so ", b "every top-level binding needs its own NOINLINE pragma", " — otherwise GHC may inline the CAF and split the state into independent copies. "
      , "Reads and writes (", c "readMainThreadRef", " / ", c "writeMainThreadRef", " / ", c "modifyMainThreadRef", ") are ordinary ", c "IORef", " operations — safe without atomics because the MTS is single-threaded — and ", c "modifyMainThreadRef_", " takes a ", c "State a ()", " so you can drive updates with the ", c "Miso.Lens", " operators." ]
    , h2 "eachFrame"
    , para
      [ "It pairs with ", c "eachFrame", " for a vsync-coalesced animation loop: read the latest gesture state from the ref, imperatively paint at most once per frame (via ", c "setStyleProperty", " / ", c "setStylePropertyTransform", "), and stop by returning ", c "False", " when the gesture ends." ]
    , hs """
      followSub :: Sub Action
      followSub _ = when mts $ eachFrame $ \\_ts -> do
        offset <- readMainThreadRef dragRef
        setStylePropertyTransform card
          [ CSS.translateX (CSS.px offset) ]
        readMainThreadRef dragging
        -- keep looping while a drag is active
      """
    ]
  }
-----------------------------------------------------------------------------
platformApis :: DocPage
platformApis = DocPage
  { pageSlug = "platform-apis"
  , pageGroup = Native
  , pageTitle = "Platform APIs and thread restrictions"
  , pageBlurb = "Native modules are BTS-only, main-thread element ops are MTS-only; guard with mts / bts."
  , pageKeywords = [ "NativeModules", "callNativeModule", "callNativeModuleWith", "Miso.Native.Module", "Miso.Native.MainThread", "setStyleProperty" ]
  , pageBody =
    [ lead
      [ "Mirroring Lynx (\"not all APIs exist on both threads\"), miso's native APIs are split by thread, and calling one from the wrong thread fails at runtime — the type system does not catch it, so guard with ", c "mts", " / ", c "bts", " when code may run on either thread. Neither module is re-exported from ", c "Miso.Native", "; import it directly." ]
    , h2 "Native modules (BTS-only)"
    , para
      [ c "Miso.Native.Module", " wraps Lynx's global ", a "https://lynxjs.org/guide/use-native-modules.html" "NativeModules", " (platform capabilities: storage, clipboard, device info, …). ", c "callNativeModule", " invokes a void-returning method and ", c "callNativeModuleWith", " a callback method whose result is decoded via ", c "FromJSON", ". ", c "NativeModules", " exists ", b "only on the BTS", ":" ]
    , hs """
      callNativeModule "NativeLocalStorageModule" "setStorageItem"
        [ String "key", String "value" ]
      """
    , para [ c "update", " runs on the BTS by default, so this just works there; from a main-thread handler, hop to the BTS first with ", c "runOnBG", ". On the MTS the module is ", c "undefined", " and the call logs a console error." ]
    , h2 "Main-thread element ops (MTS-only)"
    , para
      [ "The imperative helpers in ", c "Miso.Native.MainThread", " (", c "setStyleProperty", " etc.) and the element PAPI they call exist ", b "only on the MTS", "; on the BTS they no-op. Drive them from a ", c "*Main", " handler or via ", c "runOnMain", "." ]
    , table [ "API", "BTS", "MTS" ]
      [ [ [ c "update", " (default handlers, effects)" ], [ "✓" ], [ "via ", c "runOnMain" ] ]
      , [ [ c "Miso.Native.Module", " (NativeModules)" ], [ "✓" ], [ "✗ (undefined)" ] ]
      , [ [ c "Miso.Native.MainThread", " (element ops, ", c "eachFrame", ")" ], [ "✗ (no-op)" ], [ "✓" ] ]
      , [ [ c "MainThreadRef" ], [ "—" ], [ "✓" ] ]
      , [ [ c "Sub" ], [ "✓ (guard with ", c "bts", ")" ], [ "✓ (guard with ", c "mts", ")" ] ]
      ]
    ]
  }
-----------------------------------------------------------------------------
minimalComponent :: DocPage
minimalComponent = DocPage
  { pageSlug = "minimal-component"
  , pageGroup = Native
  , pageTitle = "A minimal native component"
  , pageBlurb = "The counter, written against Miso.Native's element vocabulary."
  , pageKeywords = [ "view_", "text_", "onTap", "nativeEvents", "counter", "example" ]
  , pageBody =
    [ lead [ "The web counter from ", goto (docsPage "your-first-component") [ "Your first Component" ], " translates line for line. Only the elements and the entry point change." ]
    , hs """
      {-# LANGUAGE StaticPointers #-}
      module Main where

      import Miso
      import Miso.Native

      data Action = Increment | Decrement

      app :: Component () () Int Action
      app = component 0 update view

      update
        :: Action
        -> Effect () () Int Action
      update = \\case
        Increment -> this += 1
        Decrement -> this -= 1

      view
        :: ()
        -> ()
        -> Int
        -> View () Int Action
      view _ _ m =
        vfrag
          [ view_ [ onTap Increment ] [ text_ [] [ "+" ] ]
          , text_ [] [ text (ms (show m)) ]
          , view_ [ onTap Decrement ] [ text_ [] [ "-" ] ]
          ]

      main :: IO ()
      main = native nativeEvents (static (mountStatic_ app))
      """
    , para
      [ c "view_", " and ", c "text_", " come from ", c "Miso.Native.Element", "; ", c "onTap", " from the element's event module. ", c "nativeEvents", " is the Lynx equivalent of ", c "defaultEvents", ". "
      , "Everything else — ", c "component", ", ", c "Effect", ", lenses, ", c "vfrag", " — is exactly the web API." ]
    , para [ "More information on how to use miso natively is available on GitHub: ", a "https://github.com/haskell-miso/miso-lynx" "haskell-miso/miso-lynx", " (with a gallery app) and the ", a "https://github.com/haskell-miso/sphynx" "sphynx", " project demonstrating cross-thread communication." ]
    ]
  }
-----------------------------------------------------------------------------
