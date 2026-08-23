-----------------------------------------------------------------------------
-- | Motion design for the site, written with "Miso.CSS".
--
-- Layout, colour and typography live in @static/style.css@. Everything that
-- /moves/ — keyframes, transitions, hover transforms, the reduced-motion
-- fallback — is built here with the typed @transform@ / @transition@
-- combinators and written out as @public/motion.css@ by the prerenderer.
-- Views only ever reference class names; the exceptions are transforms and
-- transitions driven by model state ('logoTilt', the language dropdown),
-- which are set inline with 'Miso.CSS.style_'.
module Site.Styles
  ( motionSheet
  , motionCss
  , easeOut
  , logoTilt
  ) where
-----------------------------------------------------------------------------
import           Miso.CSS
import           Miso.CSS.Types (KeyframeStop, StyleSheet, Style)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | The site's easing curve.
easeOut :: MisoString
easeOut = cubicBezier 0.2 0.7 0.2 1
-----------------------------------------------------------------------------
fast, medium, slow :: MisoString
fast   = ms 160
medium = ms 280
slow   = ms 520
-----------------------------------------------------------------------------
-- | The 3D tilt applied to the hero logo from its pointer position
-- (normalised -1..1 on each axis).
logoTilt :: Double -> Double -> [Style]
logoTilt nx ny =
  [ transforms
      [ perspectiveFn (px 900)
      , rotateX (deg (negate ny * 14))
      , rotateY (deg (nx * 18))
      , translate3d (pxD (nx * 10)) (pxD (ny * 10)) (px 0)
      ]
  , transition_ "transform" (ms 140) easeOut
  , willChange "transform"
  ]
  where
    pxD :: Double -> MisoString
    pxD d = px (round d)
-----------------------------------------------------------------------------
-- | A head shaken "no": side to side, decaying, with a touch of yaw. Used by
-- the desktop pillar on "Site.Home", which has nowhere to send a click.
shakeNo :: [KeyframeStop]
shakeNo =
  [ from_ [ transforms [ translateX (px 0) ] ]
  , at (pct 12) [ transforms [ translateX (px (-10)), rotate (deg (-1.2)) ] ]
  , at (pct 28) [ transforms [ translateX (px 9), rotate (deg 1.1) ] ]
  , at (pct 44) [ transforms [ translateX (px (-6)), rotate (deg (-0.8)) ] ]
  , at (pct 60) [ transforms [ translateX (px 5), rotate (deg 0.6) ] ]
  , at (pct 76) [ transforms [ translateX (px (-3)) ] ]
  , at (pct 90) [ transforms [ translateX (px 2) ] ]
  , to_ [ transforms [ translateX (px 0) ] ]
  ]
-----------------------------------------------------------------------------
shakeDur :: MisoString
shakeDur = ms 620
-----------------------------------------------------------------------------
motionCss :: MisoString
motionCss = renderStyleSheet motionSheet
-----------------------------------------------------------------------------
motionSheet :: StyleSheet
motionSheet = sheet_
  [ -- keyframes ------------------------------------------------------------
    keyframes_ "rise"
      [ from_ [ opacity 0, transforms [ translateY (px 10) ] ]
      , to_   [ opacity 1, transforms [ translateY (px 0) ] ]
      ]
  , keyframes_ "fade"
      [ from_ [ opacity 0 ], to_ [ opacity 1 ] ]
  , keyframes_ "float"
      [ from_ [ transforms [ translateY (px 0) ] ]
      , at (pct 50) [ transforms [ translateY (px (-9)) ] ]
      , to_ [ transforms [ translateY (px 0) ] ]
      ]
  , keyframes_ "pulse"
      [ from_ [ opacity 0.75 ], at (pct 50) [ opacity 1 ], to_ [ opacity 0.75 ] ]
  , keyframes_ "drift"
      [ from_ [ transforms [ translate3d (px 0) (px 0) (px 0), scale 1 ] ]
      , to_   [ transforms [ translate3d (px 40) (px 30) (px 0), scale 1.08 ] ]
      ]
  , keyframes_ "spark-h"
      [ from_ [ transforms [ translateX (vw (-10)) ], opacity 0 ]
      , at (pct 10) [ opacity 1 ]
      , at (pct 90) [ opacity 1 ]
      , to_ [ transforms [ translateX (vw 110) ], opacity 0 ]
      ]
  , keyframes_ "spark-v"
      [ from_ [ transforms [ translateY (px (-140)) ], opacity 0 ]
      , at (pct 10) [ opacity 1 ]
      , at (pct 90) [ opacity 1 ]
      , to_ [ transforms [ translateY (px 700) ], opacity 0 ]
      ]
  , keyframes_ "orbit"
      [ from_ [ transforms [ rotateX (deg 68), rotateZ (deg 0) ] ]
      , to_   [ transforms [ rotateX (deg 68), rotateZ (deg 360) ] ]
      ]
  , keyframes_ "orbit-b"
      [ from_ [ transforms [ rotateX (deg 68), rotateZ (deg 60) ] ]
      , to_   [ transforms [ rotateX (deg 68), rotateZ (deg 420) ] ]
      ]
  , keyframes_ "pop"
      [ from_ [ transforms [ scale 0.7 ], opacity 0.2 ]
      , at (pct 60) [ transforms [ scale 1.12 ], opacity 1 ]
      , to_ [ transforms [ scale 1 ] ]
      ]
  , keyframes_ "ping"
      [ from_ [ boxShadow "0 0 0 0 var(--accent-glow)" ]
      , to_   [ boxShadow "0 0 0 10px transparent" ]
      ]
  , keyframes_ "marquee"
      [ from_ [ transforms [ translateX (pct 0) ] ]
      , to_   [ transforms [ translateX (pct (-50)) ] ]
      ]
  , keyframes_ "spin"
      [ from_ [ transforms [ rotate (deg 0) ] ]
      , to_   [ transforms [ rotate (deg 360) ] ]
      ]
    -- Two identical copies of the same shake under different names: a CSS
    -- animation only restarts when its animation-name changes, so the
    -- desktop pillar alternates between the two on every click.
  , keyframes_ "shake-no-a" shakeNo
  , keyframes_ "shake-no-b" shakeNo

    -- page + hero choreography ---------------------------------------------
  , selector_ ".page-root .home, .page-root .docs, .page-root .page" [ animation ("rise " <> slow <> " " <> easeOut <> " both") ]
  , selector_ ".hero-eyebrow"         [ animation ("rise " <> slow <> " " <> easeOut <> " both"), animationDelay (ms 80) ]
  , selector_ ".hero-title"           [ animation ("rise " <> slow <> " " <> easeOut <> " both"), animationDelay (ms 140) ]
  , selector_ ".hero-subtitle"        [ animation ("rise " <> slow <> " " <> easeOut <> " both"), animationDelay (ms 220) ]
  , selector_ ".hero-actions"         [ animation ("rise " <> slow <> " " <> easeOut <> " both"), animationDelay (ms 300) ]
  , selector_ ".hero-install"         [ animation ("rise " <> slow <> " " <> easeOut <> " both"), animationDelay (ms 380) ]
  , selector_ ".hero-blob"            [ animation "drift 18s ease-in-out infinite alternate" ]
  , selector_ ".hero-blob-b"          [ animationDelay (s (-6)) ]
  , selector_ ".hero-logo-glow"       [ transition_ "transform" slow easeOut, animation "pulse 5s ease-in-out infinite" ]
  , selector_ ".hero-logo-float"      [ animation "float 6s ease-in-out infinite" ]
  , selector_ ".hero-logo-ring"       [ animation "orbit 14s linear infinite" ]
  , selector_ ".hero-logo-ring-b"     [ animation "orbit-b 22s linear infinite reverse" ]
  , selector_ ".spark-h"              [ animation "spark-h 9s linear infinite" ]
  , selector_ ".spark-v"              [ animation "spark-v 7s linear infinite" ]
  , selector_ ".spark-2"              [ animationDelay (s (-3)) ]
  , selector_ ".spark-3"              [ animationDelay (s (-6)), animationDirection "reverse" ]
  , selector_ ".spark-4"              [ animationDelay (s (-1.5)) ]
  , selector_ ".spark-6"              [ animationDelay (s (-2)) ]
  , selector_ ".spark-7"              [ animationDelay (s (-4.5)) ]
  , selector_ ".spark-8"              [ animationDelay (s (-1)), animationDirection "reverse" ]
  , selector_ ".pillar"               [ animation ("rise " <> slow <> " " <> easeOut <> " both") ]
  , selector_ ".pillar:nth-child(1)"  [ animationDelay (ms 420) ]
  , selector_ ".pillar:nth-child(2)"  [ animationDelay (ms 500) ]
  , selector_ ".pillar:nth-child(3)"  [ animationDelay (ms 580) ]

    -- hover / press micro-interactions -------------------------------------
  , selector_ ".wordmark-mark"        [ transition_ "transform" medium easeOut ]
  , selector_ ".brand:hover .wordmark-mark" [ transforms [ rotate (deg (-8)), scale 1.06 ] ]
  , selector_ ".btn"                  [ transition ("transform " <> fast <> " " <> easeOut <> ", box-shadow " <> fast <> " " <> easeOut <> ", background-color " <> fast <> " " <> easeOut <> ", border-color " <> fast <> " " <> easeOut <> ", color " <> fast <> " " <> easeOut) ]
  , selector_ ".btn:hover"            [ transforms [ translateY (px (-1)) ] ]
  , selector_ ".btn:active"           [ transforms [ translateY (px 1), scale 0.99 ] ]
  , selector_ ".btn-arrow"            [ display "inline-block", transition_ "transform" fast easeOut ]
  , selector_ ".btn:hover .btn-arrow" [ transforms [ translateX (px 3) ] ]
  , selector_ ".tool-btn"             [ transition ("color " <> fast <> " " <> easeOut <> ", background-color " <> fast <> " " <> easeOut <> ", transform " <> fast <> " " <> easeOut) ]
  , selector_ ".tool-btn:active"      [ transforms [ scale 0.94 ] ]
  , selector_ ".topnav-link"          [ transition ("color " <> fast <> " " <> easeOut <> ", background-color " <> fast <> " " <> easeOut) ]
  , selector_ ".pillar, .feature, .example-card, .doc-pager-link"
      [ transition ("transform " <> medium <> " " <> easeOut <> ", border-color " <> medium <> " " <> easeOut <> ", box-shadow " <> medium <> " " <> easeOut) ]
  , selector_ ".pillar:hover"         [ transforms [ translateY (px (-4)) ] ]
    -- doubled up on .pillar to outweigh the entrance delay on
    -- .pillar:nth-child(3), which would otherwise hold the shake back too
  , selector_ ".pillar.pillar-shake-a"
      [ animation ("shake-no-a " <> shakeDur <> " ease-in-out"), animationDelay (s 0) ]
  , selector_ ".pillar.pillar-shake-b"
      [ animation ("shake-no-b " <> shakeDur <> " ease-in-out"), animationDelay (s 0) ]
  , selector_ ".feature:hover, .example-card:hover" [ transforms [ translateY (px (-3)) ] ]
  , selector_ ".doc-pager-link:hover" [ transforms [ translateY (px (-2)) ] ]
  , selector_ ".pillar-icon"          [ transition_ "transform" medium easeOut ]
  , selector_ ".pillar:hover .pillar-icon" [ transforms [ rotate (deg (-6)), scale 1.06 ] ]
  , selector_ ".pillar-more"          [ transition ("transform " <> fast <> " " <> easeOut <> ", color " <> fast <> " " <> easeOut) ]
  , selector_ ".pillar:hover .pillar-more" [ transforms [ translateX (px 4) ] ]
  , selector_ ".counter-btn"          [ transition ("transform " <> fast <> " " <> easeOut <> ", border-color " <> fast <> " " <> easeOut <> ", background-color " <> fast <> " " <> easeOut) ]
  , selector_ ".counter-btn:active"   [ transforms [ scale 0.92 ] ]
  , selector_ ".counter-value"        [ animation ("pop 260ms " <> easeOut) ]
  , selector_ ".demo-dot"             [ animation "ping 1.8s ease-out infinite" ]
  , selector_ ".eco-track"            [ animation "marquee 48s linear infinite" ]
  , selector_ ".eco-marquee:hover .eco-track" [ animationPlayState "paused" ]
  , selector_ ".anchor-link"          [ transition_ "opacity" fast easeOut ]
  , selector_ ".lang-btn .icon:last-child" [ transition_ "transform" fast easeOut ]
  , selector_ ".lang-menu.open .lang-btn .icon:last-child" [ transforms [ rotate (deg 180) ] ]
  , selector_ ".search-overlay"       [ animation ("fade " <> fast <> " " <> easeOut <> " both") ]
  , selector_ ".search-modal"         [ animation ("rise " <> medium <> " " <> easeOut <> " both") ]
  , selector_ ".search-trigger"       [ transition ("border-color " <> fast <> " " <> easeOut <> ", color " <> fast <> " " <> easeOut <> ", box-shadow " <> fast <> " " <> easeOut) ]
  , selector_ ".docs-nav a, .example-links a, .footer-col a, .post-back, .blog-archive a"
      [ transition ("color " <> fast <> " " <> easeOut <> ", background-color " <> fast <> " " <> easeOut) ]
  , selector_ ".docs-scrim"           [ transition_ "opacity" medium easeOut ]

    -- responsive motion: off-canvas panels slide with transforms ------------
  , media_ (screen_ `and_` maxWidth_ (px 900))
      [ rule_ ".docs-sidebar" [ transforms [ translateX (pct (-100)) ], transition_ "transform" medium easeOut ]
      , rule_ ".docs-sidebar.open" [ transforms [ translateX (px 0) ] ]
      ]
  , media_ (screen_ `and_` maxWidth_ (px 760))
      [ rule_ ".topnav" [ opacity 0, transforms [ translateY (px (-8)) ], transition ("opacity " <> fast <> " " <> easeOut <> ", transform " <> fast <> " " <> easeOut) ]
      , rule_ ".site.menu-open .topnav" [ opacity 1, transforms [ translateY (px 0) ] ]
      ]

    -- accessibility: honour reduced motion ---------------------------------
  , media_ (prefersReducedMotion_ "reduce")
      [ rule_ "*, *::before, *::after"
          [ animationDuration "0.01ms !important"
          , animationIterationCount "1 !important"
          , transitionDuration "0.01ms !important"
          ]
      , rule_ ".hero-logo-tilt" [ transforms [ perspectiveFn (px 900) ] ]
      ]
  ]
-----------------------------------------------------------------------------
