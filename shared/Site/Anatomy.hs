-----------------------------------------------------------------------------
-- | The "anatomy" section of the front page: a scroll-driven, 3D exploded
-- view of a native miso app. As the visitor scrolls through a tall track,
-- a sticky scene tilts an iPhone into an isometric stack and pulls it
-- apart into layers:
--
--   phone → iOS process → two PrimJS interpreters (MTS / BTS)
--         → a bundle in each (JS, CSS, assets)
--         → two GHC runtimes, one application, one miso
--
-- The scroll position is read by a subscription; the model holds a single
-- progress value in [0,1] and every transform is computed from it in
-- Haskell and applied inline with "Miso.CSS" — the established pattern for
-- model-driven motion on this site.
module Site.Anatomy
  ( anatomyScene
  ) where
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.CSS as CSS
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.Lens
import           Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
import           Site.I18n
import           Site.Logo (logoMarkGradient, logoMark)
import           Site.Types
-----------------------------------------------------------------------------
data Model = Model
  { _progress :: Double
  , _hovered  :: Maybe Int
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
progress :: Lens Model Double
progress = lens _progress $ \m x -> m { _progress = x }
-----------------------------------------------------------------------------
hovered :: Lens Model (Maybe Int)
hovered = lens _hovered $ \m x -> m { _hovered = x }
-----------------------------------------------------------------------------
data Action
  = Recompute
  | SetProgress Double
  | Hover Int
  | Unhover
  deriving (Show, Eq)
-----------------------------------------------------------------------------
anatomyScene :: Component Ctx () Model Action
anatomyScene = (component (Model 0 Nothing) updateAnat viewAnat)
  { useContext = True
  , mount = Just Recompute
  , subs =
      [ windowSub "scroll" emptyDecoder (const Recompute)
      , windowSub "resize" emptyDecoder (const Recompute)
      ]
  }
-----------------------------------------------------------------------------
updateAnat :: Action -> Effect Ctx () Model Action
updateAnat = \case
  Recompute      -> io (SetProgress <$> measure)
  SetProgress p  -> progress .= p
  Hover i        -> hovered .= Just i
  Unhover        -> hovered .= Nothing
-----------------------------------------------------------------------------
-- | How far the sticky viewport has travelled through the track, 0..1,
-- quantised to 0.5% so scrolling doesn't trigger redundant redraws.
measure :: IO Double
measure = do
  el <- getElementById "anatomy-track"
  rect <- el # "getBoundingClientRect" $ ()
  top :: Double <- fromJSValUnchecked =<< rect ! "top"
  height :: Double <- fromJSValUnchecked =<< rect ! "height"
  vh <- fromIntegral <$> windowInnerHeight :: IO Double
  let denom = max 1 (height - vh)
      p = max 0 (min 1 (negate top / denom))
  pure (fromIntegral (round (p * 200) :: Int) / 200)
-----------------------------------------------------------------------------
-- Geometry --------------------------------------------------------------------
-----------------------------------------------------------------------------
clamp01 :: Double -> Double
clamp01 = max 0 . min 1
-----------------------------------------------------------------------------
smooth :: Double -> Double
smooth t = t * t * (3 - 2 * t)
-----------------------------------------------------------------------------
-- | 0..1 ramp over a window of the scroll progress.
ramp :: Double -> Double -> Double -> Double
ramp from width p = smooth (clamp01 ((p - from) / width))
-----------------------------------------------------------------------------
-- | The whole stack tilts from face-on to isometric over the first quarter.
sceneTilt :: Double -> Double
sceneTilt = ramp 0 0.28
-----------------------------------------------------------------------------
-- | The layers pull apart once the tilt is under way.
spread :: Double -> Double
spread = ramp 0.06 0.8
-----------------------------------------------------------------------------
-- | Resting Z offset of each layer (px) at full spread, top to bottom.
offsets :: [Double]
offsets = [210, 105, 0, -105, -210]
-----------------------------------------------------------------------------
-- | Scroll window in which each non-phone layer fades in.
reveals :: [Double]
reveals = [0.10, 0.28, 0.47, 0.65]
-----------------------------------------------------------------------------
stage :: Double -> Int
stage p
  | p < 0.14 = 0
  | p < 0.34 = 1
  | p < 0.53 = 2
  | p < 0.70 = 3
  | otherwise = 4
-----------------------------------------------------------------------------
at' :: [Double] -> Int -> Double
at' xs i = case drop i xs of { (x:_) -> x; [] -> 0 }
-----------------------------------------------------------------------------
d2 :: Double -> Double
d2 x = fromIntegral (round (x * 100) :: Int) / 100
-----------------------------------------------------------------------------
pxD :: Double -> MisoString
pxD = CSS.px . round
-----------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
-----------------------------------------------------------------------------
viewAnat :: Ctx -> () -> Model -> View Ctx Model Action
viewAnat ctx () m =
  H.section_ [ P.class_ "anatomy", P.id_ "anatomy-track" ]
    [ H.div_ [ P.class_ "anatomy-sticky" ]
        [ H.div_ [ P.classes_ [ "section-head", "anat-head" ] ]
            [ H.h2_ [] [ t ctx AnatTitle ]
            , H.p_ [] [ t ctx AnatSubtitle ]
            ]
        , H.div_ [ P.class_ "anat-scene" ]
            [ H.div_ [ P.class_ "anat-glow" ] []
            , H.div_
                [ P.class_ "anat-stack"
                , CSS.style_
                    [ CSS.transforms
                        [ CSS.scale (d2 (1 - 0.30 * s))
                        , CSS.rotateX (CSS.deg (d2 (58 * e)))
                        , CSS.rotateZ (CSS.deg (d2 (-26 * e)))
                        ]
                    ]
                ]
                [ layer 0 (phoneLayer (1 - 0.35 * s))
                , layer 1 processLayer
                , layer 2 (interpLayer ctx)
                , layer 3 bundleLayer
                , layer 4 (runtimeLayer ctx)
                ]
            , H.div_ [ P.class_ "anat-callouts", P.aria_ "hidden" "true" ]
                [ H.span_
                    [ P.classList_ [ ("anat-callout", True), ("on", m ^. hovered == Just i) ]
                    , CSS.style_ [ CSS.top (CSS.pct (calloutTop i)) ]
                    ]
                    [ text label ]
                | (i, label) <- zip [0 ..] calloutLabels
                ]
            ]
        , H.div_ [ P.class_ "anat-stages" ]
            [ H.div_ [ P.class_ "anat-dots", P.aria_ "hidden" "true" ]
                [ H.span_ [ P.classList_ [ ("anat-dot", True), ("on", i <= st) ] ] []
                | i <- [0 .. 4 :: Int]
                ]
            , H.div_ [ P.class_ "anat-captions" ]
                [ H.p_ [ P.classList_ [ ("anat-caption", True), ("on", i == st) ] ] [ t ctx key ]
                | (i, key) <- zip [0 ..] [ AnatStage1, AnatStage2, AnatStage3, AnatStage4, AnatStage5 ]
                ]
            ]
        , H.div_
            [ P.class_ "anat-hint"
            , CSS.style_ [ CSS.opacity (d2 (1 - ramp 0 0.06 p)) ]
            ]
            [ t ctx AnatHint, H.span_ [ P.class_ "anat-hint-arrow" ] [ "↓" ] ]
        ]
    ]
  where
    p = m ^. progress
    e = sceneTilt p
    s = spread p
    st = stage p

    layer i content =
      let lift = if m ^. hovered == Just i then 24 else 0
          z = (offsets `at'` i) * s + lift
          dim = case m ^. hovered of
                  Just j | j /= i -> 0.45
                  _ -> 1
          o | i == 0 = dim
            | otherwise = dim * ramp (reveals `at'` (i - 1)) 0.12 p
      in H.div_
          [ P.classList_
              [ ("anat-layer", True)
              , ("anat-layer-" <> ms i, True)
              , ("hovered", m ^. hovered == Just i)
              ]
          , CSS.style_
              [ CSS.transforms [ CSS.translateZ (pxD z) ]
              , CSS.opacity (d2 o)
              ]
          , E.onPointerEnter (const (Hover i))
          , E.onPointerLeave (const Unhover)
          ]
          content
-----------------------------------------------------------------------------
-- | Hover callout for each layer, top to bottom (technical identifiers,
-- deliberately untranslated).
calloutLabels :: [MisoString]
calloutLabels =
  [ "your app"
  , "iOS process"
  , "PrimJS × 2 — MTS / BTS"
  , "main.lynx.bundle × 2"
  , "GHC RTS × 2 · app · miso"
  ]
-----------------------------------------------------------------------------
calloutTop :: Int -> Double
calloutTop i = [4, 26, 46, 64, 84] `at'` i
-----------------------------------------------------------------------------
-- | The phone itself: shell, dynamic island, and a tiny miso app on screen.
phoneLayer :: Double -> [View Ctx Model Action]
phoneLayer appOpacity =
  [ H.div_ [ P.class_ "anat-phone" ]
      [ H.div_ [ P.class_ "anat-island" ] []
      , H.div_ [ P.class_ "anat-screen", CSS.style_ [ CSS.opacity (d2 appOpacity) ] ]
          [ logoMarkGradient [ P.class_ "anat-app-logo" ]
          , H.span_ [ P.class_ "anat-app-name" ] [ "miso" ]
          , H.div_ [ P.class_ "anat-app-rows" ]
              [ H.span_ [] [], H.span_ [] [], H.span_ [] [] ]
          ]
      ]
  ]
-----------------------------------------------------------------------------
processLayer :: [View Ctx Model Action]
processLayer =
  [ H.div_ [ P.classes_ [ "anat-plate", "anat-process" ] ]
      [ H.span_ [ P.class_ "anat-tag" ] [ "iOS process" ]
      , H.div_ [ P.class_ "anat-process-grid" ] []
      ]
  ]
-----------------------------------------------------------------------------
interpLayer :: Ctx -> [View Ctx Model Action]
interpLayer ctx =
  [ H.div_ [ P.classes_ [ "anat-plate", "anat-cols" ] ]
      [ H.div_ [ P.classes_ [ "anat-card", "anat-mts" ] ]
          [ H.span_ [ P.class_ "anat-card-title" ] [ "PrimJS" ]
          , H.span_ [ P.class_ "anat-card-sub" ] [ t ctx AnatMain ]
          , H.span_ [ P.class_ "anat-chip" ] [ "MTS" ]
          ]
      , H.div_ [ P.classes_ [ "anat-card", "anat-bts" ] ]
          [ H.span_ [ P.class_ "anat-card-title" ] [ "PrimJS" ]
          , H.span_ [ P.class_ "anat-card-sub" ] [ t ctx AnatBg ]
          , H.span_ [ P.class_ "anat-chip" ] [ "BTS" ]
          ]
      ]
  ]
-----------------------------------------------------------------------------
bundleLayer :: [View Ctx Model Action]
bundleLayer =
  [ H.div_ [ P.classes_ [ "anat-plate", "anat-cols" ] ]
      [ bundleCard, bundleCard ]
  ]
  where
    bundleCard :: View Ctx Model Action
    bundleCard =
      H.div_ [ P.classes_ [ "anat-card", "anat-bundle" ] ]
        [ H.span_ [ P.class_ "anat-card-title" ] [ "main.lynx.bundle" ]
        , H.div_ [ P.class_ "anat-assets" ]
            [ H.span_ [ P.class_ "anat-chip" ] [ "JS" ]
            , H.span_ [ P.class_ "anat-chip" ] [ "CSS" ]
            , H.span_ [ P.class_ "anat-chip" ] [ "assets" ]
            ]
        ]
-----------------------------------------------------------------------------
runtimeLayer :: Ctx -> [View Ctx Model Action]
runtimeLayer ctx =
  [ H.div_ [ P.classes_ [ "anat-plate", "anat-runtime" ] ]
      [ H.div_ [ P.class_ "anat-rts-row" ]
          [ H.span_ [ P.classes_ [ "anat-chip", "anat-rts" ] ] [ "GHC RTS" ]
          , H.span_ [ P.classes_ [ "anat-chip", "anat-rts" ] ] [ "GHC RTS" ]
          ]
      , H.div_ [ P.class_ "anat-bar" ] [ t ctx AnatApp ]
      , H.div_ [ P.classes_ [ "anat-bar", "anat-miso" ] ]
          [ logoMark [ P.class_ "anat-miso-mark" ], "miso" ]
      ]
  ]
-----------------------------------------------------------------------------
