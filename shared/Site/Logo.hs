-----------------------------------------------------------------------------
-- | The miso mark, inline as SVG so it inherits @currentColor@ and can be
-- animated with CSS. The static files in @assets/logo/@ share the same
-- geometry.
--
-- The mark is an X with its upper-right arm removed — a lambda: a solid
-- long stroke and a hollow (wireframe) leg of the same width that tucks
-- under it.
module Site.Logo
  ( logoMark
  , logoMarkGradient
  , wordmark
  ) where
-----------------------------------------------------------------------------
import           Miso (View, Attribute, text)
import           Miso.String (MisoString)
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP
-----------------------------------------------------------------------------
-- | Monochrome mark (uses @currentColor@).
logoMark :: [Attribute model action] -> View context model action
logoMark attrs =
  S.svg_
    ( [ SP.viewBox_ "0 0 24 24"
      , P.role_ "img"
      , P.aria_ "label" "miso"
      ] ++ attrs )
    [ legPolygon "currentColor"
    , S.polygon_ [ SP.points_ strokePoints, SP.fill_ "currentColor" ]
    ]
-----------------------------------------------------------------------------
-- | Gradient mark used by the interactive hero logo.
logoMarkGradient :: [Attribute model action] -> View context model action
logoMarkGradient attrs =
  S.svg_
    ( [ SP.viewBox_ "0 0 24 24"
      , P.role_ "img"
      , P.aria_ "label" "miso"
      ] ++ attrs )
    [ S.defs_ []
        [ S.linearGradient_
            [ P.id_ "miso-grad", SP.x1_ "0", SP.y1_ "0", SP.x2_ "1", SP.y2_ "1" ]
            [ S.stop_ [ SP.offset_ "0",    SP.stopColor_ "#ffb84a" ]
            , S.stop_ [ SP.offset_ "0.55", SP.stopColor_ "#f08a24" ]
            , S.stop_ [ SP.offset_ "1",    SP.stopColor_ "#e2531f" ]
            ]
        ]
    , legPolygon "url(#miso-grad)"
    , S.polygon_ [ SP.points_ strokePoints, SP.fill_ "url(#miso-grad)" ]
    ]
-----------------------------------------------------------------------------
-- | Mark + the word "miso".
wordmark :: View context model action
wordmark =
  H.span_ [ P.class_ "wordmark" ]
    [ logoMark [ P.class_ "wordmark-mark" ]
    , H.span_ [ P.class_ "wordmark-text" ] [ text "miso" ]
    ]
-----------------------------------------------------------------------------
-- | The hollow leg: same footprint as the lower-right leg of the stroke
-- (6.3 units wide at the baseline), outlined with a thin stroke.
legPolygon :: MisoString -> View context model action
legPolygon color =
  S.polygon_
    [ SP.points_ legPoints
    , SP.fill_ "none"
    , SP.stroke_ color
    , SP.strokeWidth_ "0.7"
    , SP.strokeLinejoin_ "miter"
    ]
-----------------------------------------------------------------------------
strokePoints, legPoints :: MisoString
strokePoints = "1.5,2 7.8,2 22.5,22 16.2,22"
legPoints    = "0.93,21.65 6.37,21.65 12.25,13.6 9.3,10.1"
-----------------------------------------------------------------------------
