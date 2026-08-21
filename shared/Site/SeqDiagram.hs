-----------------------------------------------------------------------------
-- | A tiny mermaid-style sequence-diagram renderer. Diagrams are described
-- as data and drawn to inline SVG at render time, so they are part of the
-- prerendered HTML, scale with the page and follow the light / dark theme
-- through CSS classes — no JavaScript library involved.
module Site.SeqDiagram
  ( Seq (..)
  , seqDiagram
  ) where
-----------------------------------------------------------------------------
import           Miso (View, text)
import qualified Miso.Html.Property as P
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP
import           Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
-- | One row of the diagram. Participant indices are 0-based, left to
-- right; the 'Bool' marks the row as highlighted (accent colour).
data Seq
  = Arrow Int Int Bool MisoString
  -- ^ @Arrow from to accent label@ — a message between two lifelines
  | Self Int Bool MisoString
  -- ^ @Self at accent label@ — a self-message loop on one lifeline
-----------------------------------------------------------------------------
seqDiagram :: [MisoString] -> [Seq] -> View context model action
seqDiagram participants rows =
  S.svg_
    [ SP.viewBox_ ("0 0 " <> ms width <> " " <> ms height)
    , P.class_ "seq"
    , P.role_ "img"
    ]
    ( [ lifeline x | x <- xs ]
    ++ concat [ box x (6 :: Int) name | (x, name) <- zip xs participants ]
    ++ concat [ box x (height - 40) name | (x, name) <- zip xs participants ]
    ++ concat (zipWith row [0 :: Int ..] rows)
    )
  where
    n = length participants
    colW = 250 :: Int
    width = n * colW
    rowH = 46 :: Int
    top = 78 :: Int
    height = top + length rows * rowH + 52
    xs = [ colW `div` 2 + i * colW | i <- [0 .. n - 1] ]

    lifeline :: Int -> View c m a
    lifeline x =
      S.line_
        [ SP.x1_ (ms x), SP.y1_ "38", SP.x2_ (ms x), SP.y2_ (ms (height - 40))
        , P.class_ "seq-lifeline"
        ]

    box :: Int -> Int -> MisoString -> [View c m a]
    box x y name =
      [ S.path_
          [ SP.d_ ("M" <> ms (x - 56) <> " " <> ms y
                   <> " h112 a6 6 0 0 1 6 6 v20 a6 6 0 0 1 -6 6 h-112 a6 6 0 0 1 -6 -6 v-20 a6 6 0 0 1 6 -6 z")
          , P.class_ "seq-box"
          ]
      , S.text_
          [ SP.x_ (ms x), SP.y_ (ms (y + 21)), SP.textAnchor_ "middle", P.class_ "seq-box-label" ]
          [ text name ]
      ]

    row :: Int -> Seq -> [View c m a]
    row i (Arrow from to accent label) =
      let y = top + i * rowH
          x1 = xs !! from
          x2 = xs !! to
          dir = if x2 > x1 then 1 else -1 :: Int
          xEnd = x2 - dir * 6
      in [ S.text_
             [ SP.x_ (ms ((x1 + x2) `div` 2)), SP.y_ (ms (y - 7))
             , SP.textAnchor_ "middle", lbl accent
             ]
             [ text label ]
         , S.line_
             [ SP.x1_ (ms (x1 + dir * 2)), SP.y1_ (ms y), SP.x2_ (ms xEnd), SP.y2_ (ms y)
             , ln accent
             ]
         , arrowHead xEnd y dir accent
         ]
    row i (Self at accent label) =
      let y = top + i * rowH
          x = xs !! at
          -- keep labels inside the canvas: right-half lifelines get their
          -- label to the left of the loop
          labelAttrs
            | x > width `div` 2 =
                [ SP.x_ (ms (x - 6)), SP.y_ (ms (y - 12)), SP.textAnchor_ "end", lbl accent ]
            | otherwise =
                [ SP.x_ (ms (x + 4)), SP.y_ (ms (y - 12)), lbl accent ]
      in [ S.text_ labelAttrs [ text label ]
         , S.path_
             [ SP.d_ ("M" <> ms x <> " " <> ms (y - 6)
                      <> " h38 a6 6 0 0 1 6 6 v2 a6 6 0 0 1 -6 6 h-30")
             , SP.fill_ "none", ln accent
             ]
         , arrowHead (x + 8) (y + 8) (-1) accent
         ]

    arrowHead :: Int -> Int -> Int -> Bool -> View c m a
    arrowHead x y dir accent =
      S.polygon_
        [ SP.points_
            (  ms x <> "," <> ms y <> " "
            <> ms (x - dir * 9) <> "," <> ms (y - 4) <> " "
            <> ms (x - dir * 9) <> "," <> ms (y + 4) )
        , if accent then P.classes_ [ "seq-head", "seq-accent" ] else P.class_ "seq-head"
        ]

    ln True = P.classes_ [ "seq-line", "seq-accent" ]
    ln False = P.class_ "seq-line"
    lbl True = P.classes_ [ "seq-label", "seq-accent-label" ]
    lbl False = P.class_ "seq-label"
-----------------------------------------------------------------------------
