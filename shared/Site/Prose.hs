-----------------------------------------------------------------------------
-- | A small vocabulary for writing documentation and article pages as
-- 'View's. Every page on the site is written with these combinators, so
-- the prose is Haskell — no markdown parser, no runtime templates.
module Site.Prose
  ( -- * Navigation
    Nav (..)
  , navigate
  , Doc
    -- * Blocks
  , lead
  , para
  , h2
  , h3
  , hs
  , sh
  , pre
  , ul
  , ol
  , note
  , tip
  , warn
  , table
  , api
  , figure
  , demo
    -- * Inlines
  , c
  , b
  , em
  , a
  , goto
  , br
  ) where
-----------------------------------------------------------------------------
import           Data.Char (isAlphaNum, toLower)
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.String (MisoString, ms, fromMisoString)
-----------------------------------------------------------------------------
import           Site.Route
import           Site.Syntax
import           Site.Types
-----------------------------------------------------------------------------
-- | The one action every content page shares: go somewhere.
newtype Nav = Go Route
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | The 'update' for a content page: push the route; the root component's
-- 'Miso.Subscription.History.uriSub' does the rest.
navigate :: Nav -> Effect Ctx props model Nav
navigate (Go r) = io_ (pushRoute r)
-----------------------------------------------------------------------------
type Doc = View Ctx () Nav
-----------------------------------------------------------------------------
-- | Opening paragraph, set larger.
lead :: [Doc] -> Doc
lead = H.p_ [ P.class_ "lead" ]
-----------------------------------------------------------------------------
para :: [Doc] -> Doc
para = H.p_ []
-----------------------------------------------------------------------------
-- | Section heading with a stable anchor id derived from its text.
h2 :: MisoString -> Doc
h2 title = H.h2_ [ P.id_ (slug title), P.class_ "anchor" ] [ text title, hash (slug title) ]
-----------------------------------------------------------------------------
h3 :: MisoString -> Doc
h3 title = H.h3_ [ P.id_ (slug title), P.class_ "anchor" ] [ text title, hash (slug title) ]
-----------------------------------------------------------------------------
hash :: MisoString -> Doc
hash s = H.a_ [ P.class_ "anchor-link", P.href_ ("#" <> s), P.aria_ "hidden" "true" ] [ "#" ]
-----------------------------------------------------------------------------
-- | Haskell code block.
hs :: MisoString -> Doc
hs = haskell
-----------------------------------------------------------------------------
-- | Shell block.
sh :: MisoString -> Doc
sh = shell
-----------------------------------------------------------------------------
-- | Plain preformatted block.
pre :: MisoString -> Doc
pre = plain
-----------------------------------------------------------------------------
ul :: [[Doc]] -> Doc
ul items = H.ul_ [] [ H.li_ [] item | item <- items ]
-----------------------------------------------------------------------------
ol :: [[Doc]] -> Doc
ol items = H.ol_ [] [ H.li_ [] item | item <- items ]
-----------------------------------------------------------------------------
callout :: MisoString -> MisoString -> [Doc] -> Doc
callout kind label body =
  H.aside_ [ P.classes_ [ "callout", "callout-" <> kind ] ]
    [ H.span_ [ P.class_ "callout-label" ] [ text label ]
    , H.div_ [ P.class_ "callout-body" ] body
    ]
-----------------------------------------------------------------------------
note, tip, warn :: [Doc] -> Doc
note = callout "note" "Note"
tip  = callout "tip"  "Tip"
warn = callout "warn" "Careful"
-----------------------------------------------------------------------------
-- | A simple table: header row, then rows of cells (each cell a list of
-- inline nodes).
table :: [MisoString] -> [[[Doc]]] -> Doc
table headers rows =
  H.div_ [ P.class_ "table-wrap" ]
    [ H.table_ []
        [ H.thead_ [] [ H.tr_ [] [ H.th_ [] [ text h ] | h <- headers ] ]
        , H.tbody_ [] [ H.tr_ [] [ H.td_ [] cell | cell <- row ] | row <- rows ]
        ]
    ]
-----------------------------------------------------------------------------
-- | A definition list of API names and short descriptions.
api :: [(MisoString, [Doc])] -> Doc
api entries =
  H.dl_ [ P.class_ "api" ] $ concat
    [ [ H.dt_ [] [ H.code_ [] [ text name ] ], H.dd_ [] desc ]
    | (name, desc) <- entries
    ]
-----------------------------------------------------------------------------
-- | A framed figure with a caption.
figure :: [Doc] -> [Doc] -> Doc
figure body caption =
  H.figure_ [ P.class_ "figure" ]
    [ H.div_ [ P.class_ "figure-body" ] body
    , H.figcaption_ [] caption
    ]
-----------------------------------------------------------------------------
-- | A live example: a running component on the left, its (highlighted)
-- source on the right. The source is the code that is actually executing —
-- see "Site.Demos".
demo :: MisoString -> MisoString -> Doc -> Doc
demo title source live =
  H.figure_ [ P.class_ "demo" ]
    [ H.figcaption_ [ P.class_ "demo-head" ]
        [ H.span_ [ P.class_ "demo-title" ] [ text title ]
        , H.span_ [ P.class_ "demo-badge" ] [ H.span_ [ P.class_ "demo-dot" ] [], "live" ]
        ]
    , H.div_ [ P.class_ "demo-body" ]
        [ H.div_ [ P.class_ "demo-stage" ] [ live ]
        , H.div_ [ P.class_ "demo-source" ] [ haskell source ]
        ]
    ]
-----------------------------------------------------------------------------
-- | Inline code.
c :: MisoString -> Doc
c s = H.code_ [ P.class_ "inline" ] [ text s ]
-----------------------------------------------------------------------------
b :: MisoString -> Doc
b s = H.strong_ [] [ text s ]
-----------------------------------------------------------------------------
em :: MisoString -> Doc
em s = H.em_ [] [ text s ]
-----------------------------------------------------------------------------
-- | External link (opens in a new tab).
a :: MisoString -> MisoString -> Doc
a href label =
  H.a_ [ P.href_ href, P.target_ "_blank", P.rel_ "noopener" ] [ text label ]
-----------------------------------------------------------------------------
-- | Internal link, navigated client-side.
goto :: Route -> [Doc] -> Doc
goto route body =
  H.a_ [ P.href_ (routeHref route), E.onClickPrevent (Go route) ] body
-----------------------------------------------------------------------------
br :: Doc
br = H.br_ []
-----------------------------------------------------------------------------
-- | @"Your first Component"@ → @"your-first-component"@
slug :: MisoString -> MisoString
slug = ms . collapse . map norm . fromMisoString
  where
    norm ch | isAlphaNum ch = toLower ch
            | otherwise = '-'
    collapse ('-':'-':xs) = collapse ('-':xs)
    collapse (x:xs) = x : collapse xs
    collapse [] = []
-----------------------------------------------------------------------------
