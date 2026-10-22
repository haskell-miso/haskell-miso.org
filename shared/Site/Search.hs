-----------------------------------------------------------------------------
-- | The search palette (⌘K / Ctrl+K). A self-contained component: it
-- renders the trigger button that lives in the top bar and the modal
-- itself, keeps its own model, and navigates with 'pushRoute'.
--
-- Pages are matched on their title, keywords and summary, and on their full
-- text: the first time the palette opens it loads @/search-index.txt@,
-- which the prerenderer writes from the same prose as the pages. Until it
-- arrives (or if it cannot be loaded) search falls back to titles,
-- keywords and summaries.
module Site.Search
  ( searchPalette
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (when)
import           Data.Char (isSpace)
import           Data.List (sortOn)
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.Lens
import qualified Miso.String as MS
-----------------------------------------------------------------------------
import           Site.Docs.Content
import           Site.Docs.Types
import           Site.I18n
import           Site.Icons
import           Site.Route
import           Site.Types
-----------------------------------------------------------------------------
data Model
  = Model
  { _open   :: Bool
  , _query  :: MisoString
  , _cursor :: Int
  , _bodies :: Bodies
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | The full text of the docs pages, from @/search-index.txt@.
data Bodies
  = NotLoaded
  | Loading
  | Loaded [PageText]
  deriving Show
-----------------------------------------------------------------------------
-- | Loaded once and never changed afterwards, so comparing lengths is enough
-- (and avoids comparing ~100 KB of text on every keystroke).
instance Eq Bodies where
  NotLoaded == NotLoaded = True
  Loading == Loading = True
  Loaded xs == Loaded ys = length xs == length ys
  _ == _ = False
-----------------------------------------------------------------------------
data PageText
  = PageText
  { bodyPath  :: MisoString
  -- ^ The page's 'routeHref'
  , bodyText  :: MisoString
  , bodyLower :: MisoString
  -- ^ 'bodyText' lower-cased, for matching
  --
  -- Kept as 'MisoString' rather than 'String': on the client that is a
  -- JavaScript string, so matching uses the browser's string search
  -- instead of walking ~100 KB of linked-list characters per keystroke.
  } deriving Show
-----------------------------------------------------------------------------
open :: Lens Model Bool
open = lens _open $ \m x -> m { _open = x }
-----------------------------------------------------------------------------
query :: Lens Model MisoString
query = lens _query $ \m x -> m { _query = x }
-----------------------------------------------------------------------------
cursor :: Lens Model Int
cursor = lens _cursor $ \m x -> m { _cursor = x }
-----------------------------------------------------------------------------
bodies :: Lens Model Bodies
bodies = lens _bodies $ \m x -> m { _bodies = x }
-----------------------------------------------------------------------------
data Action
  = Open
  | Close
  | SetQuery MisoString
  | Move Int
  | Choose
  | HotKey KeyInfo
  | Pick Route
  | IndexLoaded MisoString
  | IndexFailed
  | NoOp
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | A search hit.
data Hit
  = Hit
  { hitTitle :: MisoString
  , hitGroup :: MisoString
  , hitBlurb :: MisoString
  , hitRoute :: Route
  , hitScore :: Int
  , hitSnippet :: Maybe MisoString
  -- ^ The passage that matched, when the title and summary did not
  }
-----------------------------------------------------------------------------
searchPalette :: Component Ctx () Model Action
searchPalette = (component (Model False "" 0 NotLoaded) updateSearch viewSearch)
  { useContext = True
  , subs = [ windowSub "keydown" keyInfoDecoder HotKey ]
  }
-----------------------------------------------------------------------------
updateSearch :: Action -> Effect Ctx () Model Action
updateSearch = \case
  Open -> do
    open .= True
    cursor .= 0
    io_ (focus "site-search-input")
    loaded <- use bodies
    when (loaded == NotLoaded) $ do
      bodies .= Loading
      getText "/search-index.txt" [] (IndexLoaded . body) indexFailed
  IndexLoaded txt ->
    bodies .= Loaded (parseIndex txt)
  IndexFailed ->
    -- Titles and summaries still work; try again the next time it opens.
    bodies .= NotLoaded
  Close -> do
    open .= False
    query .= ""
  SetQuery q -> do
    query .= q
    cursor .= 0
  Move d -> do
    q <- use query
    b <- use bodies
    let n = length (search b q)
    cursor %= \i -> if n == 0 then 0 else (i + d) `mod` n
  Choose -> do
    q <- use query
    b <- use bodies
    i <- use cursor
    case drop i (search b q) of
      (hit:_) -> issue (Pick (hitRoute hit))
      [] -> pure ()
  Pick r -> do
    issue Close
    io_ (pushRouteHref r)
  HotKey KeyInfo {..} -> do
    isOpen <- use open
    let KeyCode code = keyCode
    case code of
      75 | metaKey || ctrlKey -> issue (if isOpen then Close else Open)   -- ⌘K / Ctrl+K
      27 | isOpen -> issue Close                                          -- Escape
      38 | isOpen -> issue (Move (-1))                                    -- ↑
      40 | isOpen -> issue (Move 1)                                       -- ↓
      13 | isOpen -> issue Choose                                         -- ↵
      _ -> pure ()
  NoOp -> pure ()
-----------------------------------------------------------------------------
viewSearch :: Model -> View Ctx () Model Action
viewSearch m = vcontext $ \ctx ->
  vfrag
    [ H.button_
        [ P.class_ "search-trigger", P.type_ "button", E.onClick Open
        , P.aria_ "label" (translate ctx NavSearch)
        ]
        [ iconSearch
        , H.span_ [ P.class_ "search-trigger-label" ] [ t ctx NavSearch ]
        , H.kbd_ [ P.class_ "search-trigger-kbd" ] [ "⌘K" ]
        ]
    , if not (m ^. open)
        then vfrag []
        else H.div_ [ P.class_ "search-overlay", E.onClick Close ]
          [ H.div_
              [ P.class_ "search-modal", P.role_ "dialog", P.aria_ "modal" "true"
              , E.onClickWithOptions stopPropagation NoOp
              ]
              [ H.div_ [ P.class_ "search-input-row" ]
                  [ iconSearch
                  , H.input_
                      [ P.id_ "site-search-input"
                      , P.class_ "search-input"
                      , P.type_ "search"
                      , P.placeholder_ (translate ctx SearchPlaceholder)
                      , P.value_ (m ^. query)
                      , P.autocomplete_ "off"
                      , P.spellcheck_ False
                      , E.onInput SetQuery
                      ]
                  , H.kbd_ [ P.class_ "search-esc", E.onClick Close ] [ "esc" ]
                  ]
              , results ctx
              , H.div_ [ P.class_ "search-hint" ] [ t ctx SearchHint ]
              ]
          ]
    ]
  where
    hits = search (m ^. bodies) (m ^. query)
    results ctx
      | null hits && not (blank (m ^. query)) =
          H.div_ [ P.class_ "search-empty" ]
            [ t ctx SearchNoResults, " “", text (m ^. query), "”" ]
      | otherwise =
          H.ul_ [ P.class_ "search-results", P.role_ "listbox" ]
            [ H.li_
                [ P.classList_ [ ("search-hit", True), ("active", i == m ^. cursor) ]
                , P.role_ "option"
                , key_ (routeHref (hitRoute hit))
                ]
                [ H.a_
                    [ P.href_ (routeHref (hitRoute hit))
                    , E.onClickPrevent (Pick (hitRoute hit))
                    ]
                    [ H.span_ [ P.class_ "search-hit-group" ] [ text (hitGroup hit) ]
                    , H.span_ [ P.class_ "search-hit-title" ] [ text (hitTitle hit) ]
                    , H.span_ [ P.class_ "search-hit-blurb" ] [ text (maybe (hitBlurb hit) id (hitSnippet hit)) ]
                    ]
                ]
            | (i, hit) <- zip [0 :: Int ..] hits
            ]
    blank q = MS.all isSpace q
-----------------------------------------------------------------------------
-- | Every searchable page: the docs table plus the top-level pages.
index :: [Hit]
index =
  [ Hit "Home" "Site" "The miso homepage." Index 0 Nothing
  , Hit "Examples" "Site" "Applications, games and libraries built with miso." Examples 0 Nothing
  , Hit "Blog" "Site" "Notes from the miso maintainers." Blog 0 Nothing
  ] ++
  [ Hit (pageTitle p) (groupLabel (pageGroup p)) (pageBlurb p) (pageRoute p) 0 Nothing
  | p <- allPages
  ]
  where
    groupLabel = \case
      Start    -> "Getting started"
      Concepts -> "Core concepts"
      Platform -> "Platform"
      Native   -> "Native"
      Thinking -> "Thinking in miso"
-----------------------------------------------------------------------------
-- | Rank pages against a query. Empty query shows the first few pages.
-- Every word must match somewhere; a match in the title counts most, then
-- the keywords, the summary and finally the page's text.
search :: Bodies -> MisoString -> [Hit]
search loaded q0
  | null ws = take 8 index
  | otherwise =
      take 8
      [ hit { hitScore = s, hitSnippet = snippet hit }
      | (hit, s) <- sortOn (negate . snd) scored
      , s > 0
      ]
  where
    ws = MS.words (MS.toLower q0)
    scored = [ (hit, score hit) | hit <- index ]
    score hit@Hit {..} =
      let title = MS.toLower hitTitle
          blurb = MS.toLower hitBlurb
          extra = MS.toLower (MS.unwords (keywordsFor hit))
          text' = maybe "" bodyLower (bodyFor hit)
          one w
            | w `MS.isPrefixOf` title = 10
            | w `MS.isInfixOf` title = 6
            | w `MS.isInfixOf` extra = 4
            | w `MS.isInfixOf` blurb = 2
            | w `MS.isInfixOf` text' = 1
            | otherwise = 0
          scores = map one ws
      in if any (== (0 :: Int)) scores then 0 else sum scores
    keywordsFor hit =
      concat [ pageKeywords p | p <- allPages, pageRoute p == hitRoute hit ]
    bodyFor hit =
      case loaded of
        Loaded bs -> lookup (routeHref (hitRoute hit)) [ (bodyPath b, b) | b <- bs ]
        _ -> Nothing
    -- Show where the page matched when its title and summary do not say.
    snippet hit
      | any (`MS.isInfixOf` shown) ws = Nothing
      | otherwise = do
          b <- bodyFor hit
          w <- case filter (`MS.isInfixOf` bodyLower b) ws of
                 (w : _) -> Just w
                 [] -> Nothing
          let i = MS.length (fst (MS.breakOn w (bodyLower b)))
          Just (excerpt i (MS.length w) (bodyText b))
      where
        shown = MS.toLower (hitTitle hit <> " " <> hitBlurb hit)
-----------------------------------------------------------------------------
-- | About a line of text around a match, cut at word boundaries. The match
-- sits near the start, since the result row is one line and is truncated.
excerpt :: Int -> Int -> MisoString -> MisoString
excerpt i n s = lead <> before' <> hit <> after' <> trail
  where
    len = MS.length s
    from = max 0 (i - 30)
    to = min len (i + n + 110)
    (before, rest) = MS.splitAt (i - from) (MS.take (to - from) (MS.drop from s))
    (hit, after) = MS.splitAt n rest
    -- drop the partial words at either end, never the match itself
    before'
      | from > 0 && MS.any isSpace before = MS.drop 1 (MS.dropWhile (not . isSpace) before)
      | otherwise = before
    after'
      | to < len && MS.any isSpace after = MS.dropWhileEnd (not . isSpace) after
      | otherwise = after
    lead = if from > 0 then "…" else ""
    trail = if to < len then "…" else ""
-----------------------------------------------------------------------------
-- | @path\ttext@ lines, as written by the prerenderer.
parseIndex :: MisoString -> [PageText]
parseIndex txt =
  [ PageText path text' (MS.toLower text')
  | line <- MS.lines txt
  , let (path, rest) = MS.breakOn "\t" line
  , not (MS.null rest)
  , let text' = MS.drop 1 rest
  ]
-----------------------------------------------------------------------------
indexFailed :: Response MisoString -> Action
indexFailed _ = IndexFailed
-----------------------------------------------------------------------------
