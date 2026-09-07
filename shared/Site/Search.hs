-----------------------------------------------------------------------------
-- | The search palette (⌘K / Ctrl+K). A self-contained component: it
-- renders the trigger button that lives in the top bar and the modal
-- itself, keeps its own model, and navigates with 'pushRoute'.
module Site.Search
  ( searchPalette
  ) where
-----------------------------------------------------------------------------
import           Data.Char (isSpace, toLower)
import           Data.List (isInfixOf, isPrefixOf, sortOn)
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.Lens
import           Miso.String (MisoString, fromMisoString)
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
  } deriving (Show, Eq)
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
data Action
  = Open
  | Close
  | SetQuery MisoString
  | Move Int
  | Choose
  | HotKey KeyInfo
  | Pick Route
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
  }
-----------------------------------------------------------------------------
searchPalette :: Component Ctx () Model Action
searchPalette = (component (Model False "" 0) updateSearch viewSearch)
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
  Close -> do
    open .= False
    query .= ""
  SetQuery q -> do
    query .= q
    cursor .= 0
  Move d -> do
    q <- use query
    let n = length (search q)
    cursor %= \i -> if n == 0 then 0 else (i + d) `mod` n
  Choose -> do
    q <- use query
    i <- use cursor
    case drop i (search q) of
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
viewSearch :: Ctx -> () -> Model -> View Ctx Model Action
viewSearch ctx () m =
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
              , results
              , H.div_ [ P.class_ "search-hint" ] [ t ctx SearchHint ]
              ]
          ]
    ]
  where
    hits = search (m ^. query)
    results
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
                    , H.span_ [ P.class_ "search-hit-blurb" ] [ text (hitBlurb hit) ]
                    ]
                ]
            | (i, hit) <- zip [0 :: Int ..] hits
            ]
    blank q = all isSpace (fromMisoString q :: String)
-----------------------------------------------------------------------------
-- | Every searchable page: the docs table plus the top-level pages.
index :: [Hit]
index =
  [ Hit "Home" "Site" "The miso homepage." Index 0
  , Hit "Examples" "Site" "Applications, games and libraries built with miso." Examples 0
  , Hit "Blog" "Site" "Notes from the miso maintainers." Blog 0
  ] ++
  [ Hit (pageTitle p) (groupLabel (pageGroup p)) (pageBlurb p) (pageRoute p) 0
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
search :: MisoString -> [Hit]
search q0
  | null ws = take 8 index
  | otherwise =
      take 8
      [ hit { hitScore = s }
      | (hit, s) <- sortOn (negate . snd) scored
      , s > 0
      ]
  where
    ws = words (map toLower (fromMisoString q0))
    scored = [ (hit, score hit) | hit <- index ]
    score hit@Hit {..} =
      let title = map toLower (fromMisoString hitTitle)
          blurb = map toLower (fromMisoString hitBlurb)
          extra = map toLower (unwords (map fromMisoString (keywordsFor hit)))
          one w
            | w `isPrefixOf` title = 10
            | w `isInfixOf` title = 6
            | w `isInfixOf` extra = 4
            | w `isInfixOf` blurb = 2
            | otherwise = 0
          scores = map one ws
      in if any (== 0) scores then 0 else sum scores
    keywordsFor hit =
      concat [ pageKeywords p | p <- allPages, pageRoute p == hitRoute hit ]
-----------------------------------------------------------------------------
