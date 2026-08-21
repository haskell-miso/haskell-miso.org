-----------------------------------------------------------------------------
-- | "Thinking in miso" — a guided walk-through of how to structure an
-- application, in the spirit of React's "Thinking in React".
module Site.Docs.Thinking
  ( thinkingPages
  ) where
-----------------------------------------------------------------------------
import Miso ((+>))
-----------------------------------------------------------------------------
import Site.Demos (bookmarksDemo)
import Site.Docs.Types
import Site.Prose
import Site.Route
-----------------------------------------------------------------------------
thinkingPages :: [DocPage]
thinkingPages =
  [ overview
  , modelPage
  , componentsPage
  , updatePage
  , dataFlowPage
  ]
-----------------------------------------------------------------------------
overview :: DocPage
overview = DocPage
  { pageSlug = "overview"
  , pageGroup = Thinking
  , pageTitle = "Thinking in miso"
  , pageBlurb = "A guided walk-through: from a mockup to a working application, the miso way."
  , pageKeywords = [ "thinking", "guide", "tutorial", "architecture", "walkthrough", "mockup", "design" ]
  , pageBody =
    [ lead
      [ "miso can change how you think about the designs you look at and the apps you build. When you build a UI with miso you will first describe its ", em "state", " as data, then write a pure function from that state to the screen, and finally name the ", em "events", " that move the state forward. "
      , "This guide walks through that process by building a small bookmarks manager." ]
    , h2 "Start with the mockup"
    , para [ "Imagine we already have a JSON endpoint and a mockup from a designer. Here it is — except this \"mockup\" is already the finished application, running (the endpoint is mocked with a local list):" ]
    , figure
      [ "bookmarks-app" +> bookmarksDemo ]
      [ "The mockup, live. Search, filter by tag, click a row for the detail pane — everything this guide builds, in ~60 lines." ]
    , para [ "and some data:" ]
    , pre """
      [ { "id": 1, "title": "miso on GitHub",  "url": "https://github.com/dmjio/miso", "tags": ["haskell"] }
      , { "id": 2, "title": "Lynx docs",       "url": "https://lynxjs.org",             "tags": ["mobile"]  }
      , { "id": 3, "title": "Elm architecture", "url": "https://guide.elm-lang.org",    "tags": ["haskell"] }
      ]
      """
    , h2 "The four steps"
    , ol
      [ [ goto (thinkingPage "model") [ b "Design the model" ], " — write down the minimal state the UI needs, and nothing that can be computed from it." ]
      , [ goto (thinkingPage "components") [ b "Break the UI into views and components" ], " — decide what is a plain view function and what deserves its own ", c "Component", ", and where each piece of state lives." ]
      , [ goto (thinkingPage "update") [ b "Name the actions and write update" ], " — describe how the model evolves and keep IO at the edges." ]
      , [ goto (thinkingPage "data-flow") [ b "Connect the pieces" ], " — props down, mail up, context around; then prerender, route and ship." ]
      ]
    , h2 "The mindset in three sentences"
    , ul
      [ [ b "State is data.", " Your whole UI at any instant is a plain Haskell value with an ", c "Eq", " instance." ]
      , [ b "The view is a function.", " ", c "view :: context -> props -> model -> View", " — no hidden state, no mutation, nothing to keep in sync." ]
      , [ b "Change is a fold.", " Every event is an ", c "action", "; ", c "update", " folds it into the model and ", em "schedules", " any IO. Results come back as more actions." ]
      ]
    , para [ "If you have written Elm, this will feel like home. If you come from React, notice that there are no hooks to order, no dependency arrays and no stale closures — the model is the only state and it is always current." ]
    ]
  }
-----------------------------------------------------------------------------
modelPage :: DocPage
modelPage = DocPage
  { pageSlug = "model"
  , pageGroup = Thinking
  , pageTitle = "Step 1: Design the model"
  , pageBlurb = "Find the minimal representation of state; derive everything else in view."
  , pageKeywords = [ "model", "state", "minimal", "derived", "Eq", "RemoteData", "lenses", "design" ]
  , pageBody =
    [ lead [ "The model is the single source of truth. The goal is to make it ", em "minimal", ": store what the UI cannot recompute, and compute everything else inside ", c "view", "." ]
    , h2 "List everything the UI shows"
    , ul
      [ [ "The list of bookmarks from the server" ]
      , [ "The search text the user typed" ]
      , [ "The selected tag filter" ]
      , [ "The bookmarks that match the search and the filter" ]
      , [ "The count next to each tag" ]
      , [ "Which bookmark is expanded in the detail pane" ]
      , [ "Whether we are still loading, or something failed" ]
      ]
    , para [ "Now ask three questions of each item. Does it change over time? Can it be computed from something else? Is it passed in from a parent?" ]
    , ul
      [ [ "The ", b "matching bookmarks", " and the ", b "tag counts", " are computed from the list, the query and the filter — ", em "not", " state." ]
      , [ "Loading and failure are ", b "states of the list itself", ", so model them as one type rather than three booleans." ]
      ]
    , h2 "Write it down"
    , hs """
      data Model = Model
        { _bookmarks :: Remote [Bookmark]   -- what the server said (or hasn't yet)
        , _query     :: MisoString          -- search text
        , _tagFilter :: Maybe Tag           -- Nothing = all
        , _selected  :: Maybe BookmarkId    -- expanded row
        } deriving (Eq, Generic)

      data Remote a = Loading | Failed MisoString | Loaded a
        deriving (Eq, Generic)

      data Bookmark = Bookmark
        { bookmarkId :: BookmarkId, title :: MisoString, url :: MisoString, tags :: [Tag] }
        deriving (Eq, Generic, FromJSON)

      makeLenses ''Model
      """
    , para [ "Four fields. Everything the mockup shows can be produced from them:" ]
    , hs """
      visible :: Model -> [Bookmark]
      visible m =
        [ b | Loaded bs <- [m ^. bookmarks], b <- bs
            , matches (m ^. query) b
            , maybe True (`elem` tags b) (m ^. tagFilter) ]

      tagCounts :: [Bookmark] -> [(Tag, Int)]
      tagCounts = ...
      """
    , tip
      [ "Derived values are just functions — call them from ", c "view", ". miso only re-renders when the model changed (its ", c "Eq", " instance decides), and the virtual DOM diff makes redraws cheap. Reach for memoisation only when a profiler tells you to." ]
    , h2 "Sum types beat booleans"
    , para
      [ c "Remote", " makes the impossible states (\"loaded ", em "and", " failed\") unrepresentable, and ", c "view", " is forced to handle every case with a ", c "case", " expression. The same idea applies to modes (", c "Viewing | Editing Draft", "), wizards (one constructor per step) and forms (", c "Either Errors Valid", ")." ]
    , h2 "Eq is a feature"
    , para
      [ "Every model needs an ", c "Eq", " instance. Derive it. It is what lets miso skip a render when nothing changed, and it is what makes tests trivial: ", c "update", " is a pure ", c "RWS", " so ", c "runEffect", " gives you the new model to compare against an expected value." ]
    , para [ "Next: ", goto (thinkingPage "components") [ "break the UI into views and components" ], "." ]
    ]
  }
-----------------------------------------------------------------------------
componentsPage :: DocPage
componentsPage = DocPage
  { pageSlug = "components"
  , pageGroup = Thinking
  , pageTitle = "Step 2: Views and components"
  , pageBlurb = "Most of your UI should be plain view functions. Reach for a Component when a piece needs its own state, subs or lifecycle."
  , pageKeywords = [ "components", "views", "hierarchy", "props", "keys", "+>", "mountWithProps_", "where state lives" ]
  , pageBody =
    [ lead
      [ "In miso there are two ways to split a UI: ", b "view functions", " (any function returning a ", c "View", ") and ", b "components", " (a ", c "Component", " with its own model, update and lifecycle). "
      , "Reach for the first by default." ]
    , h2 "Draw the boxes"
    , figure
      [ pre """
        BookmarksApp  (Component — owns the Model)
        ├── searchBar      (view function: query, onInput)
        ├── tagSidebar     (view function: tag counts, selected tag)
        ├── bookmarkTable  (view function)
        │   └── bookmarkRow (view function, keyed by id)
        ├── detailPane     (view function)
        └── "add" +> addBookmarkForm  (Component — owns its draft, validation, submit state)
        """ ]
      [ "Boxes are functions. Only two of them are components." ]
    , h2 "When is something a Component?"
    , para [ "A ", c "Component", " costs a little ceremony (its own model and action type, a key) and buys isolation. Make one when a piece of UI:" ]
    , ul
      [ [ b "owns state nobody else needs", " — the add form's draft and validation errors are irrelevant to the table;" ]
      , [ b "needs subscriptions or lifecycle hooks", " — a clock, a websocket, a third-party widget initialised in ", c "mount", ";" ]
      , [ b "is reused with different props", " — the same ", c "avatar", " component mounted for each user;" ]
      , [ b "should re-render independently", " — its model changes often while the parent's does not." ]
      ]
    , para [ "Otherwise write a function. ", c "bookmarkRow :: Bookmark -> View ctx Model Action", " is simpler than a component, is trivially testable, and re-renders as part of its parent." ]
    , hs """
      bookmarkRow :: Maybe BookmarkId -> Bookmark -> View ctx Model Action
      bookmarkRow selected b =
        H.tr_ [ key_ (bookmarkId b)                             -- stable identity in the list
              , HP.classList_ [ ("selected", selected == Just (bookmarkId b)) ]
              , HE.onClick (Select (bookmarkId b)) ]
          [ H.td_ [] [ text (title b) ]
          , H.td_ [] [ text (MS.intercalate ", " (tags b)) ]
          ]
      """
    , note [ "Give list items a ", c "key_", ". It keeps the DOM node (and any CSS transition on it) attached to the ", em "same", " bookmark when the list is filtered or reordered — see ", goto (docsPage "keys") [ "Keys" ], "." ]
    , h2 "Where does each piece of state live?"
    , table [ "State", "Lives in", "Because" ]
      [ [ [ "bookmarks, query, filter, selection" ], [ c "BookmarksApp", " model" ], [ "several views read it; the app owns it" ] ]
      , [ [ "the add form's draft & errors" ], [ c "addBookmarkForm", " model" ], [ "private; nobody else cares until submit" ] ]
      , [ [ "the currently selected tag, as seen by the form" ], [ "props" ], [ "the form only ", em "reads", " it to pre-fill a tag" ] ]
      , [ [ "language, theme, current user" ], [ "context" ], [ "global; every component may read it" ] ]
      ]
    , para [ "The rule of thumb: state lives in the ", b "closest common owner", " of everything that reads or writes it. Push it up only as far as it needs to go, and pass it down as props." ]
    , hs """
      viewApp :: Ctx -> () -> Model -> View Ctx Model Action
      viewApp ctx _ m =
        H.main_ []
          [ searchBar (m ^. query)
          , H.div_ [ HP.class_ "columns" ]
              [ tagSidebar (m ^. tagFilter) (tagCounts (loadedOr [] (m ^. bookmarks)))
              , bookmarkTable (m ^. selected) (visible m)
              , detailPane (selectedBookmark m)
              ]
          , mountWithProps_ "add-form" (FormProps (m ^. tagFilter)) addBookmarkForm
          ]
      """
    , para [ "Next: ", goto (thinkingPage "update") [ "name the actions and write update" ], "." ]
    ]
  }
-----------------------------------------------------------------------------
updatePage :: DocPage
updatePage = DocPage
  { pageSlug = "update"
  , pageGroup = Thinking
  , pageTitle = "Step 3: Actions and update"
  , pageBlurb = "Actions are the vocabulary of your UI. update folds them into the model and schedules IO at the edge."
  , pageKeywords = [ "actions", "update", "effects", "io", "getJSON", "mount", "Init", "subscriptions", "startSub", "debounce" ]
  , pageBody =
    [ lead
      [ "An ", c "action", " is something that ", em "happened", ": the user typed, the server answered, a timer fired. Name actions after events, not after setters, and let ", c "update", " decide what they mean." ]
    , h2 "The vocabulary"
    , hs """
      data Action
        = Init                              -- mounted: go fetch
        | GotBookmarks (Either MisoString [Bookmark])
        | QueryChanged MisoString
        | TagPicked (Maybe Tag)
        | Select BookmarkId
        | BookmarkAdded Bookmark            -- mailed up by the form
        deriving (Eq, Show)
      """
    , para [ c "QueryChanged", " rather than ", c "SetQuery", ": the name leaves ", c "update", " free to also reset the selection, or later to debounce a request, without renaming anything." ]
    , h2 "update is a fold"
    , hs """
      update :: Action -> Effect Ctx () Model Action
      update = \\case
        Init ->
          getJSON "/api/bookmarks" [] (GotBookmarks . Right) (GotBookmarks . Left . ms)

        GotBookmarks (Right bs) -> bookmarks .= Loaded bs
        GotBookmarks (Left err) -> bookmarks .= Failed err

        QueryChanged q -> do
          query    .= q
          selected .= Nothing                    -- a new search deselects

        TagPicked t   -> tagFilter .= t
        Select bid    -> selected  %= toggle bid

        BookmarkAdded b -> bookmarks %= fmap (b :)
      """
    , para [ "Every branch is a small, total function on the model. There is no ", c "await", ", no promise chain, no ", c "setState", " callback ordering: ", c "getJSON", " ", em "schedules", " the request and the answer arrives later as ", c "GotBookmarks", ", which is handled like any other action." ]
    , h2 "IO lives at the edge"
    , ul
      [ [ c "io", " / ", c "io_", " for asynchronous work — the default. The scheduler runs it off the update thread and catches exceptions." ]
      , [ c "sync", " only for cheap, must-be-ordered reads (a ", c "localStorage", " lookup, measuring an element)." ]
      , [ c "mount = Just Init", " to kick things off when the component appears; ", c "unmount", " to clean up." ]
      , [ "Long-lived sources are ", goto (docsPage "subscriptions") [ "subscriptions" ], " — a websocket, ", c "rAFSub", ", ", c "routerSub", " — not effects." ]
      ]
    , h3 "Example: debouncing the search"
    , para [ "Because subs can be started and stopped from ", c "update", ", a debounce is a tiny sub rather than a library:" ]
    , hs """
      QueryChanged q -> do
        query .= q
        stopSub "debounce"
        startSub "debounce" $ \\sink -> do
          threadDelay 250000
          sink (Search q)
      """
    , h2 "Testing"
    , para [ c "update", " is an ", c "RWS", " and ", c "view", " is a function, so both are testable without a browser: run ", c "update", " on a model, compare the resulting model with ", c "==", ", and render ", c "view", " to HTML with ", c "toHtml", " to assert on markup." ]
    , para [ "Next: ", goto (thinkingPage "data-flow") [ "connect the pieces and ship" ], "." ]
    ]
  }
-----------------------------------------------------------------------------
dataFlowPage :: DocPage
dataFlowPage = DocPage
  { pageSlug = "data-flow"
  , pageGroup = Thinking
  , pageTitle = "Step 4: Connect the pieces and ship"
  , pageBlurb = "Props down, mail up, context around, PubSub sideways — then routing, prerendering and mobile."
  , pageKeywords = [ "data flow", "mailParent", "props", "context", "pubsub", "routing", "prerender", "hydrate", "ship", "native" ]
  , pageBody =
    [ lead [ "So far data has flowed ", em "down", ": the app owns the model, view functions read it, the form receives props. Real apps also need to send things ", em "up", " and ", em "sideways", ". miso gives you a small set of tools; pick the least powerful one that works." ]
    , h2 "The toolbox"
    , table [ "You need to…", "Use", "Notes" ]
      [ [ [ "give a child read-only data" ], [ c "mountWithProps_" ], [ "synchronous; child re-renders when props change; ", c "onPropsChanged", " to react" ] ]
      , [ [ "tell the parent something happened" ], [ c "mailParent" ], [ "asynchronous JSON message; parent's ", c "mailbox = checkMail …" ] ]
      , [ [ "share something with the whole tree" ], [ "context (", c "modifyContext", ")" ], [ "theme, language, session; opt in with ", c "useContext" ] ]
      , [ [ "notify components that don't know each other" ], [ c "Miso.PubSub" ], [ "topics; ", c "publish", " / ", c "subscribe" ] ]
      , [ [ "poke a specific component" ], [ c "mail componentId" ], [ "ids come from ", c "ask", " inside ", c "Effect" ] ]
      ]
    , h2 "Inverse data flow: the form tells the app"
    , para [ "When the add form is submitted it does not reach into the app's model — it can't. It mails its parent, and the parent decides:" ]
    , hs """
      -- in addBookmarkForm's update
      Submit -> do
        draft <- use form
        case validate draft of
          Left errs -> errors .= errs
          Right b   -> do
            mailParent b               -- b has a ToJSON instance
            form .= emptyDraft

      -- in the app
      app = (component m update viewApp)
        { mailbox = checkMail BookmarkAdded MailError
        , mount   = Just Init }
      """
    , para [ "The child stays reusable (it knows nothing about bookmarks lists) and the app stays in control of its own state." ]
    , h2 "Routing"
    , para
      [ "Make the selected bookmark a URL: ", c "/bookmarks/2", ". Derive a ", c "Router", " from a sum type, subscribe with ", c "routerSub", " and treat navigation as just another action — see ", goto (docsPage "routing") [ "Routing" ], ". "
      , "Links stay real ", c "<a href>", " elements (crawlable, middle-clickable) with an ", c "onClickPrevent", " that pushes the route." ]
    , h2 "Prerender and hydrate"
    , para
      [ "Because ", c "view", " is pure, the same code renders HTML on a server or at build time (", c "toHtml", "), and the client ", em "hydrates", " it with ", c "miso", " instead of redrawing. Users see content before the WASM loads; search engines see everything. See ", goto (docsPage "html-and-prerendering") [ "HTML & prerendering" ], "." ]
    , h2 "Take it to mobile"
    , para
      [ "Nothing above mentioned the DOM. Swap ", c "Miso.Html.Element", " for ", c "Miso.Native.Element", ", ", c "startApp", " for ", c "native", ", and the model, update, actions and data flow carry over to iOS and Android through Lynx — see ", goto (nativePage "overview") [ "miso native" ], "." ]
    , h2 "Recap"
    , ol
      [ [ "Model: minimal state, derived values in ", c "view", ", sum types over booleans." ]
      , [ "Views by default; components for owned state, subs and lifecycle. Keys on lists." ]
      , [ "Actions are events; ", c "update", " is a pure fold; IO is scheduled at the edge." ]
      , [ "Props down, mail up, context around, PubSub sideways. Route, prerender, ship." ]
      ]
    , para [ "That is all of miso's architecture. The rest is the ", goto Docs [ "API" ], "." ]
    ]
  }
-----------------------------------------------------------------------------
