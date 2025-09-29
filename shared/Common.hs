-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-----------------------------------------------------------------------------
module Common where
-----------------------------------------------------------------------------
import           Data.Bool
import           Data.Proxy
import           Servant.API hiding (URI(..))
import           Servant.Links hiding (URI(..))
-----------------------------------------------------------------------------
import           Miso hiding (URI(..))
import           Miso.Lens
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import qualified Miso.Router as R
import qualified Miso.CSS as CSS
-----------------------------------------------------------------------------
import           Servant.Miso.Router
-----------------------------------------------------------------------------
{- | We can pretty much share everything
   | model, action, view, router, links, events map
   | decoders are all shareable.
   |
-}
-----------------------------------------------------------------------------
data Model
  = Model
  { _uri :: R.URI
  , _navMenuOpen :: Bool
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
uri :: Lens Model R.URI
uri = lens _uri $ \record field -> record { _uri = field }
-----------------------------------------------------------------------------
navMenuOpen :: Lens Model Bool
navMenuOpen = lens _navMenuOpen $ \record field -> record { _navMenuOpen = field }
-----------------------------------------------------------------------------
data Action
  = ChangeURI R.URI
  | HandleURI R.URI
  | ToggleNavMenu
  deriving (Show, Eq)
-----------------------------------------------------------------------------
type Home a = a
type Docs a = "docs" :> a
type The404 a = "404" :> a
type Examples a = "examples" :> a
type Community a = "community" :> a
-----------------------------------------------------------------------------
-- | Routes skeleton
type Routes a =
  Examples a
    :<|> Docs a
    :<|> Community a
    :<|> Home a
    :<|> The404 a
-----------------------------------------------------------------------------
type ClientRoutes = Routes (View Model Action)
-----------------------------------------------------------------------------
type HaskellMisoComponent = App Model Action
-----------------------------------------------------------------------------
uriHome, uriExamples, uriDocs, uriCommunity, uri404 :: R.URI
uriExamples :<|> uriDocs :<|> uriCommunity :<|> uriHome :<|> uri404 =
  allLinks' toMisoURI (Proxy @ClientRoutes)
-----------------------------------------------------------------------------
newtype Page = Page HaskellMisoComponent
-----------------------------------------------------------------------------
clientHandlers
  :: (Model -> View Model Action)
     :<|> (Model -> View Model Action)
     :<|> (Model -> View Model Action)
     :<|> (Model -> View Model Action)
     :<|> (Model -> View Model Action)
clientHandlers =
  examples
    :<|> docs
    :<|> community
    :<|> home
    :<|> the404
-----------------------------------------------------------------------------
secs :: Int -> Int
secs = (*1000000)
-----------------------------------------------------------------------------
haskellMisoComponent :: R.URI -> HaskellMisoComponent
haskellMisoComponent uri_ = (haskellMiso uri_)
  { subs = [ uriSub HandleURI ]
  }
-----------------------------------------------------------------------------  
haskellMiso :: R.URI -> App Model Action
haskellMiso currentUri = component emptyModel updateModel viewModel
  where
    emptyModel = Model currentUri False
    viewModel m =
      case route (Proxy :: Proxy ClientRoutes) clientHandlers _uri m of
        Left _ -> the404 m
        Right view_ -> view_
-----------------------------------------------------------------------------
updateModel :: Action -> Transition Model Action
updateModel = \case
  HandleURI u -> do
    uri .= u
  ChangeURI u -> do
    navMenuOpen .= False
    io_ (pushURI u)
  ToggleNavMenu ->
    navMenuOpen %= not
-----------------------------------------------------------------------------
-- | Views
community :: Model -> View Model Action
community = template $
  H.div_
  [ P.class_ "animated fadeIn"
  ]
  [ H.a_
    [ P.href_ "https://github.com/dmjio/miso"
    ]
    [ H.img_
      [ P.width_ "100"
      , P.class_ "animated bounceInDown"
      , P.src_ misoSrc
      , P.alt_ "miso logo"
      ]
    ]
  , H.h1_
    [ P.class_ "title animated pulse"
    , CSS.style_
      [ CSS.fontSize "82px"
      , CSS.fontWeight "100"
      ]
    ]
    [ "community" ]
  , H.h2_
    [ P.class_ "subtitle animated pulse" ]
    [ H.a_
      [ P.href_ "https://github.com/haskell-miso"
      , P.target_ "_blank"
      ]
      [ "GitHub"
      ]
    , text " / "
    , H.a_
      [ P.href_ "https://matrix.to/#/#haskell-miso:matrix.org"
      , P.target_ "_blank"
      ]
      [ text "Matrix.org"
      ]
    , text " / "
    , H.a_
      [ P.href_ "https://www.irccloud.com/invite?channel=%23haskell-miso&hostname=irc.libera.chat&port=6697&ssl=1"
      , P.target_ "_blank"
      ]
      [ "#haskell-miso"
      ]
    , text " / "
    , H.a_
      [ P.href_ "https://discord.gg/QVDtfYNSxq"
      , P.target_ "_blank"
      ]
      [ "Discord"
      ]
    ]
  ]
-----------------------------------------------------------------------------
docs :: Model -> View Model Action
docs = template $
  H.div_
  [ P.class_ "animated fadeIn" ]
  [ H.a_
    [ P.href_ "https://github.com/dmjio/miso" ]
    [ H.img_
      [ P.width_ "100"
      , P.class_ "animated bounceInDown"
      , P.src_ misoSrc
      , P.alt_ "miso logo"
      ]
    ]
  , H.h1_
    [ P.class_ "title animated pulse"
    , CSS.style_
      [ CSS.fontSize "82px"
      , CSS.fontWeight "100"
      ]
    ]
    [ "docs" ]
  , H.h2_
    [ P.class_ "subtitle animated pulse" ]
    [ H.a_
      [ P.href_ "http://haddocks.haskell-miso.org/"
      , P.target_ "_blank"
      ]
      [ "Haddocks" ]
    , text " / "
    , H.a_
      [ P.href_ "https://github.com/dmjio/miso/blob/master/README.md"
      , P.target_ "_blank"
      ]
      [ "README" ]
    ]
  ]
-----------------------------------------------------------------------------
misoSrc :: MisoString
misoSrc = "miso.png"
-----------------------------------------------------------------------------
examples :: Model -> View Model Action
examples = template $
  H.div_
  [ P.class_ "animated fadeIn" ]
  [ H.a_
    [ P.href_ "https://github.com/dmjio/miso" ]
    [ H.img_
      [ P.width_ "100"
      , P.class_ "animated bounceInDown"
      , P.src_ misoSrc
      , P.alt_ "miso logo"
      ]
    ]
  , H.h1_
    [ P.class_ "title animated pulse"
    , CSS.style_
      [ CSS.fontSize "82px"
      , CSS.fontWeight "100"
      ]
    ]
    [ "examples" ]
  , H.div_
    [ P.class_ "subtitle animated pulse"
    ]
    [ H.a_
      [ P.target_ "_blank"
      , P.href_ "https://threejs.haskell-miso.org"
      ]
      [ "three.js"
      ]
    , text " / "
    , H.a_
      [ P.target_ "_blank"
      , P.href_ "https://todomvc.haskell-miso.org"
      ]
      [ text "TodoMVC"
      ]
    , text " / "
    , H.a_
      [ P.target_ "_blank"
      , P.href_ "https://mario.haskell-miso.org/"
      ]
      [ "Mario" ]
    , text " / "
    , H.a_
      [ P.target_ "_blank"
      , P.href_ "https://flatris.haskell-miso.org/"
      ]
      [ "Flatris" ]
    , text " / "
    , H.a_
      [ P.target_ "_blank"
      , P.href_ "https://2048.haskell-miso.org/"
      ]
      [ "2048" ]
    ]
  ]
-----------------------------------------------------------------------------
home :: Model -> View Model Action
home = template $
  H.div_
  [ P.class_ "animated fadeIn" ]
  [ H.a_
    [ P.href_ "https://github.com/dmjio/miso" ]
    [ H.img_
      [ P.width_ "100"
      , P.class_ "animated bounceInDown"
      , P.src_ misoSrc
      , P.alt_ "miso logo"
      ]
    ]
  , H.h1_
    [ P.class_ "title animated pulse"
    , CSS.style_
      [ CSS.fontSize "82px"
      , CSS.fontWeight "100"
      ]
    ]
    [ "miso" ]
  , H.h2_
    [ P.class_ "subtitle animated pulse" ]
    [ text "A tasty "
    , H.a_
      [ P.href_ "https://www.haskell.org/"
      , P.rel_ "noopener"
      , P.target_ "_blank"
      ]
      [ H.strong_ [] [text "Haskell"]
      ]
    , text " web and mobile framework"
    ]
  ]
-----------------------------------------------------------------------------
githubStar :: View model action
githubStar = H.iframe_
    [ P.title_ "GitHub"
    , P.height_ "30"
    , P.width_ "170"
    , P.scrolling_ "0"
    , P.frameborder_ "0"
    , P.src_ "https://ghbtns.com/github-btn.html?user=haskell-miso&repo=miso-websocket&type=star&count=true&size=large"
    ]
    []
-----------------------------------------------------------------------------
template :: View Model Action -> Model -> View Model Action
template content m =
  H.div_
  []
  [ githubStar
  , hero content (m ^. uri) (m ^. navMenuOpen)
  , middle
  , footer
  ]
-----------------------------------------------------------------------------
middle :: View Model action
middle =
  H.section_
  [ P.class_ "hero" ]
  [ H.div_
    [ P.class_ "hero-body" ]
    [ H.div_
      [ P.class_ "container" ]
      [ H.nav_
        [ P.class_ "columns" ]
        [ H.a_
          [ P.class_ "column has-text-centered"
          , P.href_ "https://krausest.github.io/js-framework-benchmark/2024/table_chrome_130.0.6723.58.html"
          , P.target_ "_blank"
          , P.rel_ "noopener"
          ]
          [ H.span_
            [P.class_ "icon is-large"]
            [ H.i_ [P.class_ "fa fa-flash"] []
            ]
          , H.p_
            [ P.class_ "title is-4" ]
            [ H.strong_ [] [text "Fast"]
            ]
          , H.p_
            [P.class_ "subtitle"]
            [ text "Virtual DOM"
            ]
          ]
        , H.a_
          [ P.class_ "column has-text-centered"
          , P.href_ "https://en.wikipedia.org/wiki/Isomorphic_JavaScript"
          , P.target_ "_blank"
          , P.rel_ "noopener"
          ]
          [ H.span_
            [ P.class_ "icon is-large" ]
            [ H.i_ [P.class_ "fa fa-refresh"] []
            ]
          , H.p_
            [P.class_ "title is-4"]
            [ H.strong_ [] [text "Isomorphic"]
            ]
          , H.p_
            [P.class_ "subtitle"]
            [text "Seamless experience"]
          ]
        , H.a_
          [ P.class_ "column has-text-centered"
          , P.target_ "_blank"
          , P.href_ "http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html"
          , P.rel_ "noopener"
          ]
          [ H.span_
            [P.class_ "icon is-large"]
            [ H.i_ [P.class_ "fa fa-gears"] []
            ]
          , H.p_
            [P.class_ "title is-4"]
            [ H.strong_ [] [text "Concurrent"]
            ]
          , H.p_
            [P.class_ "subtitle"]
            [ text "Multi-threaded apps"
            ]
          ]
        , H.a_
          [ P.class_ "column has-text-centered"
          , P.href_ "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#javascript-ffi-in-the-wasm-backend"
          , P.rel_ "noopener"
          , P.target_ "_blank"
          ]
          [ H.span_
            [P.class_ "icon is-large"]
            [ H.i_ [P.class_ "fa fa-code-fork"] []
            ]
          , H.p_
            [P.class_ "title is-4"]
            [ H.strong_ [] [text "Interoperable"]
            ]
          , H.p_
            [P.class_ "subtitle"]
            [ text "via the FFI"
            ]
          ]
        , H.a_
          [ P.class_ "column has-text-centered"
          , P.href_ "https://github.com/haskell-miso/miso-lynx"
          , P.rel_ "noopener"
          , P.target_ "_blank"
          ]
          [ H.span_
            [P.class_ "icon is-large"]
            [ H.i_ [P.class_ "fa fa-mobile"] []
            ]
          , H.p_
            [P.class_ "title is-4"]
            [ H.strong_ [] [text "Cross Platform"]
            ]
          , H.p_
            [ P.class_ "subtitle" ]
            [ text "iOS, Android, HarmonyOS"
            ]
          ]
        ]
      ]
    ]
  ]

cols :: View Model action
cols =
  H.section_
  []
  [ H.div_
    [ P.class_ "container" ]
    [ H.div_
      [ P.class_ "columns" ]
      [ H.div_
        [ P.class_ "column" ]
        [ H.h1_
          [ P.class_ "title" ]
          [ H.span_
            [ P.class_ "icon is-large"]
            [ H.i_
              [ P.class_ "fa fa-flash"
              ]
              []
            ]
          , text "Fast"
          ]
        , H.h2_
          [ P.class_ "subtitle" ]
          [ text "Mutable virtual dom implementation"
          ]
        ]
      , H.div_
        [ P.class_ "column" ]
        [ text "Second column"
        ]
      , H.div_
        [ P.class_ "column" ]
        [ text "Third column"
        ]
      , H.div_
        [ P.class_ "column" ]
        [ text "Fourth column"
        ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
the404 :: Model -> View Model Action
the404 = template $
  H.div_
  []
  [ H.a_
    [ P.href_ "https://github.com/dmjio/miso" ]
    [ H.img_
      [ P.width_ "100"
      , P.class_ "animated bounceOutUp"
      , P.src_ misoSrc
      , P.alt_ "miso logo"
      ]
    ]
  , H.h1_
    [ P.class_ "title"
    , CSS.style_
      [ CSS.fontSize "82px"
      , CSS.fontWeight "100"
      ]
    ]
    [text "404"]
  , H.h2_
    [P.class_ "subtitle animated pulse"]
    [ text "No soup for you! "
    , H.a_ [P.href_ "/", onPreventClick (ChangeURI uriHome)] [text " - Go Home"]
    ]
  ]
-----------------------------------------------------------------------------
-- | Github stars
starMiso :: View Model action
starMiso =
  H.a_
  [ P.class_ "github-button"
  , P.href_ "https://github.com/dmjio/miso"
  , P.data_ "icon" "octicon-star"
  , P.data_ "size" "large"
  , P.data_ "show-count" "true"
  , P.aria_ "label" "Star dmjio/miso on GitHub"
  ]
  [ "Star"
  ]
-----------------------------------------------------------------------------
forkMiso :: View Model action
forkMiso =
  H.a_
  [ P.class_ "github-button"
  , P.href_ "https://github.com/dmjio/miso/fork"
  , P.data_ "icon" "octicon-repo-forked"
  , P.data_ "size" "large"
  , P.data_ "show-count" "true"
  , P.aria_ "label" "Fork dmjio/miso on GitHub"
  ]
  [ "Fork" ]
-----------------------------------------------------------------------------
-- | Hero
hero :: View Model Action -> R.URI -> Bool -> View Model Action
hero content uri' navMenuOpen' =
  H.section_
  [ P.class_ "hero is-medium is-primary is-bold has-text-centered" ]
  [ H.div_
    [ P.class_ "hero-head"
    ]
    [ H.header_
      [ P.class_ "nav"
      ]
      [ H.div_
        [ P.class_ "container"
        ]
        [ H.div_
          [ P.class_ "nav-left" ]
          [ H.a_ [P.class_ "nav-item"] []
          ]
        , H.span_
          [ P.class_ $ "nav-toggle " <> bool mempty "is-active" navMenuOpen'
          , E.onClick ToggleNavMenu
          ]
          [ H.span_ [] []
          , H.span_ [] []
          , H.span_ [] []
          ]
        , H.div_
          [ P.class_ $ "nav-right nav-menu " <> bool mempty "is-active" navMenuOpen' ]
          [ H.div_
            [ P.className "nav-item"
            ]
            [ H.a_
              [ P.href_ (ms uriHome)
              , onPreventClick (ChangeURI uriHome)
              , P.classList_
                [ ("is-active", R.uriPath uri' == R.uriPath uriHome)
                ]
              ]
              [ "Home"
              ]
            ]
          , H.div_
            [ P.className "nav-item"
            ]
            [ H.a_
              [ P.href_ (ms uriExamples)
              , onPreventClick (ChangeURI uriExamples)
              , P.classList_ [("is-active", R.uriPath uri' == R.uriPath uriExamples)]
              ]
              [ "Examples" ]
            ]
          , H.div_
            [ P.className "nav-item"
            ]
            [ H.a_
              [ P.href_ (ms uriDocs)
              , onPreventClick (ChangeURI uriDocs)
              , P.classList_
                [ ("is-active", R.uriPath uri' == R.uriPath uriDocs)
                ]
              ]
              [ "Docs" ]
            ]
          , H.div_
            [ P.className "nav-item"
            ]
            [ H.a_
              [ P.href_ (ms uriCommunity)
              , onPreventClick (ChangeURI uriCommunity)
              , P.classList_
                [ ("is-active", R.uriPath uri' == R.uriPath uriCommunity)
                ]
              ]
              [ "Community" ]
            ]
          ]
        ]
      ]
    ]
  , H.div_
    [ P.class_ "hero-body" ]
    [ H.div_
      [ P.class_ "container" ]
      [ content
      ]
    ]
  ]
-----------------------------------------------------------------------------
onPreventClick :: Action -> Attribute Action
onPreventClick = E.onClickWithOptions preventDefault
-----------------------------------------------------------------------------
-- | Footer
footer :: View Model action
footer =
  H.footer_
  [ P.class_ "footer" ]
  [ H.div_
    [ P.class_ "container" ]
    [ H.div_
      [ P.class_ "content has-text-centered" ]
      [ H.p_
        []
        [ H.strong_ []
          [ "Miso"
          ]
        , " by "
        , H.a_
          [ P.href_ "https://github.com/dmjio/miso"
          , CSS.style_ [ CSS.color (CSS.hex #363636) ]
          ]
          [ "dmjio" ]
        , text ". BSD3"
        , H.a_
          [ P.href_ "https://opensource.org/licenses/BSD-3-Clause"
          , CSS.style_ [ CSS.color (CSS.hex #363636) ]
          ]
          [ " licensed."]
        ]
      , H.p_
        []
        [ text "The source code for this website is located "
        , H.a_
          [ P.href_ "https://github.com/haskell-miso/haskell-miso.org"
          , CSS.style_ [ CSS.color (CSS.hex #363636) ]
          ]
          [" here."]
        ]
      , H.p_
        []
        [ H.a_
          [ P.href_ "https://bulma.io"]
          [ H.img_
            [ P.src_ "https://bulma.io/assets/images/made-with-bulma.png"
            , P.alt_ "Made with Bulma"
            , P.width_ "128"
            , P.height_ "24"
            ]
          ]
        ]
      , H.p_
        []
        [ H.a_
          [ P.href_ "https://github.com/dmjio/miso" ]
          [ H.span_
            [P.class_ "icon is-large"]
            [ H.i_ [P.class_ "fa fa-github"] []
            ]
          ]
        ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
newNav :: Bool -> View Model Action
newNav navMenuOpen' =
  H.div_
  [ P.class_ "container" ]
  [ H.nav_
    [ P.class_ "navbar is-transparent" ]
    [ H.div_
      [ P.class_ "navbar-brand" ]
      [ H.a_
        [ P.class_ "navbar-item"
        , P.href_ "https://haskell-miso.org"
        ]
        [ "miso"
        , H.a_
          [ P.class_ "navbar-item is-hidden-desktop"
          , P.href_ "https://github.com/dmjio/miso"
          , P.target_ "_blank"
          , P.rel_ "noopener"
          , P.name_ "miso"
          ]
          [ H.span_
            [ P.class_ "icon"
            , P.name_ "github"
            , CSS.style_ [ CSS.color (CSS.hex #333) ]
            ]
            [ H.i_
              [ P.class_ "fa fa-github" ]
              []
            ]
          ]
        , H.a_
          [ P.class_ "navbar-item is-hidden-desktop"
          , P.href_ "https://twitter.com/dmjio"
          , P.rel_ "noopener"
          , P.target_ "_blank"
          ]
          [ H.span_
            [ P.class_ "icon"
            , P.name_ "twitter"
            , CSS.style_ [ CSS.color (CSS.hex #55acee) ]
            ]
            [ H.i_
              [ P.class_ "fa fa-twitter" ]
              []
            ]
          ]
        , H.div_
          [ P.class_ ("navbar-burger burger " <> bool mempty "is-active" navMenuOpen')
          , P.data_ "target" "navMenuIndex"
          , E.onClick ToggleNavMenu
          ]
          [ H.span_ [] []
          , H.span_ [] []
          , H.span_ [] []
          ]
        ]
      , H.div_
        [ P.id_ "navMenuIndex"
        , P.class_ $ "navbar-menu " <> bool mempty "is-active" navMenuOpen'
        ]
        [ H.div_
          [ P.class_ "navbar-start" ]
          [ H.a_
            [ P.class_ "navbar-item is-active"
            , P.href_ "https://haskell-miso.org"
            ]
            [ "Home"
            ]
          , H.div_
            [ P.class_ "navbar-item has-dropdown is-hoverable" ]
            [ H.a_
              [ P.class_ "navbar-link"
              , P.href_ "/documentation/overview/start/"
              ]
              [ "Docs"
              ]
            , H.div_
              [ P.class_ "navbar-dropdown is-boxed" ]
              [ H.a_
                [ P.class_ "navbar-item "
                , P.href_ "/documentation/overview/start/"
                ]
                [ text "Overview"
                ]
              , H.a_
                [ P.class_ "navbar-item "
                , P.href_ "http://bulma.io/documentation/modifiers/syntax/"
                ]
                [ "Modifiers"
                ]
              , H.a_
                [ P.class_ "navbar-item "
                , P.href_ "http://bulma.io/documentation/grid/columns/"
                ]
                [ "Grid"
                ]
              , H.a_
                [ P.class_ "navbar-item "
                , P.href_ "http://bulma.io/documentation/form/general/"
                ]
                [ text "Form"
                ]
              , H.a_
                [ P.class_ "navbar-item "
                , P.href_ "http://bulma.io/documentation/elements/box/"
                ]
                [ text "Elements"
                ]
              , H.a_
                [ P.class_ "navbar-item "
                , P.href_ "http://bulma.io/documentation/components/breadcrumb/"
                ]
                [ "Components" ]
              , H.a_
                [ P.class_ "navbar-item "
                , P.href_ "http://bulma.io/documentation/layout/container/"
                ]
                [ "Layout" ]
              , H.hr_ [P.class_ "navbar-divider"]
              , H.div_
                [ P.class_ "navbar-item" ]
                [ H.div_
                  []
                  [ H.p_
                    [P.class_ "has-text-info is-size-6-desktop"]
                    [ H.strong_ [] [text "0.4.4"]
                    ]
                  , H.small_
                    []
                    [ H.a_
                      [P.class_ "view-all-versions", P.href_ "/versions"]
                      [ "View all versions"
                      ]
                    ]
                  ]
                ]
              ]
            ]
          , H.div_
            [ P.class_ "navbar-item has-dropdown is-hoverable" ]
            [ H.a_
              [ P.class_ "navbar-link"
              , P.href_ "http://bulma.io/blog/"
              ]
              [ "Blog"
              ]
            , H.div_
              [ P.id_ "blogDropdown"
              , P.class_ "navbar-dropdown is-boxed"
              , P.data_ "style_" "width: 18rem;"
              ]
              [ H.a_
                [ P.class_ "navbar-item"
                , P.href_ "/2017/07/24/access-previous-bulma-versions/"
                ]
                [ H.div_
                  [ P.class_ "navbar-content" ]
                  [ H.p_
                    []
                    [ H.small_
                      [ P.class_ "has-text-info" ]
                      [ text "24 Jul 2017"
                      ]
                    ]
                  , H.p_ [] [ "Access previous Bulma versions" ]
                  ]
                ]
              , H.a_
                [ P.class_ "navbar-item"
                , P.href_ "/2017/03/10/new-field-element/"
                ]
                [ H.div_
                  [ P.class_ "navbar-content" ]
                  [ H.p_
                    []
                    [ H.small_
                      [ P.class_ "has-text-info" ]
                      [ "10 Mar 2017"
                      ]
                    ]
                  , H.p_
                    []
                    [ "New field element (for better controls)"
                    ]
                  ]
                ]
              , H.a_
                [ P.class_ "navbar-item"
                , P.href_ "/2016/04/11/metro-ui-css-grid-with-bulma-tiles/"
                ]
                [ H.div_
                  [ P.class_ "navbar-content" ]
                  [ H.p_
                    []
                    [ H.small_
                      [ P.class_ "has-text-info" ]
                      [ "11 Apr 2016"
                      ]
                    ]
                  , H.p_
                    []
                    [ "Metro UI CSS grid with Bulma tiles"
                    ]
                  ]
                ]
              , H.a_
                [ P.class_ "navbar-item"
                , P.href_ "http://bulma.io/blog/"
                ]
                [ "More posts"
                ]
              , H.hr_ [ P.class_ "navbar-divider" ]
              , H.div_
                [ P.class_ "navbar-item" ]
                [ H.div_
                  [ P.class_ "navbar-content" ]
                  [ H.div_
                    [ P.class_ "level is-mobile" ]
                    [ H.div_
                      [ P.class_ "level-left" ]
                      [ H.div_
                        [ P.class_ "level-item" ]
                        [ H.strong_ []
                          [ text "Stay up to date!" ]
                        ]
                      ]
                    , H.div_
                      [ P.class_ "level-right" ]
                      [ H.div_
                        [ P.class_ "level-item" ]
                        [ H.a_
                          [ P.class_ "button is-rss is-small"
                          , P.href_ "http://bulma.io/atom.xml"
                          ]
                          [ H.span_
                            [ P.class_ "icon is-small" ]
                            [ H.i_
                              [ P.class_ "fa fa-rss"
                              ]
                              []
                            ]
                          , H.span_
                            []
                            [ "Subscribe"
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          , H.div_
            [ P.class_ "navbar-item has-dropdown is-hoverable" ]
            [ H.div_
              [ P.class_ "navbar-link" ]
              [ text "More"
              ]
            , H.div_
              [ P.id_ "moreDropdown"
              , P.class_ "navbar-dropdown is-boxed"
              ]
              [ H.a_
                [ P.class_ "navbar-item"
                , P.href_ "http://bulma.io/extensions/"
                ]
                [ H.div_
                  [ P.class_ "level is-mobile" ]
                  [ H.div_
                    [ P.class_ "level-left" ]
                    [ H.div_
                      [ P.class_ "level-item" ]
                      [ H.p_
                        []
                        [ H.strong_
                          []
                          [ "Extensions"
                          ]
                        , H.br_ []
                        , H.small_
                          []
                          [ "Side projects to enhance Bulma"
                          ]
                        ]
                      ]
                    ]
                  , H.div_
                    [P.class_ "level-right"]
                    [ H.div_
                      [P.class_ "level-item"]
                      [ H.span_
                        [P.class_ "icon has-text-info"]
                        [ H.i_ [P.class_ "fa fa-plug"] []
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        , H.div_
          [P.class_ "navbar-end"]
          [ H.a_
            [ P.class_ "navbar-item"
            , P.href_ "https://github.com/dmjio/miso"
            , P.target_ "_blank"
            ]
            [ text "Github"
            ]
          , H.a_
            [ P.class_ "navbar-item"
            , P.href_ "https://twitter.com/dmjio"
            , P.target_ "_blank"
            ]
            [ text "Twitter"
            ]
          , H.div_
            [ P.class_ "navbar-item" ]
            [ H.div_
              [ P.class_ "field is-grouped" ]
              [ H.p_
                [ P.class_ "control" ]
                [ H.a_
                  [ P.id_ "twitter"
                  , P.class_ "button"
                  , P.data_ "social-network_" "Twitter"
                  , P.data_ "social-action_" "tweet"
                  , P.data_ "social-target" "http://bulma.io"
                  , P.target_ "_blank"
                  , P.href_ "https://bulma.io"
                  ]
                  [ H.span_
                    [P.class_ "icon"]
                    [ H.i_ [P.class_ "fa fa-twitter"] []
                    ]
                  , H.span_ [] [text "Tweet"]
                  ]
                ]
              , H.p_
                [P.class_ "control"]
                [ starMiso
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
-----------------------------------------------------------------------------
