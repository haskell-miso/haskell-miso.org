-----------------------------------------------------------------------------
-- | The blog: an index and one component per post. Posts are written with
-- the same "Site.Prose" vocabulary as the docs.
--
-- Post content is translatable: a 'Post' carries one 'PostContent' per
-- language, and 'localized' picks the version for the context's active
-- language, falling back to English. Because the blog components have
-- @useContext = True@, switching the language in the top bar re-renders
-- the post in place.
module Site.Blog
  ( Post (..)
  , PostContent (..)
  , allPosts
  , lookupPost
  , localized
  , english
  , blogIndex
  , blogPostPage
  ) where
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
import           Site.I18n
import           Site.Prose
import           Site.Route
import           Site.Types
-----------------------------------------------------------------------------
data Post
  = Post
  { postSlug     :: MisoString
  , postDate     :: MisoString
  -- ^ ISO date, also shown verbatim
  , postVersions :: [(Lang, PostContent)]
  -- ^ One version per language; 'EN' must be present and comes first.
  }
-----------------------------------------------------------------------------
data PostContent
  = PostContent
  { postTitle :: MisoString
  , postBlurb :: MisoString
  , postBody  :: [Doc]
  }
-----------------------------------------------------------------------------
-- | The version for the context's language, falling back to English.
localized :: Ctx -> Post -> PostContent
localized Ctx {..} p =
  case lookup ctxLang (postVersions p) of
    Just content -> content
    Nothing      -> english p
-----------------------------------------------------------------------------
-- | The English version (used by the prerenderer, RSS and search).
english :: Post -> PostContent
english p =
  case lookup EN (postVersions p) of
    Just content -> content
    Nothing ->
      case postVersions p of
        ((_, content):_) -> content
        [] -> PostContent (postSlug p) "" []
-----------------------------------------------------------------------------
allPosts :: [Post]
allPosts =
  [ siteTour
  , newBlog
  ]
-----------------------------------------------------------------------------
lookupPost :: MisoString -> Maybe Post
lookupPost slug =
  case [ p | p <- allPosts, postSlug p == slug ] of
    (p:_) -> Just p
    []    -> Nothing
-----------------------------------------------------------------------------
blogIndex :: Component Ctx () () Nav
blogIndex = (component () navigate view) { useContext = True }
  where
    view () = vcontext $ \ctx ->
      H.div_ [ P.class_ "blog page" ]
        [ H.header_ [ P.class_ "page-head" ]
            [ H.h1_ [] [ t ctx BlogTitle ]
            , H.p_ [ P.class_ "lead" ] [ t ctx BlogSubtitle ]
            ]
        , H.ul_ [ P.class_ "post-list" ]
            [ H.li_ [ P.class_ "post-item", key_ (postSlug p) ]
                [ H.time_ [ P.class_ "post-date" ] [ text (postDate p) ]
                , H.h2_ []
                    [ H.a_ [ P.href_ (routeHref (blogPost (postSlug p))), E.onClickPrevent (Go (blogPost (postSlug p))) ]
                        [ text (postTitle (localized ctx p)) ]
                    ]
                , H.p_ [] [ text (postBlurb (localized ctx p)) ]
                , H.a_
                    [ P.class_ "post-read", P.href_ (routeHref (blogPost (postSlug p))), E.onClickPrevent (Go (blogPost (postSlug p))) ]
                    [ t ctx BlogRead, " →" ]
                ]
            | p <- allPosts
            ]
        ]
-----------------------------------------------------------------------------
blogPostPage :: Post -> Component Ctx () () Nav
blogPostPage p = (component () navigate view) { useContext = True }
  where
    view () = vcontext $ \ctx ->
      let PostContent {..} = localized ctx p
      in H.article_ [ P.class_ "post page" ]
          [ H.a_ [ P.class_ "post-back", P.href_ (routeHref Blog), E.onClickPrevent (Go Blog) ] [ "← ", t ctx BlogBack ]
          , H.header_ [ P.class_ "post-head" ]
              [ H.time_ [ P.class_ "post-date" ] [ text (postDate p) ]
              , H.h1_ [] [ text postTitle ]
              ]
          , H.div_ [ P.class_ "doc-body" ] postBody
          ]
-----------------------------------------------------------------------------
-- A tour of the new site ------------------------------------------------------
-----------------------------------------------------------------------------
siteTour :: Post
siteTour = Post
  { postSlug = "a-tour-of-the-new-site"
  , postDate = "2026-10-22"
  , postVersions = [ (EN, postEn) ]
  }
  where
    postEn = PostContent
      { postTitle = "A tour of the new haskell-miso.org"
      , postBlurb = "The site is rebuilt in miso itself: 41 pages of docs with live examples, a native section, 55 examples, full-text search and docs your AI tools can read."
      , postBody =
        [ lead [ "haskell-miso.org has been rebuilt from the ground up, in miso. Every page is a miso ", c "Component", ", prerendered to static HTML and then hydrated by the same code compiled to WebAssembly. Here is a tour of what is new." ]
        , h2 "Documentation with live examples"
        , para [ "The ", goto Docs [ "documentation" ], " now runs to 41 pages, from ", goto (docsPage "your-first-component") [ "your first Component" ], " through effects, subscriptions, routing and prerendering, all the way down to the ", goto (docsPage "internals") [ "internals" ], ". "
               , "Twelve of them include a ", b "live example", ": a running component next to its source. The source shown is the code that is running, and CI checks that the two never drift apart." ]
        , h2 "Thinking in miso"
        , para [ goto (thinkingPage "overview") [ "Thinking in miso" ], " is a guided walk-through that takes an application from a mockup to a working program: design the model, split the view, define the actions, then connect the pieces and ship. "
               , "If you are coming from React, ", goto (thinkingPage "miso-vs-react") [ "miso vs. React" ], " maps hooks onto their miso counterparts, and points out the names that look alike but are not." ]
        , h2 "Native apps"
        , para [ "miso drives the ", a "https://lynxjs.org" "Lynx", " runtime to build native iOS and Android apps from the same components you write for the web. The ", goto (nativePage "overview") [ "native section" ], " explains how it works: the ", goto (nativePage "dual-thread") [ "dual-thread architecture" ], ", static mounting, main-thread events and state, and a ", goto (nativePage "minimal-component") [ "minimal native component" ], " to start from." ]
        , h2 "Examples"
        , para [ "The ", goto Examples [ "examples page" ], " collects 55 applications and libraries from the ", a "https://github.com/haskell-miso" "haskell-miso organisation", ": games, browser API demos, integrations with JavaScript libraries, and tooling. Most of them have a live demo one click away." ]
        , h2 "Finding things"
        , para [ "Press ", c "⌘K", " (or ", c "Ctrl+K", ") on any page to search. Search covers the full text of the documentation, so a function name such as ", c "startSub", " finds every page that uses it, not just the page named after it." ]
        , h2 "Docs your tools can read"
        , para [ "Every documentation page also has a Markdown copy, linked from the page and listed in ", a "/llms.txt" "llms.txt", ". The whole documentation is in a single file, ", a "/llms-full.txt" "llms-full.txt", ", ready to hand to an AI assistant. All of it is generated from the same source as the pages, so it is never out of date." ]
        , h2 "Get involved"
        , para [ "The site's source is at ", a "https://github.com/haskell-miso/haskell-miso.org" "haskell-miso/haskell-miso.org", ", and issues and pull requests are welcome, whether it is a typo or a new docs page. "
               , "Questions and show-and-tell go to ", a "https://discord.gg/QVDtfYNSxq" "Discord", " or ", a "https://matrix.to/#/#haskell-miso:matrix.org" "Matrix", "." ]
        ]
      }
-----------------------------------------------------------------------------
-- New blog -------------------------------------------------------------------
-----------------------------------------------------------------------------
newBlog :: Post
newBlog = Post
  { postSlug = "new-blog"
  , postDate = "2026-08-18"
  , postVersions = [ (EN, postEn) ]
  }
  where
    postEn = PostContent
      { postTitle = "New blog"
      , postBlurb = "Welcome to the new home of haskell-miso.org. More blog posts coming soon."
      , postBody =
        [ lead [ "Welcome to the new home of ", a "https://haskell-miso.org" "haskell-miso.org", ". More blog posts coming soon." ]
        , para [ "Please see the ", goto (nativePage "overview") [ "native section" ], " and the ", a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", " for the latest on mobile application development." ]
        ]
      }
-----------------------------------------------------------------------------
