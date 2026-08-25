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
  [ newBlog
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
    view ctx () () =
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
        , H.p_ [ P.class_ "blog-archive" ]
            [ H.a_ [ P.href_ "https://blog.haskell-miso.org", P.target_ "_blank", P.rel_ "noopener" ] [ t ctx BlogArchive, " ↗" ] ]
        ]
-----------------------------------------------------------------------------
blogPostPage :: Post -> Component Ctx () () Nav
blogPostPage p = (component () navigate view) { useContext = True }
  where
    view ctx () () =
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
