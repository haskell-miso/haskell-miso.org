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
  [ longLiveHaskell
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
-- Haskell is dead, long live Haskell ----------------------------------------
-----------------------------------------------------------------------------
longLiveHaskell :: Post
longLiveHaskell = Post
  { postSlug = "haskell-is-dead-long-live-haskell"
  , postDate = "2026-08-18"
  , postVersions =
    [ (EN, postEn), (ZH, postZh), (ES, postEs), (FR, postFr)
    , (DE, postDe), (PT, postPt), (JA, postJa), (RU, postRu)
    ]
  }
  where
    postEn = PostContent
      { postTitle = "Haskell is dead, long live Haskell"
      , postBlurb = "Welcome to the new home of haskell-miso.org. More blog posts coming soon."
      , postBody =
        [ lead [ "Welcome to the new home of ", a "https://haskell-miso.org" "haskell-miso.org", ". More blog posts coming soon." ]
        , para [ "Please see the ", goto (nativePage "overview") [ "native section" ], " and the ", a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", " for the latest on mobile application development." ]
        ]
      }
    postZh = PostContent
      { postTitle = "Haskell 已死，Haskell 万岁"
      , postBlurb = "欢迎来到 haskell-miso.org 的新家。更多博客文章即将推出。"
      , postBody =
        [ lead [ "欢迎来到 ", a "https://haskell-miso.org" "haskell-miso.org", " 的新家。更多博客文章即将推出。" ]
        , para [ "想了解移动应用开发的最新进展，请参阅", goto (nativePage "overview") [ "原生章节" ], "以及 ", a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", "。" ]
        ]
      }
    postEs = PostContent
      { postTitle = "Haskell ha muerto, larga vida a Haskell"
      , postBlurb = "Bienvenido al nuevo hogar de haskell-miso.org. Pronto habrá más entradas."
      , postBody =
        [ lead [ "Bienvenido al nuevo hogar de ", a "https://haskell-miso.org" "haskell-miso.org", ". Pronto habrá más entradas en el blog." ]
        , para [ "Consulta la ", goto (nativePage "overview") [ "sección nativa" ], " y el ", a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", " para lo último en desarrollo de aplicaciones móviles." ]
        ]
      }
    postFr = PostContent
      { postTitle = "Haskell est mort, vive Haskell"
      , postBlurb = "Bienvenue dans la nouvelle maison de haskell-miso.org. D'autres billets arrivent bientôt."
      , postBody =
        [ lead [ "Bienvenue dans la nouvelle maison de ", a "https://haskell-miso.org" "haskell-miso.org", ". D'autres billets arrivent bientôt." ]
        , para [ "Consultez la ", goto (nativePage "overview") [ "section native" ], " et le dépôt ", a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", " pour les dernières nouvelles du développement mobile." ]
        ]
      }
    postDe = PostContent
      { postTitle = "Haskell ist tot, lang lebe Haskell"
      , postBlurb = "Willkommen im neuen Zuhause von haskell-miso.org. Weitere Blogbeiträge folgen bald."
      , postBody =
        [ lead [ "Willkommen im neuen Zuhause von ", a "https://haskell-miso.org" "haskell-miso.org", ". Weitere Blogbeiträge folgen bald." ]
        , para [ "Das Neueste zur mobilen App-Entwicklung findest du im ", goto (nativePage "overview") [ "Native-Bereich" ], " und in der ", a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", "." ]
        ]
      }
    postPt = PostContent
      { postTitle = "Haskell morreu, vida longa ao Haskell"
      , postBlurb = "Bem-vindo ao novo lar do haskell-miso.org. Mais posts em breve."
      , postBody =
        [ lead [ "Bem-vindo ao novo lar do ", a "https://haskell-miso.org" "haskell-miso.org", ". Mais posts do blog em breve." ]
        , para [ "Veja a ", goto (nativePage "overview") [ "seção nativa" ], " e o ", a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", " para as novidades do desenvolvimento mobile." ]
        ]
      }
    postJa = PostContent
      { postTitle = "Haskell は死んだ、Haskell 万歳"
      , postBlurb = "haskell-miso.org の新しいホームへようこそ。ブログ記事は近日追加予定です。"
      , postBody =
        [ lead [ a "https://haskell-miso.org" "haskell-miso.org", " の新しいホームへようこそ。ブログ記事は近日追加予定です。" ]
        , para [ "モバイルアプリ開発の最新情報は", goto (nativePage "overview") [ "ネイティブセクション" ], "と ", a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", " をご覧ください。" ]
        ]
      }
    postRu = PostContent
      { postTitle = "Haskell умер, да здравствует Haskell"
      , postBlurb = "Добро пожаловать в новый дом haskell-miso.org. Скоро будет больше записей."
      , postBody =
        [ lead [ "Добро пожаловать в новый дом ", a "https://haskell-miso.org" "haskell-miso.org", ". Скоро в блоге появятся новые записи." ]
        , para [ "Последнее о мобильной разработке — в ", goto (nativePage "overview") [ "разделе о нативной разработке" ], " и в ", a "https://github.com/haskell-miso/miso-lynx-gallery" "miso-lynx-gallery", "." ]
        ]
      }
-----------------------------------------------------------------------------
