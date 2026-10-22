-----------------------------------------------------------------------------
-- | Static site generator. Renders every route of the site to
-- @public/<path>/index.html@ with 'toHtmlWith', plus @404.html@,
-- @sitemap.xml@, @robots.txt@ and @manifest.json@, and for LLMs
-- (<https://llmstxt.org>) @llms.txt@, @llms-full.txt@ and a Markdown copy
-- of every docs page, blog post and the examples page at
-- @public/<path>/index.html.md@.
--
-- Built with vanilla GHC and miso's @ssr@ flag; depends on @base@, @miso@
-- and the @directory@ boot library (to create output folders).
module Main (main) where
-----------------------------------------------------------------------------
import           Control.Monad (forM_)
import           Data.Bits (xor)
import qualified Data.ByteString.Lazy as BL
import           Data.List (intercalate)
import           Data.Word (Word64)
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.IO (hSetEncoding, utf8, withFile, IOMode (..), hPutStr)
import           Text.Printf (printf)
-----------------------------------------------------------------------------
import           Miso
import           Miso.Html.Render (toHtmlWith)
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
import           Miso.Router (emptyURI, toURI)
import           Miso.String (MisoString, ms, fromMisoString)
import qualified Miso.String as MS
-----------------------------------------------------------------------------
import           Markdown (escape, markdown)
import           Site
import           Site.Blog
import           Site.Docs.Content (allPages, firstPage, groupKey, groupPages, groups)
import           Site.Docs.Types
import           Site.Examples (Example (..), catalogue)
import           Site.I18n (catalog, translate)
import qualified Site.I18n as I18n
import           Site.Prose (Doc)
import           Site.Route
import           Site.Styles (motionCss)
import           Site.Types
-----------------------------------------------------------------------------
main :: IO ()
main = do
  -- Views read the app-global context (language table, theme) ambiently, via
  -- 'vcontext'. Since miso 1.14.0 there is no global context cell to seed:
  -- the value is handed to the renderer instead, in 'render' below.
  ver <- buildVersion
  putStrLn ("Prerendering haskell-miso.org into public/ (v=" <> fromMisoString ver <> ") ...")
  forM_ pages $ \(route, meta) -> do
    let file = "public/" <> outputPath route
    putStrLn ("  " <> file)
    writeUtf8 file (render ver (toURI route) (routeHref route) meta)
  writeUtf8 "public/404.html" (render ver emptyURI { uriPath = "404" } "/404" notFoundMeta)
  writeUtf8 "public/sitemap.xml" sitemap
  writeUtf8 "public/robots.txt" robots
  writeUtf8 "public/llms.txt" llmsTxt
  writeUtf8 "public/llms-full.txt" llmsFullTxt
  forM_ markdownPages $ \(route, contents) ->
    writeUtf8 ("public/" <> markdownPath route) contents
  writeUtf8 "public/manifest.json" manifest
  writeUtf8 "public/blog/feed.xml" rss
  -- Motion design (keyframes, transitions, transforms) is written with
  -- Miso.CSS and rendered to an external stylesheet.
  writeUtf8 "public/motion.css" (fromMisoString motionCss)
  putStrLn ("Done: " <> show (length pages + 1) <> " pages, " <> show (length markdownPages) <> " Markdown copies.")
-----------------------------------------------------------------------------
-- | Every route on the site, with its SEO metadata.
pages :: [(Route, Meta)]
pages =
  [ (Index,    website "The React framework for Haskell" siteDescription [ "functional programming", "UI framework" ])
    -- /docs/ shows the first docs page, so it names that page as canonical
    -- and stays out of the sitemap: one URL per page for search engines.
  , (Docs,     (website "Documentation — miso" "The miso documentation: from your first Component to the native dual-thread runtime, with live examples." [ "documentation", "guide" ])
                 { metaCanonical = Just (routeHref (pageRoute firstPage)) })
  , (Examples, website "Examples — miso" examplesDescription [ "examples", "demos", "games" ])
  , (Blog,     website "Blog — miso" "Notes from the miso maintainers." [ "blog" ])
  ] ++
  [ (pageRoute p, website (pageTitle p <> " — miso docs") (pageBlurb p) (pageKeywords p)) | p <- allPages ] ++
  [ (blogPost (postSlug p), (website (postTitle (english p) <> " — miso blog") (postBlurb (english p)) [ "blog" ]) { metaPublished = Just (postDate p) }) | p <- allPosts ]
-----------------------------------------------------------------------------
siteDescription :: MisoString
siteDescription = "miso is a small, fast and composable Haskell library for building web and native user interfaces. Compiles to WebAssembly or JavaScript."

examplesDescription :: MisoString
examplesDescription = "Real applications built with miso: games, browser API demos, integrations and libraries from the haskell-miso organisation."
-----------------------------------------------------------------------------
data Meta = Meta
  { metaTitle       :: MisoString
  , metaDescription :: MisoString
  , metaKeywords    :: [MisoString]
  , metaPublished   :: Maybe MisoString   -- ^ ISO date for blog posts (og:type article)
  , metaCanonical   :: Maybe MisoString   -- ^ Path of the page this one duplicates, if any
  , metaNoIndex     :: Bool               -- ^ Keep out of search engines (the 404 page)
  }

website :: MisoString -> MisoString -> [MisoString] -> Meta
website title desc kws = Meta title desc (baseKeywords ++ kws) Nothing Nothing False

baseKeywords :: [MisoString]
baseKeywords = [ "miso", "Haskell", "web framework", "WebAssembly", "virtual DOM", "Elm architecture", "React", "mobile", "Lynx" ]
-----------------------------------------------------------------------------
notFoundMeta :: Meta
notFoundMeta = (website "Page not found — miso" "There is nothing at this address." []) { metaNoIndex = True }
-----------------------------------------------------------------------------
-- | @\/docs\/components\/@ → @docs\/components\/index.html@; @\/@ → @index.html@.
outputPath :: Route -> String
outputPath route =
  case dropWhile (== '/') (fromMisoString (routeHref route)) of
    "" -> "index.html"
    rest -> rest <> "index.html"
-----------------------------------------------------------------------------
siteUrl :: MisoString
siteUrl = "https://haskell-miso.org"

-- | Absolute URL of a page as GitHub Pages actually serves it. Every route
-- is written to @<path>/index.html@, so Pages 301s @/examples@ to
-- @/examples/@. The canonical, og:url, JSON-LD, sitemap and feed links
-- must all name the slashed form; otherwise Google sees each page's
-- canonical pointing at a redirect back to itself and refuses to index it
-- ("Alternate page with proper canonical tag").
pageUrl :: MisoString -> MisoString
pageUrl path
  | "/" `MS.isSuffixOf` path = siteUrl <> path
  | otherwise = siteUrl <> path <> "/"
-----------------------------------------------------------------------------
-- | Cache-busting stamp appended as @?v=…@ to the payload URLs below.
-- A content hash of @app.wasm@ (or @index.js@ on the JS backend), so the
-- HTML, loader, FFI glue and wasm of one deploy always load together
-- instead of mixing cached and fresh versions. Run @make optim@ before
-- @make prerender@ so the final (optimised) wasm is what gets hashed.
buildVersion :: IO MisoString
buildVersion = go [ "public/app.wasm", "public/index.js" ]
  where
    go [] = pure "dev"
    go (f:fs) = do
      exists <- doesFileExist f
      if exists
        then ms . hex . fnv1a <$> BL.readFile f
        else go fs
    hex w = printf "%016x" w :: String
    -- FNV-1a: tiny, dependency-free; only needs to change between builds.
    fnv1a :: BL.ByteString -> Word64
    fnv1a = BL.foldl' step 0xcbf29ce484222325
      where
        step h b = (h `xor` fromIntegral b) * 0x100000001b3
-----------------------------------------------------------------------------
render :: MisoString -> URI -> MisoString -> Meta -> String
render ver uri path Meta {..} =
  fromMisoString . ms . toHtmlWith (mkCtx catalog) () () . vfrag $
  [ H.doctype_
  , H.html_ [ P.lang_ "en", P.data_ "theme" "light" ]
    [ H.head_ []
        [ H.meta_ [ P.charset_ "utf-8" ]
        , H.meta_ [ P.name_ "viewport", P.content_ "width=device-width, initial-scale=1" ]
        , H.title_ [] [ text metaTitle ]
        , H.meta_ [ P.name_ "description", P.content_ metaDescription ]
        , H.meta_ [ P.name_ "keywords", P.content_ (MS.intercalate ", " metaKeywords) ]
        , H.meta_ [ P.name_ "author", P.content_ "David M. Johnson" ]
        , H.meta_ [ P.name_ "robots", P.content_ (if metaNoIndex then "noindex" else "index, follow, max-image-preview:large") ]
        , H.meta_ [ P.name_ "theme-color", P.content_ "#fbfaf7", textProp "media" "(prefers-color-scheme: light)" ]
        , H.meta_ [ P.name_ "theme-color", P.content_ "#0e0d0b", textProp "media" "(prefers-color-scheme: dark)" ]
        , H.meta_ [ P.name_ "color-scheme", P.content_ "light dark" ]
        , H.meta_ [ P.name_ "apple-mobile-web-app-title", P.content_ "miso" ]
        , H.meta_ [ P.name_ "application-name", P.content_ "miso" ]
          -- The 404 page is served at every missing address, so it has no
          -- URL of its own to name.
        , unlessNoIndex $ H.link_ [ P.rel_ "canonical", P.href_ canonical ]
          -- Markdown copy of the page, for LLMs (see llmsTxt)
        , if hasMarkdown path
            then H.link_ [ P.rel_ "alternate", P.type_ "text/markdown", P.href_ (markdownHref path) ]
            else vfrag []
          -- Open Graph (iMessage, Slack, Discord, LinkedIn, Facebook, …)
        , og "og:type" (maybe "website" (const "article") metaPublished)
        , og "og:site_name" "miso"
        , og "og:locale" "en_US"
        , og "og:title" metaTitle
        , og "og:description" metaDescription
        , unlessNoIndex $ og "og:url" canonical
        , og "og:image" ogImage
        , og "og:image:secure_url" ogImage
        , og "og:image:type" "image/png"
        , og "og:image:width" "1200"
        , og "og:image:height" "630"
        , og "og:image:alt" "The miso logo — a lambda — with the words: miso, a tasty Haskell UI framework for web, mobile and desktop"
        , case metaPublished of
            Just d  -> og "article:published_time" d
            Nothing -> vfrag []
          -- Twitter / X cards
        , H.meta_ [ P.name_ "twitter:card", P.content_ "summary_large_image" ]
        , H.meta_ [ P.name_ "twitter:site", P.content_ "@haskell_miso" ]
        , H.meta_ [ P.name_ "twitter:creator", P.content_ "@dmj_io" ]
        , H.meta_ [ P.name_ "twitter:title", P.content_ metaTitle ]
        , H.meta_ [ P.name_ "twitter:description", P.content_ metaDescription ]
        , H.meta_ [ P.name_ "twitter:image", P.content_ ogImage ]
        , H.meta_ [ P.name_ "twitter:image:alt", P.content_ "The miso lambda logo" ]
          -- Icons
        , H.link_ [ P.rel_ "icon", P.type_ "image/svg+xml", P.href_ "/assets/logo/favicon.svg" ]
        , H.link_ [ P.rel_ "icon", P.type_ "image/png", textProp "sizes" "32x32", P.href_ "/assets/logo/favicon-32.png" ]
        , H.link_ [ P.rel_ "icon", P.type_ "image/png", textProp "sizes" "64x64", P.href_ "/assets/logo/favicon.png" ]
        , H.link_ [ P.rel_ "apple-touch-icon", textProp "sizes" "180x180", P.href_ "/assets/logo/apple-touch-icon.png" ]
        , H.link_ [ P.rel_ "manifest", P.href_ "/manifest.json" ]
        , H.link_ [ P.rel_ "alternate", P.type_ "application/rss+xml", P.title_ "miso blog", P.href_ "/blog/feed.xml" ]
        , H.link_ [ P.rel_ "sitemap", P.type_ "application/xml", P.href_ "/sitemap.xml" ]
          -- Structured data
        , unlessNoIndex $ H.script_ [ P.type_ "application/ld+json" ] (jsonLd canonicalPath metaTitle metaDescription metaPublished)
        , H.link_ [ P.rel_ "preconnect", P.href_ "https://fonts.googleapis.com" ]
        , H.link_ [ P.rel_ "preconnect", P.href_ "https://fonts.gstatic.com", textProp "crossorigin" "" ]
        , H.link_ [ P.rel_ "stylesheet", P.href_ fontsHref ]
        , H.link_ [ P.rel_ "stylesheet", P.href_ ("/style.css?v=" <> ver) ]
        , H.link_ [ P.rel_ "stylesheet", P.href_ ("/motion.css?v=" <> ver) ]
        -- Apply the saved theme / language before first paint (no flash).
        , H.script_ [] themeScript
          -- GoatCounter (privacy-friendly, no cookies). count.js counts the
          -- initial page load itself and skips localhost; __misoTrack is
          -- called by the app on every client-side navigation.
        , H.script_ [] trackScript
        , H.script_
            [ P.data_ "goatcounter" "https://miso.goatcounter.com/count"
            , P.async_ True
            , P.src_ "https://gc.zgo.at/count.js"
            ] ""
          -- Privacy-friendly analytics by Plausible
        , H.script_ [ P.async_ True, P.src_ "https://plausible.io/js/pa-MUE6ESoctLsi2-a6Qm7Ve.js" ] ""
        , H.script_ [] plausibleScript
        , H.script_ [ P.src_ ("/index.js?v=" <> ver), P.type_ "module", P.defer_ True ] ""
        ]
    , H.body_ [] [ mount_ (site uri) ]
    ]
  ]
  where
    canonicalPath = maybe path id metaCanonical
    canonical = pageUrl canonicalPath
    unlessNoIndex node = if metaNoIndex then vfrag [] else node
    ogImage = siteUrl <> "/assets/logo/og-image.png"
    og k v = H.meta_ [ textProp "property" k, P.content_ v ]
-----------------------------------------------------------------------------
-- | schema.org structured data: the site (with a search action) plus the
-- current page (WebPage / TechArticle for docs / BlogPosting for posts).
jsonLd :: MisoString -> MisoString -> MisoString -> Maybe MisoString -> MisoString
jsonLd path title description published = MS.concat
  [ "{\"@context\":\"https://schema.org\",\"@graph\":["
  , "{\"@type\":\"WebSite\",\"@id\":\"", siteUrl, "/#website\",\"url\":\"", siteUrl, "/\",\"name\":\"miso\","
  , "\"description\":\"A tasty Haskell UI framework for web, mobile and desktop\","
  , "\"publisher\":{\"@id\":\"", siteUrl, "/#org\"}},"
  , "{\"@type\":\"Organization\",\"@id\":\"", siteUrl, "/#org\",\"name\":\"haskell-miso\",\"url\":\"https://github.com/haskell-miso\","
  , "\"logo\":\"", siteUrl, "/assets/logo/miso-mark-512.png\","
  , "\"sameAs\":[\"https://github.com/dmjio/miso\",\"https://x.com/haskell_miso\",\"https://discord.gg/QVDtfYNSxq\"]},"
  , "{\"@type\":\"SoftwareSourceCode\",\"name\":\"miso\",\"codeRepository\":\"https://github.com/dmjio/miso\","
  , "\"programmingLanguage\":\"Haskell\",\"license\":\"https://opensource.org/licenses/BSD-3-Clause\","
  , "\"runtimePlatform\":[\"WebAssembly\",\"JavaScript\",\"Lynx\"],\"url\":\"", siteUrl, "/\"},"
  , "{\"@type\":\"", pageType, "\",\"@id\":\"", pageUrl path, "\",\"url\":\"", pageUrl path, "\","
  , "\"name\":\"", esc title, "\",\"headline\":\"", esc title, "\",\"description\":\"", esc description, "\","
  , "\"image\":\"", siteUrl, "/assets/logo/og-image.png\",\"inLanguage\":\"en\","
  , "\"isPartOf\":{\"@id\":\"", siteUrl, "/#website\"}"
  , maybe "" (\d -> ",\"datePublished\":\"" <> d <> "\",\"author\":{\"@type\":\"Person\",\"name\":\"David M. Johnson\"}") published
  , "}]}"
  ]
  where
    pageType
      | Just _ <- published = "BlogPosting"
      | "/docs" `MS.isPrefixOf` path = "TechArticle"
      | otherwise = "WebPage"
    esc = MS.concatMap (\ch -> case ch of { '"' -> "\\\""; '\\' -> "\\\\"; _ -> MS.singleton ch })
-----------------------------------------------------------------------------
-- | RSS 2.0 feed for the blog.
rss :: String
rss = unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">"
  , "<channel>"
  , "  <title>miso blog</title>"
  , "  <link>" <> fromMisoString (pageUrl (routeHref Blog)) <> "</link>"
  , "  <description>Notes from the miso maintainers.</description>"
  , "  <language>en</language>"
  , "  <atom:link href=\"https://haskell-miso.org/blog/feed.xml\" rel=\"self\" type=\"application/rss+xml\"/>"
  ] ++ concat
  [ [ "  <item>"
    , "    <title>" <> xmlEsc (fromMisoString (postTitle (english p))) <> "</title>"
    , "    <link>" <> fromMisoString (pageUrl (routeHref (blogPost (postSlug p)))) <> "</link>"
    , "    <guid>https://haskell-miso.org" <> fromMisoString (routeHref (blogPost (postSlug p))) <> "</guid>"
    , "    <pubDate>" <> fromMisoString (postDate p) <> "T00:00:00Z</pubDate>"
    , "    <description>" <> xmlEsc (fromMisoString (postBlurb (english p))) <> "</description>"
    , "  </item>"
    ]
  | p <- allPosts
  ] ++
  [ "</channel>", "</rss>" ]
  where
    xmlEsc :: String -> String
    xmlEsc = concatMap (\ch -> case ch of { '<' -> "&lt;"; '>' -> "&gt;"; '&' -> "&amp;"; _ -> [ch] })
-----------------------------------------------------------------------------
fontsHref :: MisoString
fontsHref = "https://fonts.googleapis.com/css2?family=Inter:opsz,wght@14..32,400..800&family=JetBrains+Mono:wght@400;500;600&display=swap"
-----------------------------------------------------------------------------
themeScript :: MisoString
themeScript = ms . unlines $
  [ "(function(){try{"
  , "var t=localStorage.getItem('miso.theme');"
  , "if(!t){t=window.matchMedia&&window.matchMedia('(prefers-color-scheme: dark)').matches?'dark':'light';}"
  , "document.documentElement.setAttribute('data-theme',t);"
  , "var l=localStorage.getItem('miso.lang');if(l){document.documentElement.setAttribute('lang',l);}"
  , "}catch(e){}})();"
  ]
-----------------------------------------------------------------------------
trackScript :: MisoString
trackScript = ms . unlines $
  [ "window.__misoTrack=function(p){"
  , "if(window.goatcounter&&window.goatcounter.count){window.goatcounter.count({path:p});}"
  , "};"
  ]
-----------------------------------------------------------------------------
plausibleScript :: MisoString
plausibleScript = ms . unlines $
  [ "window.plausible=window.plausible||function(){(plausible.q=plausible.q||[]).push(arguments)},plausible.init=plausible.init||function(i){plausible.o=i||{}};"
  , "plausible.init()"
  ]
-----------------------------------------------------------------------------
sitemap :: String
sitemap = unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
  ] ++
  [ "  <url><loc>" <> fromMisoString (pageUrl (routeHref route)) <> "</loc>"
      <> maybe "" (\d -> "<lastmod>" <> fromMisoString d <> "</lastmod>") (metaPublished meta)
      <> "<changefreq>weekly</changefreq><priority>" <> priority route <> "</priority></url>"
  | (route, meta) <- pages
  , metaCanonical meta == Nothing
  , not (metaNoIndex meta)
  ] ++
  [ "</urlset>" ]
  where
    priority Index = "1.0"
    priority Docs = "0.9"
    priority Examples = "0.8"
    priority Blog = "0.7"
    priority _ = "0.6"
-----------------------------------------------------------------------------
robots :: String
robots = unlines
  [ "# www.robotstxt.org/"
  , "User-agent: *"
  , "Disallow:"
  , ""
  , "Sitemap: https://haskell-miso.org/sitemap.xml"
  , ""
  , "# Documentation for LLMs (https://llmstxt.org):"
  , "#   https://haskell-miso.org/llms.txt"
  , "#   https://haskell-miso.org/llms-full.txt"
  ]
-----------------------------------------------------------------------------
-- | The pages with a Markdown copy, and that copy. Docs pages and blog
-- posts are rendered from the same prose the HTML is ("Markdown"); the
-- examples page from its catalogue.
markdownPages :: [(Route, String)]
markdownPages =
  [ (pageRoute p, docMarkdown p) | p <- allPages ] ++
  [ (blogPost (postSlug p), postMarkdown p) | p <- allPosts ] ++
  [ (Examples, examplesMarkdown) ]
-----------------------------------------------------------------------------
-- | @\/docs\/components\/@ → @docs\/components\/index.html.md@, the
-- llms.txt convention for a page served from a directory.
markdownPath :: Route -> String
markdownPath route = dropWhile (== '/') (fromMisoString (markdownHref (routeHref route)))

markdownHref :: MisoString -> MisoString
markdownHref path
  | "/" `MS.isSuffixOf` path = path <> "index.html.md"
  | otherwise = path <> "/index.html.md"

markdownUrl :: Route -> String
markdownUrl = fromMisoString . (siteUrl <>) . markdownHref . routeHref

hasMarkdown :: MisoString -> Bool
hasMarkdown path = path `elem` [ routeHref route | (route, _) <- markdownPages ]
-----------------------------------------------------------------------------
-- | Every page of a Markdown document starts with its title, summary and
-- address, so a page read on its own (or inside llms-full.txt) says what
-- it is and where it lives.
pageHeader :: MisoString -> MisoString -> Route -> [String]
pageHeader title blurb route =
  [ "# " <> md title
  , "> " <> md blurb
  , "Source: " <> fromMisoString (pageUrl (routeHref route))
  ]

docMarkdown :: DocPage -> String
docMarkdown p =
  paragraphs (pageHeader (pageTitle p) (pageBlurb p) (pageRoute p) ++ [ prose (pageBody p) ])

postMarkdown :: Post -> String
postMarkdown p =
  paragraphs (pageHeader (postTitle content) (postBlurb content) (blogPost (postSlug p))
    ++ [ "Published " <> fromMisoString (postDate p), prose (postBody content) ])
  where
    content = english p

examplesMarkdown :: String
examplesMarkdown =
  paragraphs $
    pageHeader "Examples" examplesDescription Examples ++
    concat
      [ [ "## " <> en key
        , unlines' [ "- [" <> md exName <> "](" <> fromMisoString exRepo <> "): "
                       <> md exBlurb
                       <> maybe "" (\url -> " Live demo: " <> fromMisoString url) exLive
                   | Example {..} <- items
                   ]
        ]
      | (key, items) <- catalogue
      ]

prose :: [Doc] -> String
prose = markdown (fromMisoString siteUrl) (mkCtx catalog)
-----------------------------------------------------------------------------
-- | @\/llms.txt@: what miso is and an index of the Markdown pages, grouped
-- the way the docs sidebar is.
llmsTxt :: String
llmsTxt =
  paragraphs $
    [ "# miso"
    , "> " <> md siteDescription
    , unlines'
        [ "miso is a Haskell UI framework in the Elm / React tradition: an application is a tree of `Component`s, each a model, an `update` function and a `view` built from a virtual DOM. It runs in the browser (WebAssembly or JavaScript) and natively on iOS and Android through Lynx."
        , ""
        , "Each link below is a Markdown copy of a page on " <> fromMisoString siteUrl <> ". All of the documentation in one file: " <> fromMisoString siteUrl <> "/llms-full.txt"
        ]
    ] ++
    concat
      [ [ "## " <> en (groupKey g)
        , unlines' [ link (pageTitle p) (markdownUrl (pageRoute p)) (pageBlurb p) | p <- groupPages g ]
        ]
      | g <- groups
      ] ++
    [ "## Examples"
    , link "Examples" (markdownUrl Examples) examplesDescription
    , "## Blog"
    , unlines' [ link (postTitle (english p)) (markdownUrl (blogPost (postSlug p))) (postBlurb (english p)) | p <- allPosts ]
    , "## Optional"
    , unlines'
        [ link "API reference (Haddock)" "https://haddocks.haskell-miso.org/miso/Miso.html" "Every module, type and function in miso."
        , link "miso on GitHub" "https://github.com/dmjio/miso" "Source code, issues and releases."
        , link "haskell-miso on GitHub" "https://github.com/haskell-miso" "The example applications, libraries and tooling."
        ]
    ]
  where
    link :: MisoString -> String -> MisoString -> String
    link title url note = "- [" <> md title <> "](" <> url <> "): " <> md note
-----------------------------------------------------------------------------
-- | @\/llms-full.txt@: the llms.txt introduction followed by every docs
-- page in reading order, the examples and the blog posts, in full.
llmsFullTxt :: String
llmsFullTxt =
  intercalate "\n\n---\n\n" $
    paragraphs
      [ "# miso"
      , "> " <> md siteDescription
      , "The complete documentation of miso, from " <> fromMisoString siteUrl <> ", as Markdown. An index of the individual pages is at " <> fromMisoString siteUrl <> "/llms.txt"
      ] :
    map docMarkdown allPages ++
    [ examplesMarkdown ] ++
    map postMarkdown allPosts
-----------------------------------------------------------------------------
-- | Plain text (a title, a blurb) escaped for Markdown.
md :: MisoString -> String
md = escape . fromMisoString

-- | English text for a translation key.
en :: I18n.Key -> String
en = md . translate (mkCtx catalog)

paragraphs :: [String] -> String
paragraphs = (<> "\n") . intercalate "\n\n" . filter (not . null)

unlines' :: [String] -> String
unlines' = intercalate "\n"
-----------------------------------------------------------------------------
manifest :: String
manifest = unlines
  [ "{"
  , "  \"name\": \"Haskell miso\","
  , "  \"short_name\": \"miso\","
  , "  \"description\": \"A tasty Haskell UI framework for web, mobile and desktop\","
  , "  \"start_url\": \"/\","
  , "  \"display\": \"standalone\","
  , "  \"background_color\": \"#fbfaf7\","
  , "  \"theme_color\": \"#141210\","
  , "  \"icons\": ["
  , "    { \"src\": \"/assets/logo/miso-mark-512.png\", \"sizes\": \"512x512\", \"type\": \"image/png\" },"
  , "    { \"src\": \"/assets/logo/miso-mark-192.png\", \"sizes\": \"192x192\", \"type\": \"image/png\" }"
  , "  ]"
  , "}"
  ]
-----------------------------------------------------------------------------
writeUtf8 :: FilePath -> String -> IO ()
writeUtf8 path contents = do
  mkdirs path
  withFile path WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h contents
-----------------------------------------------------------------------------
-- | Create the parent directories of a file.
mkdirs :: FilePath -> IO ()
mkdirs path = createDirectoryIfMissing True (parent path)
  where
    parent = reverse . drop 1 . dropWhile (/= '/') . reverse
-----------------------------------------------------------------------------
