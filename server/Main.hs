-----------------------------------------------------------------------------
-- | Static site generator. Renders every route of the site to
-- @public/<path>/index.html@ with 'toHtml', plus @404.html@,
-- @sitemap.xml@, @robots.txt@ and @manifest.json@.
--
-- Built with vanilla GHC and miso's @ssr@ flag; depends on @base@, @miso@
-- and the @directory@ boot library (to create output folders).
module Main (main) where
-----------------------------------------------------------------------------
import           Control.Monad (forM_)
import           Data.Bits (xor)
import qualified Data.ByteString.Lazy as BL
import           Data.Word (Word64)
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.IO (hSetEncoding, utf8, withFile, IOMode (..), hPutStr)
import           Text.Printf (printf)
-----------------------------------------------------------------------------
import           Miso
import           Miso.Html.Render (toHtml)
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
import           Miso.Router (emptyURI, toURI)
import           Miso.String (MisoString, ms, fromMisoString)
import qualified Miso.String as MS
-----------------------------------------------------------------------------
import           Site
import           Site.Blog
import           Site.Docs.Content (allPages)
import           Site.Docs.Types
import           Site.I18n (catalog)
import           Site.Route
import           Site.Styles (motionCss)
import           Site.Types
-----------------------------------------------------------------------------
main :: IO ()
main = do
  -- Views read the app-global context (language table, theme), so seed it
  -- before rendering anything.
  setContext (mkCtx catalog)
  ver <- buildVersion
  putStrLn ("Prerendering haskell-miso.org into public/ (v=" <> fromMisoString ver <> ") ...")
  forM_ pages $ \(route, meta) -> do
    let file = "public/" <> outputPath route
    putStrLn ("  " <> file)
    writeUtf8 file (render ver (toURI route) (routeHref route) meta)
  writeUtf8 "public/404.html" (render ver emptyURI { uriPath = "404" } "/404" notFoundMeta)
  writeUtf8 "public/sitemap.xml" sitemap
  writeUtf8 "public/robots.txt" robots
  writeUtf8 "public/manifest.json" manifest
  writeUtf8 "public/blog/feed.xml" rss
  -- Motion design (keyframes, transitions, transforms) is written with
  -- Miso.CSS and rendered to an external stylesheet.
  writeUtf8 "public/motion.css" (fromMisoString motionCss)
  putStrLn ("Done: " <> show (length pages + 1) <> " pages.")
-----------------------------------------------------------------------------
-- | Every route on the site, with its SEO metadata.
pages :: [(Route, Meta)]
pages =
  [ (Index,    website "The React framework for Haskell"
                       "miso is a small, fast and composable Haskell library for building web and native user interfaces. Compiles to WebAssembly or JavaScript." [ "functional programming", "UI framework" ])
  , (Docs,     website "Documentation — miso" "The miso documentation: from your first Component to the native dual-thread runtime, with live examples." [ "documentation", "guide" ])
  , (Examples, website "Examples — miso" "Real applications built with miso: games, browser API demos, integrations and libraries from the haskell-miso organisation." [ "examples", "demos", "games" ])
  , (Blog,     website "Blog — miso" "Notes from the miso maintainers." [ "blog" ])
  ] ++
  [ (pageRoute p, website (pageTitle p <> " — miso docs") (pageBlurb p) (pageKeywords p)) | p <- allPages ] ++
  [ (blogPost (postSlug p), (website (postTitle (english p) <> " — miso blog") (postBlurb (english p)) [ "blog" ]) { metaPublished = Just (postDate p) }) | p <- allPosts ]
-----------------------------------------------------------------------------
data Meta = Meta
  { metaTitle       :: MisoString
  , metaDescription :: MisoString
  , metaKeywords    :: [MisoString]
  , metaPublished   :: Maybe MisoString   -- ^ ISO date for blog posts (og:type article)
  }

website :: MisoString -> MisoString -> [MisoString] -> Meta
website title desc kws = Meta title desc (baseKeywords ++ kws) Nothing

baseKeywords :: [MisoString]
baseKeywords = [ "miso", "Haskell", "web framework", "WebAssembly", "virtual DOM", "Elm architecture", "React", "mobile", "Lynx" ]
-----------------------------------------------------------------------------
notFoundMeta :: Meta
notFoundMeta = website "Page not found — miso" "There is nothing at this address." []
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
render ver uri path Meta {..} = fromMisoString . ms . toHtml $
  [ H.doctype_
  , H.html_ [ P.lang_ "en", P.data_ "theme" "light" ]
    [ H.head_ []
        [ H.meta_ [ P.charset_ "utf-8" ]
        , H.meta_ [ P.name_ "viewport", P.content_ "width=device-width, initial-scale=1" ]
        , H.title_ [] [ text metaTitle ]
        , H.meta_ [ P.name_ "description", P.content_ metaDescription ]
        , H.meta_ [ P.name_ "keywords", P.content_ (MS.intercalate ", " metaKeywords) ]
        , H.meta_ [ P.name_ "author", P.content_ "David M. Johnson" ]
        , H.meta_ [ P.name_ "robots", P.content_ "index, follow, max-image-preview:large" ]
        , H.meta_ [ P.name_ "theme-color", P.content_ "#fbfaf7", textProp "media" "(prefers-color-scheme: light)" ]
        , H.meta_ [ P.name_ "theme-color", P.content_ "#0e0d0b", textProp "media" "(prefers-color-scheme: dark)" ]
        , H.meta_ [ P.name_ "color-scheme", P.content_ "light dark" ]
        , H.meta_ [ P.name_ "apple-mobile-web-app-title", P.content_ "miso" ]
        , H.meta_ [ P.name_ "application-name", P.content_ "miso" ]
        , H.link_ [ P.rel_ "canonical", P.href_ canonical ]
          -- Open Graph (iMessage, Slack, Discord, LinkedIn, Facebook, …)
        , og "og:type" (maybe "website" (const "article") metaPublished)
        , og "og:site_name" "miso"
        , og "og:locale" "en_US"
        , og "og:title" metaTitle
        , og "og:description" metaDescription
        , og "og:url" canonical
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
        , H.script_ [ P.type_ "application/ld+json" ] (jsonLd path metaTitle metaDescription metaPublished)
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
    canonical = pageUrl path
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
  [ "  <url><loc>" <> fromMisoString (pageUrl (routeHref route)) <> "</loc><changefreq>weekly</changefreq><priority>" <> priority route <> "</priority></url>"
  | (route, _) <- pages
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
  ]
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
