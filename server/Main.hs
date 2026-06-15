-----------------------------------------------------------------------------
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
module Main (main) where
-----------------------------------------------------------------------------
import Common
  ( Page (..)
  , haskellMisoComponent
  , uri404
  , uriCommunity
  , uriDocs
  , uriExamples
  , uriHome
  )
-----------------------------------------------------------------------------
import           Control.Monad (forM_)
import           Data.Aeson (ToJSON, encodeFile)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Text (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           GHC.Generics (Generic)
-----------------------------------------------------------------------------
import           Miso
import           Miso.Html
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = generate
-----------------------------------------------------------------------------
-- | Per-page SEO metadata
data PageMeta = PageMeta
  { metaTitle       :: MisoString
  , metaDescription :: MisoString
  , metaCanonical   :: MisoString
  , metaKeywords    :: MisoString
  }
-----------------------------------------------------------------------------
data SitePage = SitePage Page PageMeta
-----------------------------------------------------------------------------
siteUrl :: MisoString
siteUrl = "https://haskell-miso.org"
-----------------------------------------------------------------------------
siteImage :: MisoString
siteImage = "https://haskell-miso.org/miso.png"
-----------------------------------------------------------------------------
homeMeta, docsMeta, examplesMeta, communityMeta, the404Meta :: PageMeta
homeMeta = PageMeta
  { metaTitle       = "miso: A tasty Haskell web and mobile framework"
  , metaDescription = "miso is a fast, isomorphic Haskell framework for building web and mobile apps using WebAssembly and a virtual DOM. Write full-stack applications entirely in Haskell."
  , metaCanonical   = siteUrl <> "/"
  , metaKeywords    = "Haskell, web framework, WebAssembly, WASM, virtual DOM, isomorphic, miso, functional programming"
  }
docsMeta = PageMeta
  { metaTitle       = "Docs | miso"
  , metaDescription = "Official documentation for Miso: Haddock API reference and README guide for building isomorphic Haskell web applications with WebAssembly."
  , metaCanonical   = siteUrl <> "/docs"
  , metaKeywords    = "Miso documentation, Haskell web framework docs, Haddock, API reference, WebAssembly"
  }
examplesMeta = PageMeta
  { metaTitle       = "Examples | miso"
  , metaDescription = "Live demo applications built with Miso: Solitaire, TodoMVC, Chess, Tetris, and 2048 — all written in Haskell and compiled to WebAssembly."
  , metaCanonical   = siteUrl <> "/examples"
  , metaKeywords    = "Miso examples, Haskell WebAssembly demos, TodoMVC Haskell, Haskell games, WASM demos"
  }
communityMeta = PageMeta
  { metaTitle       = "Community | miso"
  , metaDescription = "Join the Miso Haskell community on GitHub, Matrix, IRC, and Discord. Contribute to the open-source Haskell web framework."
  , metaCanonical   = siteUrl <> "/community"
  , metaKeywords    = "Miso community, Haskell web framework community, GitHub, Discord, IRC, Matrix"
  }
the404Meta = PageMeta
  { metaTitle       = "404 Not Found | miso"
  , metaDescription = "Page not found. Return to the Miso Haskell web framework homepage."
  , metaCanonical   = siteUrl <> "/404"
  , metaKeywords    = ""
  }
-----------------------------------------------------------------------------
-- | Render out pages, manifest, robots.txt, sitemap.xml
generate :: IO ()
generate = do
  putStrLn "Generating server assets..."
  putStrLn "Writing robots.txt..."
  T.writeFile "public/robots.txt" robotsTxt
  putStrLn "Writing manifest.json..."
  encodeFile "public/manifest.json" misoManifest
  putStrLn "Writing sitemap.xml..."
  T.writeFile "public/sitemap.xml" sitemapXml
  forM_ pages $ \(fileName, page_) -> do
    putStrLn ("Writing " <> fileName <> "...")
    BL8.writeFile ("public/" <> fileName) (toHtml page_)
-----------------------------------------------------------------------------
robotsTxt :: Text
robotsTxt =
  T.unlines
  [ "# www.robotstxt.org/"
  , ""
  , "# Allow crawling of all content"
  , "User-agent: *"
  , "Disallow:"
  , ""
  , "Sitemap: https://haskell-miso.org/sitemap.xml"
  ]
-----------------------------------------------------------------------------
sitemapXml :: Text
sitemapXml =
  T.unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
  , "  <url><loc>https://haskell-miso.org/</loc></url>"
  , "  <url><loc>https://haskell-miso.org/docs</loc></url>"
  , "  <url><loc>https://haskell-miso.org/examples</loc></url>"
  , "  <url><loc>https://haskell-miso.org/community</loc></url>"
  , "</urlset>"
  ]
-----------------------------------------------------------------------------
data Manifest
  = Manifest
  { name :: MisoString
  , short_name :: MisoString
  , start_url :: MisoString
  , display :: MisoString
  , theme_color :: MisoString
  , description :: MisoString
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
instance ToJSON Manifest
-----------------------------------------------------------------------------
misoManifest :: Manifest
misoManifest =
  Manifest
  { name        = "Haskell miso"
  , display     = "standalone"
  , start_url   = "."
  , short_name  = "Miso"
  , theme_color = "#00d1b2"
  , description = "A tasty Haskell web and mobile framework"
  }
-----------------------------------------------------------------------------
instance ToHtml SitePage where
  toHtml (SitePage (Page x) PageMeta{..}) =
    toHtml
    [ doctype_
    , H.html_
      [ P.lang_ "en"
      ]
      [ H.head_ []
        [ H.title_ [] [text metaTitle]
        , H.meta_ [P.charset_ "utf-8"]
        , H.meta_ [P.name_ "theme-color", P.content_ "#00d1b2"]
        , H.meta_ [P.httpEquiv_ "X-UA-Compatible", P.content_ "IE=edge"]
        , H.meta_ [P.name_ "viewport", P.content_ "width=device-width, initial-scale=1"]
        , H.meta_ [P.name_ "description", P.content_ metaDescription]
        , H.meta_ [P.name_ "keywords", P.content_ metaKeywords]
        , H.link_ [P.rel_ "canonical", P.href_ metaCanonical]
        -- Open Graph
        , ogMeta "og:type" "website"
        , ogMeta "og:site_name" "miso"
        , ogMeta "og:title" metaTitle
        , ogMeta "og:description" metaDescription
        , ogMeta "og:url" metaCanonical
        , ogMeta "og:image" siteImage
        -- Twitter Card
        , H.meta_ [P.name_ "twitter:card", P.content_ "summary"]
        , H.meta_ [P.name_ "twitter:title", P.content_ metaTitle]
        , H.meta_ [P.name_ "twitter:description", P.content_ metaDescription]
        , H.meta_ [P.name_ "twitter:image", P.content_ siteImage]
        -- Stylesheets and scripts
        , H.link_
          [ P.rel_ "stylesheet"
          , P.href_ "https://cdnjs.cloudflare.com/ajax/libs/github-fork-ribbon-css/0.2.2/gh-fork-ribbon.min.css"
          ]
        , H.link_ [P.rel_ "manifest", P.href_ "/manifest.json"]
        , H.link_ [P.rel_ "icon", P.href_ "/favicon.ico", P.type_ "image/x-icon"]
        , H.style_ [] ".github-fork-ribbon:before { background-color: \"#e59751\" !important; } "
        , cssRef animateRef
        , cssRef bulmaRef
        , cssRef fontAwesomeRef
        , jsRef "https://buttons.github.io/buttons.js"
        , script_ [] analytics
        , jsRef "index.js"
        ]
      , H.body_ [] [mount_ x]
      ]
    ]
    where
      jsRef href =
        script_
        [ P.src_ href
        , P.async_ True
        , P.defer_ True
        , P.type_ "module"
        ] mempty
      cssRef href =
        link_
        [ P.rel_ "stylesheet"
        , P.type_ "text/css"
        , P.href_ href
        ]
      ogMeta k val =
        H.meta_ [textProp "property" k, P.content_ val]
-----------------------------------------------------------------------------
fontAwesomeRef :: MisoString
fontAwesomeRef = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
-----------------------------------------------------------------------------
animateRef :: MisoString
animateRef = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.5.2/animate.min.css"
-----------------------------------------------------------------------------
bulmaRef :: MisoString
bulmaRef = "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"
-----------------------------------------------------------------------------
analytics :: MisoString
analytics =
  mconcat
  [ "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
  , "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
  , "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
  , "})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');"
  , "ga('create', 'UA-102668481-1', 'auto');"
  , "ga('send', 'pageview');"
  ]
-----------------------------------------------------------------------------
-- | Server handlers
pages :: [ (String, SitePage) ]
pages =
  [ "community.html" =: SitePage (mkPage uriCommunity) communityMeta
  , "examples.html"  =: SitePage (mkPage uriExamples) examplesMeta
  , "docs.html"      =: SitePage (mkPage uriDocs) docsMeta
  , "index.html"     =: SitePage (mkPage uriHome) homeMeta
  , "404.html"       =: SitePage (mkPage uri404) the404Meta
  ] where
      mkPage :: URI -> Page
      mkPage uri = Page (haskellMisoComponent uri)
-----------------------------------------------------------------------------
