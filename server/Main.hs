-----------------------------------------------------------------------------
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
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
  , Model
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
import           Miso hiding (run)
import           Miso.Html
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = prerender
-----------------------------------------------------------------------------
-- | Render out, pages, manifest, robots.txt, llms.txt?
prerender :: IO ()
prerender = do
  putStrLn "Generating server assets..."
  putStrLn "Writing robots.txt..."
  T.writeFile "public/robots.txt" robotsTxt
  putStrLn "Writing manifest.json..."
  encodeFile "public/manifest.json" misoManifest
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
  { name        = "Haskell Miso"
  , display     = "standalone"
  , start_url   = "."
  , short_name  = "Miso"
  , theme_color = "#00d1b2"
  , description = "A tasty Haskell web and mobile framework"
  }
-----------------------------------------------------------------------------
instance ToHtml Page where
  toHtml (Page x) =
    toHtml
    [ doctype_
    , H.html_
      [ P.lang_ "en"
      ]
      [ H.head_
        []
        [ H.title_ [] [ "Miso: A tasty Haskell web and mobile framework"]
        , H.link_
          [ P.rel_ "stylesheet"
          , P.href_ "https://cdnjs.cloudflare.com/ajax/libs/github-fork-ribbon-css/0.2.2/gh-fork-ribbon.min.css"
          ]
        , H.link_
          [ P.rel_ "manifest"
          , P.href_ "/manifest.json"
          ]
        , H.link_
          [ P.rel_ "icon"
          , P.href_ "/favicon.ico"
          , P.type_ "image/x-icon"
          ]
        , H.meta_
          [ P.charset_ "utf-8"
          ]
        , H.meta_
          [ P.name_ "theme-color"
          , P.content_ "#00d1b2"
          ]
        , H.meta_
          [ P.httpEquiv_ "X-UA-Compatible"
          , P.content_ "IE=edge"
          ]
        , H.meta_
          [ P.name_ "viewport"
          , P.content_ "width=device-width, initial-scale=1"
          ]
        , H.meta_
          [ P.name_ "description"
          , P.content_ "Miso is a Haskell web and mobile framework"
          ]
        , H.style_ [] ".github-fork-ribbon:before { background-color: \"#e59751\" !important; } "
        , cssRef animateRef
        , cssRef bulmaRef
        , cssRef fontAwesomeRef
        , jsRef "https://buttons.github.io/buttons.js"
        , script_ [] analytics
        , jsRef "index.js"
        , body_ [] +> x
        ]
      ]
    ]
    where
      jsRef href =
        script_
        [ P.src_ href
        , P.async_ "true"
        , P.defer_ "true"
        , P.type_ "module"
        ] mempty
      cssRef href =
        link_
        [ P.rel_ "stylesheet"
        , P.type_ "text/css"
        , P.href_ href
        ]
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
pages :: [ (String, Page) ]
pages =
  [ "community.html" =: mkPage uriCommunity
  , "examples.html"  =: mkPage uriExamples
  , "docs.html"      =: mkPage uriDocs
  , "index.html"     =: mkPage uriHome
  , "404.html"       =: mkPage uri404
  ] where
      mkPage :: URI -> Page
      mkPage uri = Page (haskellMisoComponent uri)
-----------------------------------------------------------------------------
