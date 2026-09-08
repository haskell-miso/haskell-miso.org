-----------------------------------------------------------------------------
-- | The examples page: a curated tour of the haskell-miso GitHub
-- organisation, grouped by what each repository demonstrates.
module Site.Examples
  ( examplesPage
  ) where
-----------------------------------------------------------------------------
import           Miso hiding (Key)
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
import           Site.I18n
import           Site.Icons
import           Site.Prose (Nav, navigate)
import           Site.Types
-----------------------------------------------------------------------------
data Example
  = Example
  { exName  :: MisoString
  , exEmoji :: MisoString
  , exBlurb :: MisoString
  , exLive  :: Maybe MisoString
  }
-----------------------------------------------------------------------------
repo :: MisoString -> MisoString
repo name = "https://github.com/haskell-miso/" <> name
-----------------------------------------------------------------------------
examplesPage :: Component Ctx () () Nav
examplesPage = (component () navigate view) { useContext = True }
  where
    view ctx () () =
      H.div_ [ P.class_ "examples page" ]
        [ H.header_ [ P.class_ "page-head" ]
            [ H.h1_ [] [ t ctx ExTitle ]
            , H.p_ [ P.class_ "lead" ] [ t ctx ExSubtitle ]
            , H.a_
                [ P.classes_ [ "btn", "btn-ghost" ]
                , P.href_ "https://github.com/haskell-miso", P.target_ "_blank", P.rel_ "noopener" ]
                [ iconGitHub, "github.com/haskell-miso" ]
            ]
        , vfrag [ section ctx key items | (key, items) <- catalogue ]
        ]

    section ctx key items =
      H.section_ [ P.class_ "example-group" ]
        [ H.h2_ [] [ t ctx key ]
        , H.div_ [ P.class_ "example-grid" ] (map (card ctx) items)
        ]

    -- The whole card is clickable (stretched link): it opens the live demo
    -- when there is one, the repository otherwise. The explicit links in the
    -- footer sit above the stretched link so both stay reachable.
    card ctx Example {..} =
      H.article_ [ P.class_ "example-card", key_ exName ]
        [ H.a_
            [ P.class_ "example-card-link"
            , P.href_ (maybe (repo exName) id exLive)
            , P.target_ "_blank", P.rel_ "noopener"
            , P.aria_ "label" exName
            ] []
        , H.div_ [ P.class_ "example-card-head" ]
            [ H.span_ [ P.class_ "example-emoji", P.aria_ "hidden" "true" ] [ text exEmoji ]
            , H.h3_ [] [ text exName ]
            ]
        , H.p_ [] [ text exBlurb ]
        , H.div_ [ P.class_ "example-links" ]
            [ H.a_ [ P.href_ (repo exName), P.target_ "_blank", P.rel_ "noopener" ] [ iconGitHub, t ctx ExSource ]
            , case exLive of
                Just url -> H.a_ [ P.href_ url, P.target_ "_blank", P.rel_ "noopener" ] [ iconExternal, t ctx ExLive ]
                Nothing  -> vfrag []
            ]
        ]
-----------------------------------------------------------------------------
catalogue :: [(Key, [Example])]
catalogue =
  [ ( ExCatGames
    , [ Example "chess" "♟️" "The game of chess." (Just "https://chess.haskell-miso.org/")
      , Example "solitaire" "🃏" "Klondike solitaire." (Just "https://solitaire.haskell-miso.org/")
      , Example "tetris" "🧱" "The game of Tetris." (Just "https://tetris.haskell-miso.org/")
      , Example "2048" "🔢" "A 2048 clone." (Just "https://2048.haskell-miso.org/")
      , Example "snake" "🐍" "A snake clone." (Just "https://snake.haskell-miso.org/")
      , Example "asteroid" "🚀" "The game of Asteroids." (Just "https://asteroid.haskell-miso.org/")
      , Example "plane" "🛩️" "A Flappy Bird clone." (Just "https://plane.haskell-miso.org/")
      , Example "mario" "🍄" "A Super Mario physics example." (Just "https://mario.haskell-miso.org/")
      , Example "minesweeper" "💣" "The classic Minesweeper." (Just "https://minesweeper.haskell-miso.org/")
      , Example "tic-tac-miso" "❌" "Tic-tac-toe." (Just "https://tic-tac-miso.haskell-miso.org/")
      , Example "blockout" "🟦" "The game of Blockout." (Just "https://blockout.haskell-miso.org/")
      , Example "mahjong" "🀄" "The game of Mahjong." (Just "https://mahjong.haskell-miso.org/")
      , Example "sudoku" "🧩" "The game of Sudoku." (Just "https://sudoku.haskell-miso.org/")
      , Example "slingo" "🎰" "The game of Slingo." (Just "https://slingo.haskell-miso.org/")
      , Example "Texas Hold 'Em" "🤠" "Texas Hold 'Em poker" (Just "https://texasholdem.haskell-miso.org/")      
      ]
    )
  , ( ExCatBrowser
    , [ Example "canvas2d" "🖌️" "2D canvas rendering." (Just "https://canvas.haskell-miso.org/")
      , Example "svg" "🖼️" "SVG rendering showcase." (Just "https://svg.haskell-miso.org/")
      , Example "mathml" "➕" "MathML rendering." (Just "https://mathml.haskell-miso.org/")
      , Example "audio" "🔊" "The <audio> API." (Just "https://audio.haskell-miso.org/")
      , Example "video" "📽️" "The <video> API." (Just "https://video.haskell-miso.org/")
      , Example "camera" "📷" "The Camera API." (Just "https://camera.haskell-miso.org/")
      , Example "filereader" "📁" "The FileReader API." (Just "https://file-reader.haskell-miso.org/")
      , Example "fileupload" "⬆️" "File uploads." Nothing
      , Example "drag-and-drop" "🫳" "The Drag-and-drop API." (Just "https://drag-and-drop.haskell-miso.org/")
      , Example "storage" "🗂️" "Local and session storage." (Just "https://storage.haskell-miso.org/")
      , Example "cookies" "🍪" "The CookieStore API." (Just "https://cookies.haskell-miso.org/")
      , Example "fetch" "⚡" "AJAX requests with fetch." (Just "https://fetch.haskell-miso.org/")
      , Example "websocket" "🔌" "A multi-WebSocket example." (Just "https://websocket.haskell-miso.org/")
      , Example "sse" "📡" "Server-sent events." (Just "https://sse.haskell-miso.org/")
      ]
    )
  , ( ExCatPatterns
    , [ Example "counter" "💯" "The simplest possible app." (Just "https://counter.haskell-miso.org/")
      , Example "todo-mvc" "✅" "The classic TodoMVC." (Just "https://todomvc.haskell-miso.org/")
      , Example "router" "🌐" "Client-side routing." (Just "https://router.haskell-miso.org/")
      , Example "props" "🎁" "React-style props." (Just "https://props.haskell-miso.org/")
      , Example "context" "🧵" "The global context." (Just "https://context.haskell-miso.org/")
      , Example "pubsub" "🚰" "Publish / subscribe between components." (Just "https://pubsub.haskell-miso.org/")
      , Example "sampler" "🍱" "A sample application for getting started quickly." (Just "https://sampler.haskell-miso.org/")
      ]
    )
  , ( ExCatIntegrations
    , [ Example "three-miso" "🧊" "Three.js via three.hs." (Just "https://threejs.haskell-miso.org/")
      , Example "aframe" "🎮" "A-Frame WebXR scenes." (Just "https://aframe.haskell-miso.org/")
      , Example "chartjs" "📊" "chart.js charts." (Just "https://chartjs.haskell-miso.org/")
      , Example "c3.js" "📈" "c3.js charts." (Just "https://c3js.haskell-miso.org/")
      , Example "highlight.js" "✨" "highlight.js syntax highlighting." (Just "https://highlightjs.haskell-miso.org/")
      , Example "mathjax" "🔣" "MathJax typesetting." (Just "https://mathjax.haskell-miso.org/")
      , Example "tiptap" "📝" "The TipTap rich-text editor." (Just "https://tiptap.haskell-miso.org/")
      , Example "supabase-miso" "🟢" "Supabase bindings." Nothing
      , Example "miso-diagrams" "📐" "Draw diagrams with miso." (Just "https://diagrams.haskell-miso.org/")
      ]
    )
  , ( ExCatLibraries
    , [ Example "miso.ui" "💅" "A component library based on shadcn and Tailwind, built with Basecoat." (Just "https://ui.haskell-miso.org/")
      , Example "miso-tagsoup" "🥫" "Parse raw HTML / SVG into a View." Nothing
      , Example "servant-miso-html" "📄" "Render miso Views as HTML with servant." Nothing
      , Example "servant-miso-router" "🧭" "A servant router for miso." Nothing
      , Example "servant-miso-client" "📬" "A servant-client interpretation for miso." Nothing
      , Example "try-miso" "🥡" "Try miso in the browser." (Just "https://try.haskell-miso.org/")
      ]
    )
  , ( ExCatNative
    , [ Example "miso-lynx" "🐈" "miso on Lynx: the native mobile backend, tooling and docs." (Just "https://lynxjs.haskell-miso.org/")
      , Example "miso-lynx-gallery" "📱" "A gallery of native components." Nothing
      , Example "misogram" "📸" "An Instagram clone in miso and LynxJS." Nothing
      ]
    )
  ]
-----------------------------------------------------------------------------
