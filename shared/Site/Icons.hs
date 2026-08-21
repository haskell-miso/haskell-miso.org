-----------------------------------------------------------------------------
-- | Small inline SVG icons (all @currentColor@, 24×24 viewBox).
module Site.Icons
  ( iconGlobe
  , iconPhone
  , iconMonitor
  , iconGitHub
  , iconDiscord
  , iconX
  , iconSun
  , iconMoon
  , iconChevron
  , iconLanguage
  , iconSearch
  , iconMenu
  , iconClose
  , iconExternal
  , iconNative
  , copyIcon
  , checkIcon
  ) where
-----------------------------------------------------------------------------
import           Miso (View, Attribute)
import qualified Miso.Html.Property as P
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
svg :: [View context model action] -> View context model action
svg = S.svg_ [ SP.viewBox_ "0 0 24 24", P.class_ "icon", P.aria_ "hidden" "true", SP.fill_ "none", SP.stroke_ "currentColor", SP.strokeWidth_ "1.75", SP.strokeLinecap_ "round", SP.strokeLinejoin_ "round" ]
-----------------------------------------------------------------------------
path :: MisoString -> View context model action
path d = S.path_ [ SP.d_ d ]
-----------------------------------------------------------------------------
filled :: [Attribute model action] -> [View context model action] -> View context model action
filled attrs = S.svg_ ([ SP.viewBox_ "0 0 24 24", P.class_ "icon", P.aria_ "hidden" "true", SP.fill_ "currentColor" ] ++ attrs)
-----------------------------------------------------------------------------
iconGlobe :: View context model action
iconGlobe = svg
  [ S.circle_ [ SP.cx_ "12", SP.cy_ "12", SP.r_ "9" ]
  , path "M3 12h18M12 3a14 14 0 0 1 0 18M12 3a14 14 0 0 0 0 18"
  ]
-----------------------------------------------------------------------------
iconPhone :: View context model action
iconPhone = svg
  [ path "M10.2 1.8h3.6a2 2 0 0 1 2 2v16.4a2 2 0 0 1-2 2h-3.6a2 2 0 0 1-2-2V3.8a2 2 0 0 1 2-2z"
  , path "M11.2 19.1h1.6"
  ]
-----------------------------------------------------------------------------
iconMonitor :: View context model action
iconMonitor = svg
  [ path "M5 4h14a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2z"
  , path "M8 20h8M12 16v4"
  ]
-----------------------------------------------------------------------------
iconGitHub :: View context model action
iconGitHub = filled []
  [ S.path_ [ SP.d_ "M12 .5C5.65.5.5 5.65.5 12c0 5.08 3.29 9.39 7.86 10.91.58.1.79-.25.79-.56v-2.17c-3.2.7-3.87-1.37-3.87-1.37-.52-1.33-1.28-1.68-1.28-1.68-1.04-.71.08-.7.08-.7 1.15.08 1.76 1.19 1.76 1.19 1.03 1.76 2.69 1.25 3.35.96.1-.75.4-1.25.73-1.54-2.55-.29-5.24-1.28-5.24-5.68 0-1.26.45-2.28 1.19-3.09-.12-.29-.52-1.46.11-3.05 0 0 .97-.31 3.18 1.18a11 11 0 0 1 5.8 0c2.2-1.49 3.17-1.18 3.17-1.18.63 1.59.23 2.76.11 3.05.74.81 1.19 1.83 1.19 3.09 0 4.41-2.69 5.38-5.25 5.67.41.36.78 1.06.78 2.14v3.17c0 .31.21.67.8.56A11.5 11.5 0 0 0 23.5 12C23.5 5.65 18.35.5 12 .5z" ] ]
-----------------------------------------------------------------------------
iconDiscord :: View context model action
iconDiscord = filled []
  [ S.path_ [ SP.d_ "M19.54 5.34A17 17 0 0 0 15.4 4l-.2.4a15.6 15.6 0 0 1 3.8 1.9 13.6 13.6 0 0 0-14 0A15.6 15.6 0 0 1 8.8 4.4L8.6 4a17 17 0 0 0-4.14 1.34C1.83 9.24 1.12 13.05 1.47 16.8a17.2 17.2 0 0 0 5.2 2.62l1.1-1.78a11 11 0 0 1-1.74-.84l.42-.33a12.3 12.3 0 0 0 11.1 0l.42.33c-.55.33-1.13.61-1.74.84l1.1 1.78a17.2 17.2 0 0 0 5.2-2.62c.42-4.35-.7-8.12-2.99-11.46zM8.68 14.5c-1 0-1.84-.93-1.84-2.07s.81-2.07 1.84-2.07 1.86.94 1.84 2.07c0 1.14-.81 2.07-1.84 2.07zm6.64 0c-1 0-1.84-.93-1.84-2.07s.81-2.07 1.84-2.07 1.86.94 1.84 2.07c0 1.14-.81 2.07-1.84 2.07z" ] ]
-----------------------------------------------------------------------------
iconX :: View context model action
iconX = filled []
  [ S.path_ [ SP.d_ "M17.53 3h3.03l-6.62 7.57L21.7 21h-6.1l-4.78-6.25L5.35 21H2.32l7.08-8.1L1.94 3h6.26l4.32 5.71L17.53 3zm-1.06 16.2h1.68L7.35 4.7H5.55l10.92 14.5z" ] ]
-----------------------------------------------------------------------------
iconSun :: View context model action
iconSun = svg
  [ S.circle_ [ SP.cx_ "12", SP.cy_ "12", SP.r_ "4" ]
  , path "M12 2v2M12 20v2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41M2 12h2M20 12h2M4.93 19.07l1.41-1.41M17.66 6.34l1.41-1.41"
  ]
-----------------------------------------------------------------------------
iconMoon :: View context model action
iconMoon = svg [ path "M21 12.8A9 9 0 1 1 11.2 3a7 7 0 0 0 9.8 9.8z" ]
-----------------------------------------------------------------------------
iconChevron :: View context model action
iconChevron = svg [ path "M6 9l6 6 6-6" ]
-----------------------------------------------------------------------------
iconLanguage :: View context model action
iconLanguage = svg
  [ path "M4 5h9M8.5 3v2M11 5c-.6 3.4-2.6 6.4-5.5 8.5M6.5 8.5c1 1.9 2.7 3.5 4.5 4.5M13 19l4-9 4 9M14.2 16h5.6" ]
-----------------------------------------------------------------------------
iconSearch :: View context model action
iconSearch = svg
  [ S.circle_ [ SP.cx_ "11", SP.cy_ "11", SP.r_ "6.5" ]
  , path "M20 20l-4.2-4.2"
  ]
-----------------------------------------------------------------------------
iconMenu :: View context model action
iconMenu = svg [ path "M4 7h16M4 12h16M4 17h16" ]
-----------------------------------------------------------------------------
iconClose :: View context model action
iconClose = svg [ path "M6 6l12 12M18 6L6 18" ]
-----------------------------------------------------------------------------
-- | A slim phone with the miso lambda on its screen — the "native" icon.
iconNative :: View context model action
iconNative = svg
  [ S.path_
      [ SP.d_ "M10.2 1.8h3.6a2 2 0 0 1 2 2v16.4a2 2 0 0 1-2 2h-3.6a2 2 0 0 1-2-2V3.8a2 2 0 0 1 2-2z"
      , SP.strokeWidth_ "1"
      ]
  , S.g_ [ SP.transform_ "translate(8.75 8.2) scale(0.27)" ]
      [ S.polygon_ [ SP.points_ "1.6,22 5.2,22 12.4,12.2 10.6,9.75", SP.fill_ "currentColor", SP.stroke_ "none" ]
      , S.polygon_ [ SP.points_ "1.5,2 7.8,2 22.5,22 16.2,22", SP.fill_ "currentColor", SP.stroke_ "none" ]
      ]
  ]
-----------------------------------------------------------------------------
copyIcon :: View context model action
copyIcon = svg
  [ path "M9 9h10a1.5 1.5 0 0 1 1.5 1.5V20A1.5 1.5 0 0 1 19 21.5H9A1.5 1.5 0 0 1 7.5 20V10.5A1.5 1.5 0 0 1 9 9z"
  , path "M4.5 15h-.75A1.25 1.25 0 0 1 2.5 13.75V4A1.5 1.5 0 0 1 4 2.5h9.75A1.25 1.25 0 0 1 15 3.75V4.5"
  ]
-----------------------------------------------------------------------------
checkIcon :: View context model action
checkIcon = svg [ path "M4.5 12.5l5 5 10-11" ]
-----------------------------------------------------------------------------
iconExternal :: View context model action
iconExternal = svg [ path "M14 4h6v6M20 4l-9 9M19 14v5a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V6a1 1 0 0 1 1-1h5" ]
-----------------------------------------------------------------------------
