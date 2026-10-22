-----------------------------------------------------------------------------
-- | Markdown for LLMs (<https://llmstxt.org>): renders the site's prose
-- 'View's (docs pages, blog posts) to CommonMark, for @llms.txt@,
-- @llms-full.txt@ and the per-page @index.html.md@ files.
--
-- The prose is written with "Site.Prose", whose combinators produce a
-- small, fixed set of HTML shapes: paragraphs, headings, code blocks,
-- callouts, tables, API lists, figures and live demos. Each is mapped back
-- to its Markdown form here. Interactive widgets (buttons, inputs, canvases)
-- and SVG / MathML diagrams have no Markdown form and are dropped; a live
-- demo keeps its title and source code.
--
-- 'plainText' flattens the same prose to one line of text, for the search
-- index the docs search palette reads.
module Markdown
  ( markdown
  , plainText
  , escape
  ) where
-----------------------------------------------------------------------------
import           Data.Char (isSpace)
import           Data.List (dropWhileEnd, intercalate, isPrefixOf)
import           GHC.StaticPtr (deRefStaticPtr)
-----------------------------------------------------------------------------
import           Miso.JSON (Value (..))
import           Miso.Types
-----------------------------------------------------------------------------
-- | Render prose to Markdown. @context@ resolves 'vcontext' accessors;
-- @site@ (e.g. @https://haskell-miso.org@) makes root-relative links and
-- image sources absolute, since the Markdown is read outside the site.
markdown :: String -> context -> [View context () () action] -> String
markdown site ctx = blocks site . concatMap (flatten ctx () ())
-----------------------------------------------------------------------------
-- | The words of the prose, code included, on one line: what a reader can
-- search for. Blocks are separated by a space so that text from adjacent
-- paragraphs, list items or table cells does not run together.
plainText :: context -> [View context () () action] -> String
plainText ctx = squash . go . concatMap (flatten ctx () ())
  where
    go = concatMap $ \n -> case n of
      Txt s -> s
      El tag _ _ kids
        | dropped n -> " "
        | isBlock n || tag `elem` [ "br", "td", "th", "dt", "dd" ] -> " " <> go kids <> " "
        | otherwise -> go kids
-----------------------------------------------------------------------------
-- | A 'View' with every ambient accessor, fragment and mounted component
-- resolved, leaving only HTML elements and text.
data Node
  = El String [String] [(String, String)] [Node]
  -- ^ tag, classes, string-valued properties, children
  | Txt String
-----------------------------------------------------------------------------
flatten :: forall context props model action . context -> props -> model -> View context props model action -> [Node]
flatten ctx props m = \case
  VText _ s -> [ Txt (decodeEntities (fromMisoString s)) ]
  VNode HTML tag attrs kids _ ->
    [ El (fromMisoString tag) (classesOf attrs) (propsOf attrs) (concatMap (flatten ctx props m) kids) ]
  VNode {} -> []
  VComp (SomeComponent _ props' comp) -> mounted props' comp
  VCompStatic ptr props' ->
    case deRefStaticPtr ptr of
      SomeStaticComponent comp -> mounted props' comp
  VFrag _ kids -> concatMap (flatten ctx props m) kids
  VContext f -> flatten ctx props m (f ctx)
  VProps f -> flatten ctx props m (f props)
  VModel f -> flatten ctx props m (f m)
  where
    -- A child component is drawn from its initial model, as the
    -- prerenderer does.
    mounted :: forall p m' a . p -> Component context p m' a -> [Node]
    mounted props' comp = flatten ctx props' (model comp) (view comp (model comp))
    classesOf attrs = concat [ map fromMisoString cs | ClassList cs <- attrs ]
    propsOf attrs = [ (fromMisoString k, fromMisoString v) | Property k (String v) <- attrs ]
-----------------------------------------------------------------------------
-- | Under miso's @ssr@ flag 'text' HTML-encodes its argument when the
-- 'View' is built; Markdown wants the original characters back.
decodeEntities :: String -> String
decodeEntities = \case
  [] -> []
  s@('&':rest) ->
    case [ (ch, drop (length name) rest) | (name, ch) <- entities, name `isPrefixOf` rest ] of
      ((ch, rest') : _) -> ch : decodeEntities rest'
      [] -> '&' : decodeEntities (drop 1 s)
  ch : rest -> ch : decodeEntities rest
  where
    entities = [ ("lt;", '<'), ("gt;", '>'), ("amp;", '&'), ("quot;", '"'), ("#39;", '\'') ]
-----------------------------------------------------------------------------
-- | Elements that start a new Markdown block; everything else is inline.
isBlock :: Node -> Bool
isBlock = \case
  El tag _ _ _ -> tag `elem` blockTags
  Txt _ -> False
  where
    blockTags =
      [ "p", "h1", "h2", "h3", "h4", "h5", "h6", "pre", "ul", "ol", "li", "aside"
      , "div", "table", "dl", "figure", "figcaption", "blockquote", "section"
      , "article", "header", "footer", "nav", "hr", "details", "summary"
      ]
-----------------------------------------------------------------------------
-- | Elements with no Markdown form: controls, scripts and the pieces of the
-- docs chrome that only make sense on the page (heading anchors, the copy
-- button's toast, a demo's running component and its "live" badge, the
-- animated phone of the native anatomy scene — its captions are kept).
dropped :: Node -> Bool
dropped = \case
  El tag cls _ _ ->
    tag `elem` [ "button", "input", "select", "textarea", "canvas", "script", "style", "video", "audio", "iframe" ]
      || any (`elem` cls) [ "anchor-link", "copy-toast", "demo-stage", "demo-badge", "anat-scene", "anat-dots", "anat-hint" ]
  Txt _ -> False
-----------------------------------------------------------------------------
-- | Render a sequence of nodes as Markdown blocks separated by blank lines.
-- Runs of inline nodes between blocks become paragraphs.
blocks :: String -> [Node] -> String
blocks site = intercalate "\n\n" . filter (not . all isSpace) . go . filter (not . dropped)
  where
    go [] = []
    go ns@(n : rest)
      | isBlock n = block site n : go rest
      | otherwise =
          let (inl, rest') = break isBlock ns
          in inlines site inl : go rest'
-----------------------------------------------------------------------------
block :: String -> Node -> String
block site = \case
  Txt s -> squash s
  El tag cls props kids -> case tag of
    "p" -> inlines site kids
    "h1" -> heading 1
    "h2" -> heading 2
    "h3" -> heading 3
    "h4" -> heading 4
    "h5" -> heading 5
    "h6" -> heading 6
    "hr" -> "---"
    "pre" -> fence (maybe "" id (lookup "data-lang" props)) (textOf kids)
    "ul" -> list (const "- ") kids
    "ol" -> list (\i -> show i <> ". ") kids
    "li" -> list (const "- ") [ El tag cls props kids ]
    "table" -> table site kids
    "dl" -> definitions site kids
    "aside" | "callout" `elem` cls -> callout site kids
    "figure" | "demo" `elem` cls -> demo site kids
    "figcaption" -> emphasise (inlines site kids)
    "blockquote" -> quote (blocks site kids)
    _ -> blocks site kids
    where
      heading n = replicate n '#' <> " " <> inlines site kids
      list marker items =
        intercalate "\n"
          [ indentAfterFirst (length (marker i)) (marker i <> blocks site itemKids)
          | (i, El "li" _ _ itemKids) <- zip [1 :: Int ..] items
          ]
-----------------------------------------------------------------------------
-- | A callout ('Site.Prose.note' / 'tip' / 'warn'): a label and a body,
-- rendered as a blockquote led by the bold label.
callout :: String -> [Node] -> String
callout site kids =
  quote ("**" <> squash (textOf labels) <> ":** " <> blocks site body)
  where
    labels = [ n | n@(El _ cls _ _) <- kids, "callout-label" `elem` cls ]
    body = [ n | n@(El _ cls _ _) <- kids, "callout-label" `notElem` cls ]
-----------------------------------------------------------------------------
-- | A live demo ('Site.Prose.demo'): its title, then the source that runs.
demo :: String -> [Node] -> String
demo site kids = intercalate "\n\n" (filter (not . null) [ title, blocks site source ])
  where
    title = case [ n | n@(El _ cls _ _) <- descendants kids, "demo-title" `elem` cls ] of
      (n : _) -> "**Live example: " <> squash (textOf [ n ]) <> "**"
      [] -> ""
    source = [ n | n@(El _ cls _ _) <- descendants kids, "demo-source" `elem` cls ]
-----------------------------------------------------------------------------
-- | An HTML table as a GitHub-flavoured Markdown table.
table :: String -> [Node] -> String
table site kids =
  case rows of
    [] -> ""
    header : body ->
      intercalate "\n" $
        row header : row (map (const "---") header) : map (row . pad (length header)) body
  where
    rows = [ [ cell c | c@(El t _ _ _) <- cells, t `elem` [ "th", "td" ] ]
           | El "tr" _ _ cells <- descendants kids
           ]
    cell (El _ _ _ cs) = concatMap escapePipe (inlines site cs)
    cell (Txt s) = squash s
    row cs = "| " <> intercalate " | " cs <> " |"
    pad n cs = take n (cs <> repeat "")
    escapePipe '|' = "\\|"
    escapePipe ch = [ ch ]
-----------------------------------------------------------------------------
-- | An API list ('Site.Prose.api'): one bullet per term.
definitions :: String -> [Node] -> String
definitions site = intercalate "\n" . go
  where
    go (El "dt" _ _ dt : El "dd" _ _ dd : rest) =
      ("- " <> inlines site dt <> " — " <> inlines site dd) : go rest
    go (El "dt" _ _ dt : rest) = ("- " <> inlines site dt) : go rest
    go (_ : rest) = go rest
    go [] = []
-----------------------------------------------------------------------------
-- | Inline content, with whitespace collapsed as a browser would.
inlines :: String -> [Node] -> String
inlines site = trim . squashSpaces . concatMap inline . filter (not . dropped)
  where
    inline = \case
      Txt s -> escape s
      El tag _ props kids -> case tag of
        "code" -> code (textOf kids)
        "kbd" -> code (textOf kids)
        "strong" -> wrap "**" (inlines site kids)
        "b" -> wrap "**" (inlines site kids)
        "em" -> wrap "*" (inlines site kids)
        "i" -> wrap "*" (inlines site kids)
        "br" -> "\\\n"
        "img" -> "![" <> escape (maybe "" id (lookup "alt" props)) <> "](" <> absolute site (maybe "" id (lookup "src" props)) <> ")"
        "a" -> case (inlines site kids, lookup "href" props) of
          ("", _) -> ""
          (label, Just href) -> "[" <> label <> "](" <> absolute site href <> ")"
          (label, Nothing) -> label
        _ | isBlock (El tag [] props kids) -> " " <> blocks site kids <> " "
          | otherwise -> inlines' kids
    inlines' = concatMap inline . filter (not . dropped)
    wrap m s
      | all isSpace s = s
      | otherwise = m <> s <> m
-----------------------------------------------------------------------------
-- | Inline code, with a fence long enough for any backticks inside.
code :: String -> String
code s = fence' <> pad <> s <> pad <> fence'
  where
    fence' = replicate (longestRun '`' s + 1) '`'
    pad = if "`" `isPrefixOf` s || take 1 (reverse s) == "`" then " " else ""
-----------------------------------------------------------------------------
-- | A fenced code block.
fence :: String -> String -> String
fence lang src = ticks <> lang <> "\n" <> dropWhileEnd (== '\n') src <> "\n" <> ticks
  where
    ticks = replicate (max 3 (longestRun '`' src + 1)) '`'
-----------------------------------------------------------------------------
longestRun :: Char -> String -> Int
longestRun ch = go 0 0
  where
    go best _ [] = best
    go best n (x : xs)
      | x == ch = go (max best (n + 1)) (n + 1) xs
      | otherwise = go best 0 xs
-----------------------------------------------------------------------------
quote :: String -> String
quote = intercalate "\n" . map (\l -> if null l then ">" else "> " <> l) . lines
-----------------------------------------------------------------------------
emphasise :: String -> String
emphasise s
  | all isSpace s = ""
  | otherwise = "*" <> s <> "*"
-----------------------------------------------------------------------------
-- | Indent every line but the first, so a list item's later paragraphs and
-- code blocks stay inside the item.
indentAfterFirst :: Int -> String -> String
indentAfterFirst n s =
  case lines s of
    [] -> ""
    first : rest -> intercalate "\n" (first : map (\l -> if null l then l else replicate n ' ' <> l) rest)
-----------------------------------------------------------------------------
-- | Root-relative links and image sources made absolute.
absolute :: String -> String -> String
absolute site href
  | "/" `isPrefixOf` href, not ("//" `isPrefixOf` href) = site <> href
  | otherwise = href
-----------------------------------------------------------------------------
-- | Escape the characters Markdown would otherwise read as syntax.
escape :: String -> String
escape = concatMap $ \ch ->
  if ch `elem` ("\\`*_[]<" :: String) then [ '\\', ch ] else [ ch ]
-----------------------------------------------------------------------------
-- | The text of a subtree, as is (for code, which keeps its whitespace).
textOf :: [Node] -> String
textOf = concatMap $ \case
  Txt s -> s
  El _ _ _ kids -> textOf kids
-----------------------------------------------------------------------------
descendants :: [Node] -> [Node]
descendants = concatMap $ \case
  n@(El _ _ _ kids) -> n : descendants kids
  Txt _ -> []
-----------------------------------------------------------------------------
squash :: String -> String
squash = trim . squashSpaces
-----------------------------------------------------------------------------
-- | Collapse runs of whitespace to one space, keeping hard line breaks.
squashSpaces :: String -> String
squashSpaces = \case
  [] -> []
  '\\' : '\n' : rest -> '\\' : '\n' : squashSpaces (dropWhile (== ' ') rest)
  ch : rest
    | isSpace ch -> ' ' : squashSpaces (dropWhile isSpace rest)
    | otherwise -> ch : squashSpaces rest
-----------------------------------------------------------------------------
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
-----------------------------------------------------------------------------
