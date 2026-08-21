-----------------------------------------------------------------------------
-- | A tiny Haskell syntax highlighter that produces a 'View'.
--
-- It is deliberately small — enough to make the snippets on this site read
-- well — and runs anywhere a 'View' is rendered (WASM, JS or the
-- prerenderer), so code blocks are highlighted in the static HTML too.
module Site.Syntax
  ( haskell
  , plain
  , shell
  ) where
-----------------------------------------------------------------------------
import           Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import           Data.List (dropWhileEnd)
-----------------------------------------------------------------------------
import           Miso (View, text)
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
import           Miso.String (MisoString, ms, fromMisoString)
-----------------------------------------------------------------------------
data Tok
  = TKeyword
  | TPragma
  | TComment
  | TString
  | TNumber
  | TType
  | TOperator
  | TIdent
  | TSpace
  | TPunct
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | A highlighted Haskell code block.
haskell :: MisoString -> View context model action
haskell src =
  H.pre_ [ P.class_ "code", P.data_ "lang" "haskell" ]
    [ H.code_ [] (map render (tokenize (trimEnd (fromMisoString src)))) ]
-----------------------------------------------------------------------------
-- | A shell / terminal block. Lines starting with @$@ get a prompt style.
shell :: MisoString -> View context model action
shell src =
  H.pre_ [ P.class_ "code", P.data_ "lang" "shell" ]
    [ H.code_ [] (concatMap line (lines (trimEnd (fromMisoString src)))) ]
  where
    line ('$':' ':rest) =
      [ H.span_ [ P.class_ "tk-prompt" ] [ "$ " ]
      , H.span_ [ P.class_ "tk-cmd" ] [ text (ms rest) ]
      , "\n"
      ]
    line other = [ H.span_ [ P.class_ "tk-comment" ] [ text (ms other) ], "\n" ]
-----------------------------------------------------------------------------
-- | An unhighlighted block (HTML, JSON, …).
plain :: MisoString -> View context model action
plain src =
  H.pre_ [ P.class_ "code" ] [ H.code_ [] [ text (ms (trimEnd (fromMisoString src))) ] ]
-----------------------------------------------------------------------------
trimEnd :: String -> String
trimEnd = dropWhileEnd (== '\n')
-----------------------------------------------------------------------------
render :: (Tok, String) -> View context model action
render (TSpace, s) = text (ms s)
render (TPunct, s) = text (ms s)
render (TIdent, s) = text (ms s)
render (tok, s)    = H.span_ [ P.class_ (cls tok) ] [ text (ms s) ]
  where
    cls = \case
      TKeyword  -> "tk-kw"
      TPragma   -> "tk-pragma"
      TComment  -> "tk-comment"
      TString   -> "tk-str"
      TNumber   -> "tk-num"
      TType     -> "tk-type"
      TOperator -> "tk-op"
      _         -> ""
-----------------------------------------------------------------------------
keywords :: [String]
keywords =
  [ "module", "import", "where", "data", "type", "newtype", "deriving"
  , "instance", "class", "let", "in", "case", "of", "do", "if", "then"
  , "else", "forall", "qualified", "as", "hiding", "static", "foreign"
  , "export", "javascript", "stock", "anyclass", "via", "infixr", "infixl"
  , "infix", "default", "mdo", "rec", "proc"
  ]
-----------------------------------------------------------------------------
tokenize :: String -> [(Tok, String)]
tokenize [] = []
tokenize s@(c:cs)
  -- pragmas
  | Just (body, rest) <- pragma s = (TPragma, body) : tokenize rest
  -- block comments (non-nested is fine for our snippets)
  | Just (body, rest) <- blockComment s = (TComment, body) : tokenize rest
  -- line comments
  | Just rest <- stripDashes s =
      let (body, rest') = break (== '\n') rest
      in (TComment, "--" ++ body) : tokenize rest'
  -- strings
  | c == '"' =
      let (body, rest) = stringLit cs
      in (TString, '"' : body) : tokenize rest
  -- char literals (avoid confusing with type-level ticks such as 'Miso.Types.Component')
  | c == '\'', (x:'\'':rest) <- cs, x /= '\\' = (TString, ['\'', x, '\'']) : tokenize rest
  | c == '\'', ('\\':x:'\'':rest) <- cs = (TString, ['\'', '\\', x, '\'']) : tokenize rest
  -- whitespace
  | isSpace c =
      let (sp, rest) = span isSpace s
      in (TSpace, sp) : tokenize rest
  -- numbers
  | isDigit c =
      let (num, rest) = span (\x -> isAlphaNum x || x == '.' || x == '_') s
      in (TNumber, num) : tokenize rest
  -- identifiers / constructors / qualified names
  | isAlpha c || c == '_' =
      let (ident, rest) = span identChar s
      in classify ident : tokenize rest
  -- operators
  | c `elem` opChars =
      let (op, rest) = span (`elem` opChars) s
      in (TOperator, op) : tokenize rest
  | otherwise = (TPunct, [c]) : tokenize cs
  where
    identChar x = isAlphaNum x || x == '_' || x == '\'' || x == '.'
    opChars = "!#$%&*+./<=>?@\\^|-~:" :: String
    classify ident
      | ident `elem` keywords = (TKeyword, ident)
      | isUpper (head' (lastSegment ident)) = (TType, ident)
      | otherwise = (TIdent, ident)
    head' (x:_) = x
    head' []    = ' '
    -- "H.div_" → "div_", "Miso.Lens" → "Lens"
    lastSegment = reverse . takeWhile (/= '.') . reverse . dropWhileEnd (== '.')
-----------------------------------------------------------------------------
stripDashes :: String -> Maybe String
stripDashes ('-':'-':rest)
  | not (startsWithOp rest) = Just rest
  where
    -- "-->" is an operator, "--" or "-- foo" is a comment
    startsWithOp (x:_) = x `elem` ("!#$%&*+./<=>?@\\^|~:" :: String)
    startsWithOp []    = False
stripDashes _ = Nothing
-----------------------------------------------------------------------------
pragma :: String -> Maybe (String, String)
pragma ('{':'-':'#':rest) = go "#-{" rest
  where
    go acc ('#':'-':'}':xs) = Just (reverse acc ++ "#-}", xs)
    go acc (x:xs) = go (x:acc) xs
    go acc [] = Just (reverse acc, [])
pragma _ = Nothing
-----------------------------------------------------------------------------
blockComment :: String -> Maybe (String, String)
blockComment ('{':'-':rest) = go "-{" rest
  where
    go acc ('-':'}':xs) = Just (reverse acc ++ "-}", xs)
    go acc (x:xs) = go (x:acc) xs
    go acc [] = Just (reverse acc, [])
blockComment _ = Nothing
-----------------------------------------------------------------------------
stringLit :: String -> (String, String)
stringLit = go ""
  where
    go acc ('\\':x:xs) = go (x:'\\':acc) xs
    go acc ('"':xs) = (reverse ('"':acc), xs)
    go acc (x:xs) = go (x:acc) xs
    go acc [] = (reverse acc, [])
-----------------------------------------------------------------------------
