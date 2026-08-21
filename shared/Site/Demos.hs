-----------------------------------------------------------------------------
-- | Live, interactive examples embedded in the documentation.
--
-- Every demo is a real miso 'Component' mounted into the page with @(+>)@,
-- shown next to its own source. The source strings below are checked
-- against the code between the @-- >>> name@ / @-- <<< name@ markers by
-- @scripts/check-demos.ts@, so what you read is what runs.
--
-- Demos import the site's usual qualified names:
--
-- > import           Miso
-- > import           Miso.Lens
-- > import qualified Miso.Html.Element  as H
-- > import qualified Miso.Html.Event    as HE
-- > import qualified Miso.Html.Property as HP
module Site.Demos
  ( counterDemo, counterSource
  , lifecycleDemo, lifecycleSource
  , propsDemo, propsSource
  , contextDemo, contextSource
  , eventsDemo, eventsSource
  , effectsDemo, effectsSource
  , subsDemo, subsSource
  , mailDemo, mailSource
  , lensDemo, lensSource
  , attrsDemo, attrsSource
  , canvasDemo, canvasSource
  , bookmarksDemo
  , jsonDemo, jsonSource
  ) where
-----------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, forM_, void)
import           Data.List (nub)
import           GHC.Generics (Generic)
-----------------------------------------------------------------------------
import           Miso
import           Miso.JSON (ToJSON, FromJSON, Parser, encode, eitherDecode, withObject, (.:))
import           Miso.Lens
import qualified Miso.CSS as CSS
import qualified Miso.Canvas as Canvas
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as HE
import qualified Miso.Html.Property as HP
import           Miso.String (MisoString, ms, fromMisoString)
import qualified Miso.String as MS
-----------------------------------------------------------------------------
import           Miso.CSS.Color (Color (..))
-----------------------------------------------------------------------------
import           Site.Types (Ctx (..), Theme (..), Lang (..), langName, themeCode)
-----------------------------------------------------------------------------
-- Counter --------------------------------------------------------------------
-----------------------------------------------------------------------------
-- >>> counter
data CounterAction = Add | Subtract
  deriving (Show, Eq)

counter
  :: Component ctx () Int CounterAction
counter = component m u v
  where
    m = 0

    u = \case
      Add      -> this += 1
      Subtract -> this -= 1

    v _ () n =
      H.div_ [ HP.class_ "row" ]
        [ H.button_ [ HE.onClick Subtract ] [ "−" ]
        , H.strong_ [] [ text (ms n) ]
        , H.button_ [ HE.onClick Add ] [ "+" ]
        ]
-- <<< counter
counterDemo :: Component Ctx () Int CounterAction
counterDemo = counter
-----------------------------------------------------------------------------
counterSource :: MisoString
counterSource = """
  data CounterAction = Add | Subtract
    deriving (Show, Eq)

  counter
    :: Component ctx () Int CounterAction
  counter = component m u v
    where
      m = 0

      u = \\case
        Add      -> this += 1
        Subtract -> this -= 1

      v _ () n =
        H.div_ [ HP.class_ "row" ]
          [ H.button_ [ HE.onClick Subtract ] [ "−" ]
          , H.strong_ [] [ text (ms n) ]
          , H.button_ [ HE.onClick Add ] [ "+" ]
          ]
  """
-----------------------------------------------------------------------------
-- Lifecycle: mount / unmount / subs / mailbox ---------------------------------
-----------------------------------------------------------------------------
-- >>> lifecycle
-- A parent that mounts and unmounts a keyed child, and
-- logs what the child reports through the mailbox.
data ParentModel = ParentModel
  { _mounted :: Bool
  , _entries :: [MisoString]
  } deriving (Show, Eq)

mounted :: Lens ParentModel Bool
mounted = lens _mounted $ \p x -> p { _mounted = x }

entries :: Lens ParentModel [MisoString]
entries = lens _entries $ \p x -> p { _entries = x }

data ParentAction
  = ToggleChild
  | ChildSaid MisoString
  | BadMail MisoString

parent
  :: Eq ctx
  => Component ctx () ParentModel ParentAction
parent = (component (ParentModel True []) update view)
  { mailbox = checkMail ChildSaid BadMail
    -- receive mail from the child
  }
  where
    update = \case
      ToggleChild -> mounted %= not
      ChildSaid s -> entries %= take 6 . (s :)
      BadMail _   -> pure ()

    view _ () m =
      H.div_ []
        [ H.button_ [ HE.onClick ToggleChild ]
            [ text $ if m ^. mounted
                then "Unmount the clock"
                else "Mount the clock"
            ]
        , if m ^. mounted then "clock" +> clock else "no clock"
        , H.ul_ [ HP.class_ "log" ]
            [ H.li_ [] [ text e ] | e <- m ^. entries ]
        ]

-- The child: a clock that ticks from a subscription and
-- reports its lifecycle to the parent.
data ClockAction = Tick | Mounted | Unmounted

clock
  :: Component ctx () Int ClockAction
clock = (component 0 update view)
  { mount = Just Mounted
    -- dispatched when the component appears
  , unmount = Just Unmounted
    -- ...and when it goes away
  , subs = [ everySecond ]
    -- runs for the component's lifetime
  }
  where
    everySecond sink =
      forever (threadDelay 1000000 >> sink Tick)

    update = \case
      Tick      -> this += 1
      Mounted   -> mailParent ("clock mounted" :: MisoString)
      Unmounted -> mailParent ("clock unmounted" :: MisoString)

    view _ () secs =
      H.p_ [] [ "⏱ ", text (ms secs), "s since mount" ]
-- <<< lifecycle
lifecycleDemo :: Component Ctx () ParentModel ParentAction
lifecycleDemo = parent
-----------------------------------------------------------------------------
lifecycleSource :: MisoString
lifecycleSource = """
  -- A parent that mounts and unmounts a keyed child, and
  -- logs what the child reports through the mailbox.
  data ParentModel = ParentModel
    { _mounted :: Bool
    , _entries :: [MisoString]
    } deriving (Show, Eq)

  mounted :: Lens ParentModel Bool
  mounted = lens _mounted $ \\p x -> p { _mounted = x }

  entries :: Lens ParentModel [MisoString]
  entries = lens _entries $ \\p x -> p { _entries = x }

  data ParentAction
    = ToggleChild
    | ChildSaid MisoString
    | BadMail MisoString

  parent
    :: Eq ctx
    => Component ctx () ParentModel ParentAction
  parent = (component (ParentModel True []) update view)
    { mailbox = checkMail ChildSaid BadMail
      -- receive mail from the child
    }
    where
      update = \\case
        ToggleChild -> mounted %= not
        ChildSaid s -> entries %= take 6 . (s :)
        BadMail _   -> pure ()

      view _ () m =
        H.div_ []
          [ H.button_ [ HE.onClick ToggleChild ]
              [ text $ if m ^. mounted
                  then "Unmount the clock"
                  else "Mount the clock"
              ]
          , if m ^. mounted then "clock" +> clock else "no clock"
          , H.ul_ [ HP.class_ "log" ]
              [ H.li_ [] [ text e ] | e <- m ^. entries ]
          ]

  -- The child: a clock that ticks from a subscription and
  -- reports its lifecycle to the parent.
  data ClockAction = Tick | Mounted | Unmounted

  clock
    :: Component ctx () Int ClockAction
  clock = (component 0 update view)
    { mount = Just Mounted
      -- dispatched when the component appears
    , unmount = Just Unmounted
      -- ...and when it goes away
    , subs = [ everySecond ]
      -- runs for the component's lifetime
    }
    where
      everySecond sink =
        forever (threadDelay 1000000 >> sink Tick)

      update = \\case
        Tick      -> this += 1
        Mounted   -> mailParent ("clock mounted" :: MisoString)
        Unmounted -> mailParent ("clock unmounted" :: MisoString)

      view _ () secs =
        H.p_ [] [ "⏱ ", text (ms secs), "s since mount" ]
  """
-----------------------------------------------------------------------------
-- Props ---------------------------------------------------------------------
-----------------------------------------------------------------------------
-- >>> props
-- The props type: what the parent shares with the child.
newtype Greeting = Greeting MisoString
  deriving (Show, Eq)

-- The parent owns the name and passes it down as props.
data NamerAction = NameChanged MisoString

namer
  :: Eq ctx
  => Component ctx () MisoString NamerAction
namer = component "World" update view
  where
    update (NameChanged s) = this .= s

    view _ () name =
      H.div_ []
        [ H.input_
            [ HP.value_ name
            , HE.onInput NameChanged
            , HP.placeholder_ "Your name"
            ]
        , mountWithProps_ "greeter" (Greeting name) greeter
          -- keyed, with props
        ]

-- The child reads its props in view and in update.
data GreeterModel = GreeterModel
  { _changes :: Int
  , _shown   :: MisoString
  } deriving (Show, Eq)

changes :: Lens GreeterModel Int
changes = lens _changes $ \m x -> m { _changes = x }

shown :: Lens GreeterModel MisoString
shown = lens _shown $ \m x -> m { _shown = x }

data GreeterAction
  = ShowProps
  | PropsChanged Greeting Greeting

greeter
  :: Component ctx Greeting GreeterModel GreeterAction
greeter = (component (GreeterModel 0 "") update view)
  { onPropsChanged = Just PropsChanged
    -- react when the parent changes props
  }
  where
    update = \case
      PropsChanged _old _new -> changes += 1
      ShowProps -> do
        Greeting g <- getProps
        -- props are readable in Effect
        shown .= "props are: " <> g

    view _ (Greeting g) m =
      H.div_ []
        [ H.p_ [] [ "Hello, ", H.strong_ [] [ text g ], "!" ]
        , H.p_ [ HP.class_ "muted" ]
            [ "props changed ", text (ms (m ^. changes)), " times" ]
        , H.button_ [ HE.onClick ShowProps ]
            [ "show props" ]
        , H.p_ [] [ text (m ^. shown) ]
        ]
-- <<< props
propsDemo :: Component Ctx () MisoString NamerAction
propsDemo = namer
-----------------------------------------------------------------------------
propsSource :: MisoString
propsSource = """
  -- The props type: what the parent shares with the child.
  newtype Greeting = Greeting MisoString
    deriving (Show, Eq)

  -- The parent owns the name and passes it down as props.
  data NamerAction = NameChanged MisoString

  namer
    :: Eq ctx
    => Component ctx () MisoString NamerAction
  namer = component "World" update view
    where
      update (NameChanged s) = this .= s

      view _ () name =
        H.div_ []
          [ H.input_
              [ HP.value_ name
              , HE.onInput NameChanged
              , HP.placeholder_ "Your name"
              ]
          , mountWithProps_ "greeter" (Greeting name) greeter
            -- keyed, with props
          ]

  -- The child reads its props in view and in update.
  data GreeterModel = GreeterModel
    { _changes :: Int
    , _shown   :: MisoString
    } deriving (Show, Eq)

  changes :: Lens GreeterModel Int
  changes = lens _changes $ \\m x -> m { _changes = x }

  shown :: Lens GreeterModel MisoString
  shown = lens _shown $ \\m x -> m { _shown = x }

  data GreeterAction
    = ShowProps
    | PropsChanged Greeting Greeting

  greeter
    :: Component ctx Greeting GreeterModel GreeterAction
  greeter = (component (GreeterModel 0 "") update view)
    { onPropsChanged = Just PropsChanged
      -- react when the parent changes props
    }
    where
      update = \\case
        PropsChanged _old _new -> changes += 1
        ShowProps -> do
          Greeting g <- getProps
          -- props are readable in Effect
          shown .= "props are: " <> g

      view _ (Greeting g) m =
        H.div_ []
          [ H.p_ [] [ "Hello, ", H.strong_ [] [ text g ], "!" ]
          , H.p_ [ HP.class_ "muted" ]
              [ "props changed ", text (ms (m ^. changes)), " times" ]
          , H.button_ [ HE.onClick ShowProps ]
              [ "show props" ]
          , H.p_ [] [ text (m ^. shown) ]
          ]
  """
-----------------------------------------------------------------------------
-- Context -------------------------------------------------------------------
-----------------------------------------------------------------------------
-- >>> context
-- This site's context holds the language and the theme.
-- Any component can read it (first argument of view) and
-- change it (modifyContext). Only components with
-- useContext = True re-render when it changes.
data ThemeAction = FlipTheme

themeSwitch
  :: Component Ctx () () ThemeAction
themeSwitch = (component () update view)
  { useContext = True }
  where
    update FlipTheme = do
      ctx <- getContext
      -- context is readable in Effect
      let theme =
            if ctxTheme ctx == Dark then Light else Dark
      modifyContext (\c -> c { ctxTheme = theme })
      io_ $ do
        -- persist + apply, like the top bar
        setLocalStorage "miso.theme" (themeCode theme)
        html <- jsg "document" ! "documentElement"
        void $ html # "setAttribute" $
          ("data-theme" :: MisoString, themeCode theme)

    view ctx () () =
      H.div_ []
        [ H.p_ []
            [ "The context says: theme = "
            , H.strong_ []
                [ text (ms (show (ctxTheme ctx))) ]
            , ", language = "
            , H.strong_ []
                [ text (langName (ctxLang ctx)) ]
            ]
        , H.button_ [ HE.onClick FlipTheme ]
            [ "Flip the whole site's theme" ]
        , "frozen" +> frozen
        ]

-- A sibling that does not opt in: it keeps showing the
-- context it mounted with.
frozen :: Component Ctx () () ()
frozen = component () (\() -> pure ()) view
  -- useContext defaults to False
  where
    view ctx () () =
      H.p_ [ HP.class_ "muted" ]
        [ "useContext = False: I still think the theme is "
        , text (ms (show (ctxTheme ctx))) ]
-- <<< context
contextDemo :: Component Ctx () () ThemeAction
contextDemo = themeSwitch
-----------------------------------------------------------------------------
contextSource :: MisoString
contextSource = """
  -- This site's context holds the language and the theme.
  -- Any component can read it (first argument of view) and
  -- change it (modifyContext). Only components with
  -- useContext = True re-render when it changes.
  data ThemeAction = FlipTheme

  themeSwitch
    :: Component Ctx () () ThemeAction
  themeSwitch = (component () update view)
    { useContext = True }
    where
      update FlipTheme = do
        ctx <- getContext
        -- context is readable in Effect
        let theme =
              if ctxTheme ctx == Dark then Light else Dark
        modifyContext (\\c -> c { ctxTheme = theme })
        io_ $ do
          -- persist + apply, like the top bar
          setLocalStorage "miso.theme" (themeCode theme)
          html <- jsg "document" ! "documentElement"
          void $ html # "setAttribute" $
            ("data-theme" :: MisoString, themeCode theme)

      view ctx () () =
        H.div_ []
          [ H.p_ []
              [ "The context says: theme = "
              , H.strong_ []
                  [ text (ms (show (ctxTheme ctx))) ]
              , ", language = "
              , H.strong_ []
                  [ text (langName (ctxLang ctx)) ]
              ]
          , H.button_ [ HE.onClick FlipTheme ]
              [ "Flip the whole site's theme" ]
          , "frozen" +> frozen
          ]

  -- A sibling that does not opt in: it keeps showing the
  -- context it mounted with.
  frozen :: Component Ctx () () ()
  frozen = component () (\\() -> pure ()) view
    -- useContext defaults to False
    where
      view ctx () () =
        H.p_ [ HP.class_ "muted" ]
          [ "useContext = False: I still think the theme is "
          , text (ms (show (ctxTheme ctx))) ]
  """
-----------------------------------------------------------------------------
-- Events --------------------------------------------------------------------
-----------------------------------------------------------------------------
-- >>> events
data EventsModel = EventsModel
  { _typed   :: MisoString
  , _lastKey :: Maybe Int
  , _clickAt :: Maybe (Int, Int)
  } deriving (Show, Eq)

typed :: Lens EventsModel MisoString
typed = lens _typed $ \m x -> m { _typed = x }

lastKey :: Lens EventsModel (Maybe Int)
lastKey = lens _lastKey $ \m x -> m { _lastKey = x }

clickAt :: Lens EventsModel (Maybe (Int, Int))
clickAt = lens _clickAt $ \m x -> m { _clickAt = x }

data EventsAction
  = Typed MisoString
  | Pressed KeyCode
  | ClickedAt (Int, Int)

-- A custom decoder: read offsetX / offsetY from the
-- raw event object.
offsetDecoder :: Decoder (Int, Int)
offsetDecoder = Decoder
  { decodeAt = DecodeTarget []
  , decoder = withObject "click" $ \o -> do
      x <- o .: "offsetX" :: Parser Double
      y <- o .: "offsetY" :: Parser Double
      pure (floor x, floor y)
  }

events
  :: Component ctx () EventsModel EventsAction
events = component (EventsModel "" Nothing Nothing) update view
  where
    update = \case
      Typed s             -> typed .= s
      Pressed (KeyCode k) -> lastKey .= Just k
      ClickedAt xy        -> clickAt .= Just xy

    view _ () m =
      H.div_ []
        [ H.input_
            [ HP.placeholder_ "Type, then press keys…"
            , HE.onInput Typed
              -- "input"   (in defaultEvents)
            , HE.onKeyDown Pressed
              -- "keydown" (needs keyboardEvents)
            ]
        , H.p_ []
            [ "value: ", H.code_ [] [ text (m ^. typed) ] ]
        , H.p_ []
            [ "keyCode: ", text (maybe "–" ms (m ^. lastKey)) ]
        , H.div_
            [ HP.class_ "target"
            , on "click" offsetDecoder $ \xy _ _ ->
                ClickedAt xy
              -- custom decoder
            ]
            [ text $ case m ^. clickAt of
                Nothing ->
                  "click me"
                Just (x, y) ->
                  "clicked at " <> ms x <> "," <> ms y
            ]
        ]
-- <<< events
eventsDemo :: Component Ctx () EventsModel EventsAction
eventsDemo = events
-----------------------------------------------------------------------------
eventsSource :: MisoString
eventsSource = """
  data EventsModel = EventsModel
    { _typed   :: MisoString
    , _lastKey :: Maybe Int
    , _clickAt :: Maybe (Int, Int)
    } deriving (Show, Eq)

  typed :: Lens EventsModel MisoString
  typed = lens _typed $ \\m x -> m { _typed = x }

  lastKey :: Lens EventsModel (Maybe Int)
  lastKey = lens _lastKey $ \\m x -> m { _lastKey = x }

  clickAt :: Lens EventsModel (Maybe (Int, Int))
  clickAt = lens _clickAt $ \\m x -> m { _clickAt = x }

  data EventsAction
    = Typed MisoString
    | Pressed KeyCode
    | ClickedAt (Int, Int)

  -- A custom decoder: read offsetX / offsetY from the
  -- raw event object.
  offsetDecoder :: Decoder (Int, Int)
  offsetDecoder = Decoder
    { decodeAt = DecodeTarget []
    , decoder = withObject "click" $ \\o -> do
        x <- o .: "offsetX" :: Parser Double
        y <- o .: "offsetY" :: Parser Double
        pure (floor x, floor y)
    }

  events
    :: Component ctx () EventsModel EventsAction
  events = component (EventsModel "" Nothing Nothing) update view
    where
      update = \\case
        Typed s             -> typed .= s
        Pressed (KeyCode k) -> lastKey .= Just k
        ClickedAt xy        -> clickAt .= Just xy

      view _ () m =
        H.div_ []
          [ H.input_
              [ HP.placeholder_ "Type, then press keys…"
              , HE.onInput Typed
                -- "input"   (in defaultEvents)
              , HE.onKeyDown Pressed
                -- "keydown" (needs keyboardEvents)
              ]
          , H.p_ []
              [ "value: ", H.code_ [] [ text (m ^. typed) ] ]
          , H.p_ []
              [ "keyCode: ", text (maybe "–" ms (m ^. lastKey)) ]
          , H.div_
              [ HP.class_ "target"
              , on "click" offsetDecoder $ \\xy _ _ ->
                  ClickedAt xy
                -- custom decoder
              ]
              [ text $ case m ^. clickAt of
                  Nothing ->
                    "click me"
                  Just (x, y) ->
                    "clicked at " <> ms x <> "," <> ms y
              ]
          ]
  """
-----------------------------------------------------------------------------
-- Effects -------------------------------------------------------------------
-----------------------------------------------------------------------------
-- >>> effects
data DiceModel = DiceModel
  { _rolls :: [Int]
  , _busy  :: Bool
  } deriving (Show, Eq)

rolls :: Lens DiceModel [Int]
rolls = lens _rolls $ \m x -> m { _rolls = x }

busy :: Lens DiceModel Bool
busy = lens _busy $ \m x -> m { _busy = x }

data DiceAction = Roll | Rolled Int | Clear

dice
  :: Component ctx () DiceModel DiceAction
dice = component (DiceModel [] False) update view
  where
    update = \case
      -- `io` schedules IO; its result comes
      -- back as another action.
      Roll -> do
        busy .= True
        io $ do
          threadDelay 300000
          -- pretend this is a network call
          r <- mathRandom
          pure (Rolled (1 + floor (r * 6)))
      Rolled n -> do
        rolls %= take 12 . (n :)
        busy .= False
      -- `io_` schedules IO whose result is discarded.
      Clear -> do
        rolls .= []
        io_ (consoleLog "cleared")

    view _ () m =
      H.div_ []
        [ H.button_
            [ HE.onClick Roll
            , boolProp "disabled" (m ^. busy)
            ]
            [ text $ if m ^. busy
                then "rolling…"
                else "Roll a die (async)"
            ]
        , H.button_ [ HE.onClick Clear ] [ "clear" ]
        , H.p_ []
            [ text $ if null (m ^. rolls)
                then "no rolls yet"
                else ms (unwords (map show (m ^. rolls)))
            ]
        ]
-- <<< effects
effectsDemo :: Component Ctx () DiceModel DiceAction
effectsDemo = dice
-----------------------------------------------------------------------------
effectsSource :: MisoString
effectsSource = """
  data DiceModel = DiceModel
    { _rolls :: [Int]
    , _busy  :: Bool
    } deriving (Show, Eq)

  rolls :: Lens DiceModel [Int]
  rolls = lens _rolls $ \\m x -> m { _rolls = x }

  busy :: Lens DiceModel Bool
  busy = lens _busy $ \\m x -> m { _busy = x }

  data DiceAction = Roll | Rolled Int | Clear

  dice
    :: Component ctx () DiceModel DiceAction
  dice = component (DiceModel [] False) update view
    where
      update = \\case
        -- `io` schedules IO; its result comes
        -- back as another action.
        Roll -> do
          busy .= True
          io $ do
            threadDelay 300000
            -- pretend this is a network call
            r <- mathRandom
            pure (Rolled (1 + floor (r * 6)))
        Rolled n -> do
          rolls %= take 12 . (n :)
          busy .= False
        -- `io_` schedules IO whose result is discarded.
        Clear -> do
          rolls .= []
          io_ (consoleLog "cleared")

      view _ () m =
        H.div_ []
          [ H.button_
              [ HE.onClick Roll
              , boolProp "disabled" (m ^. busy)
              ]
              [ text $ if m ^. busy
                  then "rolling…"
                  else "Roll a die (async)"
              ]
          , H.button_ [ HE.onClick Clear ] [ "clear" ]
          , H.p_ []
              [ text $ if null (m ^. rolls)
                  then "no rolls yet"
                  else ms (unwords (map show (m ^. rolls)))
              ]
          ]
  """
-----------------------------------------------------------------------------
-- Subscriptions -------------------------------------------------------------
-----------------------------------------------------------------------------
-- >>> subs
data TimerModel = TimerModel
  { _ticks   :: Int
  , _running :: Bool
  } deriving (Show, Eq)

ticks :: Lens TimerModel Int
ticks = lens _ticks $ \m x -> m { _ticks = x }

running :: Lens TimerModel Bool
running = lens _running $ \m x -> m { _running = x }

data TimerAction = Start | Stop | Ticked

timer
  :: Component ctx () TimerModel TimerAction
timer = component (TimerModel 0 False) update view
  where
    tenTimesASecond :: Sub TimerAction
    tenTimesASecond sink =
      forever (threadDelay 100000 >> sink Ticked)

    update = \case
      Start -> do
        running .= True
        startSub ("timer" :: MisoString) tenTimesASecond
      Stop -> do
        running .= False
        stopSub ("timer" :: MisoString)
      Ticked ->
        ticks += 1

    view _ () m =
      H.div_ [ HP.class_ "row" ]
        [ H.button_ [ HE.onClick toggle ] [ text label ]
        , H.strong_ []
            [ let (secs, tenths) = (m ^. ticks) `divMod` 10
              in text (ms secs <> "." <> ms tenths <> "s")
            ]
        ]
      where
        toggle = if m ^. running then Stop else Start
        label  = if m ^. running then "Stop" else "Start"
-- <<< subs
subsDemo :: Component Ctx () TimerModel TimerAction
subsDemo = timer
-----------------------------------------------------------------------------
subsSource :: MisoString
subsSource = """
  data TimerModel = TimerModel
    { _ticks   :: Int
    , _running :: Bool
    } deriving (Show, Eq)

  ticks :: Lens TimerModel Int
  ticks = lens _ticks $ \\m x -> m { _ticks = x }

  running :: Lens TimerModel Bool
  running = lens _running $ \\m x -> m { _running = x }

  data TimerAction = Start | Stop | Ticked

  timer
    :: Component ctx () TimerModel TimerAction
  timer = component (TimerModel 0 False) update view
    where
      tenTimesASecond :: Sub TimerAction
      tenTimesASecond sink =
        forever (threadDelay 100000 >> sink Ticked)

      update = \\case
        Start -> do
          running .= True
          startSub ("timer" :: MisoString) tenTimesASecond
        Stop -> do
          running .= False
          stopSub ("timer" :: MisoString)
        Ticked ->
          ticks += 1

      view _ () m =
        H.div_ [ HP.class_ "row" ]
          [ H.button_ [ HE.onClick toggle ] [ text label ]
          , H.strong_ []
              [ let (secs, tenths) = (m ^. ticks) `divMod` 10
                in text (ms secs <> "." <> ms tenths <> "s")
              ]
          ]
        where
          toggle = if m ^. running then Stop else Start
          label  = if m ^. running then "Stop" else "Start"
  """
-----------------------------------------------------------------------------
-- Communication: mailbox + PubSub --------------------------------------------
-----------------------------------------------------------------------------
-- >>> mail
-- Two siblings that do not know each other talk over a
-- PubSub topic; the publisher also mails its parent,
-- which relays the message to every child.
data Note = Note MisoString
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

notes :: Topic Note
notes = topic "demo-notes"

data ChatAction
  = Relayed Note
  | MailErr MisoString

chat
  :: Eq ctx
  => Component ctx () () ChatAction
chat = (component () update view)
  { mailbox = checkMail Relayed MailErr }
  where
    update = \case
      Relayed n -> mailChildren n
        -- parent → all of its children
      MailErr _ -> pure ()

    view _ () _ =
      H.div_ [ HP.class_ "cols" ]
        [ "publisher"  +> publisher
        , "subscriber" +> subscriber
        ]

data PubAction = Send | Draft MisoString

publisher
  :: Component ctx () MisoString PubAction
publisher = component "hello from the publisher" update view
  where
    update = \case
      Draft s -> this .= s
      Send -> do
        s <- get
        io_ (publish notes (Note s))
        -- fan out over the topic (IO)
        mailParent (Note s)
        -- and tell the parent directly

    view _ () s =
      H.div_ []
        [ H.input_ [ HP.value_ s, HE.onInput Draft ]
        , H.button_ [ HE.onClick Send ] [ "publish" ]
        ]

data SubAction
  = Subscribe
  | Got Note
  | Oops MisoString

subscriber
  :: Component ctx () [MisoString] SubAction
subscriber = (component [] update view)
  { mount = Just Subscribe
    -- subscribe on mount
  , mailbox = checkMail Got Oops
    -- also accepts parent mail
  }
  where
    update = \case
      Subscribe    -> subscribe notes Got Oops
      Got (Note s) -> this %= take 5 . (s :)
      Oops _       -> pure ()

    view _ () received =
      H.ul_ [ HP.class_ "log" ]
        [ H.li_ [] [ text s ] | s <- received ]
-- <<< mail
mailDemo :: Component Ctx () () ChatAction
mailDemo = chat
-----------------------------------------------------------------------------
mailSource :: MisoString
mailSource = """
  -- Two siblings that do not know each other talk over a
  -- PubSub topic; the publisher also mails its parent,
  -- which relays the message to every child.
  data Note = Note MisoString
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

  notes :: Topic Note
  notes = topic "demo-notes"

  data ChatAction
    = Relayed Note
    | MailErr MisoString

  chat
    :: Eq ctx
    => Component ctx () () ChatAction
  chat = (component () update view)
    { mailbox = checkMail Relayed MailErr }
    where
      update = \\case
        Relayed n -> mailChildren n
          -- parent → all of its children
        MailErr _ -> pure ()

      view _ () _ =
        H.div_ [ HP.class_ "cols" ]
          [ "publisher"  +> publisher
          , "subscriber" +> subscriber
          ]

  data PubAction = Send | Draft MisoString

  publisher
    :: Component ctx () MisoString PubAction
  publisher = component "hello from the publisher" update view
    where
      update = \\case
        Draft s -> this .= s
        Send -> do
          s <- get
          io_ (publish notes (Note s))
          -- fan out over the topic (IO)
          mailParent (Note s)
          -- and tell the parent directly

      view _ () s =
        H.div_ []
          [ H.input_ [ HP.value_ s, HE.onInput Draft ]
          , H.button_ [ HE.onClick Send ] [ "publish" ]
          ]

  data SubAction
    = Subscribe
    | Got Note
    | Oops MisoString

  subscriber
    :: Component ctx () [MisoString] SubAction
  subscriber = (component [] update view)
    { mount = Just Subscribe
      -- subscribe on mount
    , mailbox = checkMail Got Oops
      -- also accepts parent mail
    }
    where
      update = \\case
        Subscribe    -> subscribe notes Got Oops
        Got (Note s) -> this %= take 5 . (s :)
        Oops _       -> pure ()

      view _ () received =
        H.ul_ [ HP.class_ "log" ]
          [ H.li_ [] [ text s ] | s <- received ]
  """
-----------------------------------------------------------------------------
-- State & lenses ------------------------------------------------------------
-----------------------------------------------------------------------------
-- >>> lens
data Person = Person
  { _name :: MisoString
  , _age  :: Int
  } deriving (Show, Eq)

-- Hand-written lenses (Miso.Lens.TH and
-- Miso.Lens.Generic can write these).
name :: Lens Person MisoString
name = lens _name $ \p n -> p { _name = n }

age :: Lens Person Int
age = lens _age $ \p a -> p { _age = a }

data PersonAction
  = Rename MisoString
  | Birthday
  | Younger

person
  :: Component ctx () Person PersonAction
person = component (Person "Ada" 36) update view
  where
    update = \case
      Rename n -> name .= n
        -- set through a lens
      Birthday -> age += 1
        -- arithmetic through a lens
      Younger  -> age %= max 0 . subtract 1

    view _ () p =
      H.div_ []
        [ H.input_
            [ HP.value_ (p ^. name), HE.onInput Rename ]
        , H.p_ []
            [ text (p ^. name)
            , " is "
            , text (ms (p ^. age))
            , " years old"
            ]
        , H.button_ [ HE.onClick Birthday ] [ "birthday" ]
        , H.button_ [ HE.onClick Younger ] [ "younger" ]
        ]
-- <<< lens
lensDemo :: Component Ctx () Person PersonAction
lensDemo = person
-----------------------------------------------------------------------------
lensSource :: MisoString
lensSource = """
  data Person = Person
    { _name :: MisoString
    , _age  :: Int
    } deriving (Show, Eq)

  -- Hand-written lenses (Miso.Lens.TH and
  -- Miso.Lens.Generic can write these).
  name :: Lens Person MisoString
  name = lens _name $ \\p n -> p { _name = n }

  age :: Lens Person Int
  age = lens _age $ \\p a -> p { _age = a }

  data PersonAction
    = Rename MisoString
    | Birthday
    | Younger

  person
    :: Component ctx () Person PersonAction
  person = component (Person "Ada" 36) update view
    where
      update = \\case
        Rename n -> name .= n
          -- set through a lens
        Birthday -> age += 1
          -- arithmetic through a lens
        Younger  -> age %= max 0 . subtract 1

      view _ () p =
        H.div_ []
          [ H.input_
              [ HP.value_ (p ^. name), HE.onInput Rename ]
          , H.p_ []
              [ text (p ^. name)
              , " is "
              , text (ms (p ^. age))
              , " years old"
              ]
          , H.button_ [ HE.onClick Birthday ] [ "birthday" ]
          , H.button_ [ HE.onClick Younger ] [ "younger" ]
          ]
  """
-----------------------------------------------------------------------------
-- Attributes & styles ---------------------------------------------------------
-----------------------------------------------------------------------------
-- >>> attrs
data BarModel = BarModel { _pct :: Int }
  deriving (Show, Eq)

pct :: Lens BarModel Int
pct = lens _pct $ \m x -> m { _pct = x }

data BarAction = SetPct MisoString

battery
  :: Component ctx () BarModel BarAction
battery = component (BarModel 65) update view
  where
    update (SetPct v) = pct .= fromMisoString v

    view _ () (BarModel p) =
      H.div_ []
        [ H.div_
            [ HP.class_ "bar-track"
            , HP.title_ (ms p <> "%")
              -- textProp "title"
            ]
            [ H.div_
                [ HP.classList_
                    -- conditional classes
                    [ ("bar", True)
                    , ("bar-low",  p < 30)
                    , ("bar-ok",   p >= 30 && p < 80)
                    , ("bar-full", p >= 80)
                    ]
                , CSS.style_
                    -- structured inline style
                    [ CSS.width (CSS.pct (fromIntegral p))
                    , CSS.transition_ "width"
                        (CSS.ms 250)
                        "ease-out"
                    ]
                ] []
            ]
        , H.input_
            [ HP.type_ "range"
            , HP.min_ "0"
            , HP.max_ "100"
            , HP.value_ (ms p)
            , HE.onInput SetPct
            ]
        , H.p_ [ HP.class_ "muted" ]
            [ H.code_ [] [ text (ms p), "% → ." , text klass ] ]
        ]
      where
        klass | p < 30 = "bar-low"
              | p < 80 = "bar-ok"
              | otherwise = "bar-full"
-- <<< attrs
attrsDemo :: Component Ctx () BarModel BarAction
attrsDemo = battery
-----------------------------------------------------------------------------
attrsSource :: MisoString
attrsSource = """
  data BarModel = BarModel { _pct :: Int }
    deriving (Show, Eq)

  pct :: Lens BarModel Int
  pct = lens _pct $ \\m x -> m { _pct = x }

  data BarAction = SetPct MisoString

  battery
    :: Component ctx () BarModel BarAction
  battery = component (BarModel 65) update view
    where
      update (SetPct v) = pct .= fromMisoString v

      view _ () (BarModel p) =
        H.div_ []
          [ H.div_
              [ HP.class_ "bar-track"
              , HP.title_ (ms p <> "%")
                -- textProp "title"
              ]
              [ H.div_
                  [ HP.classList_
                      -- conditional classes
                      [ ("bar", True)
                      , ("bar-low",  p < 30)
                      , ("bar-ok",   p >= 30 && p < 80)
                      , ("bar-full", p >= 80)
                      ]
                  , CSS.style_
                      -- structured inline style
                      [ CSS.width (CSS.pct (fromIntegral p))
                      , CSS.transition_ "width"
                          (CSS.ms 250)
                          "ease-out"
                      ]
                  ] []
              ]
          , H.input_
              [ HP.type_ "range"
              , HP.min_ "0"
              , HP.max_ "100"
              , HP.value_ (ms p)
              , HE.onInput SetPct
              ]
          , H.p_ [ HP.class_ "muted" ]
              [ H.code_ [] [ text (ms p), "% → ." , text klass ] ]
          ]
        where
          klass | p < 30 = "bar-low"
                | p < 80 = "bar-ok"
                | otherwise = "bar-full"
  """
-----------------------------------------------------------------------------
-- JSON ----------------------------------------------------------------------
-----------------------------------------------------------------------------
-- >>> json
data User = User
  { userName :: MisoString
  , userAge  :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
    -- generic instances from Miso.JSON

data JsonAction = Edit MisoString

jsonRoundTrip
  :: Component ctx () MisoString JsonAction
jsonRoundTrip = component initial update view
  where
    initial = "{ \"userName\": \"Ada\", \"userAge\": 36 }"

    update (Edit s) = this .= s

    view _ () input =
      H.div_ []
        [ H.textarea_
            [ HP.value_ input
            , HE.onInput Edit
            , textProp "rows" "3"
            ]
        , case eitherDecode input :: Either MisoString User of
            Left err ->
              H.p_ [ HP.class_ "muted" ] [ "✗ ", text err ]
            Right user ->
              H.pre_ []
                [ "✓ "
                , text (ms (show user))
                , "\n"
                , text (encode user)
                ]
        ]
-- <<< json
jsonDemo :: Component Ctx () MisoString JsonAction
jsonDemo = jsonRoundTrip
-----------------------------------------------------------------------------
jsonSource :: MisoString
jsonSource = """
  data User = User
    { userName :: MisoString
    , userAge  :: Int
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
      -- generic instances from Miso.JSON

  data JsonAction = Edit MisoString

  jsonRoundTrip
    :: Component ctx () MisoString JsonAction
  jsonRoundTrip = component initial update view
    where
      initial = "{ \\"userName\\": \\"Ada\\", \\"userAge\\": 36 }"

      update (Edit s) = this .= s

      view _ () input =
        H.div_ []
          [ H.textarea_
              [ HP.value_ input
              , HE.onInput Edit
              , textProp "rows" "3"
              ]
          , case eitherDecode input :: Either MisoString User of
              Left err ->
                H.p_ [ HP.class_ "muted" ] [ "✗ ", text err ]
              Right user ->
                H.pre_ []
                  [ "✓ "
                  , text (ms (show user))
                  , "\\n"
                  , text (encode user)
                  ]
          ]
  """
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Canvas ---------------------------------------------------------------------
-----------------------------------------------------------------------------
-- >>> canvas
data OrbitAction = Frame Double

-- Three planets orbit on a 2D canvas at 60 FPS.
-- rAFSub delivers a requestAnimationFrame timestamp;
-- the draw callback closes over the model.
orbits
  :: Component ctx () Double OrbitAction
orbits = (component 0 update view)
  { subs = [ rAFSub Frame ] }
  where
    update (Frame ms') = this .= ms' / 1000

    view _ () t =
      Canvas.canvas [ HP.width_ "320", HP.height_ "220" ]
        (\_ -> pure ())
        -- init: runs once, no state needed
        (\() -> scene t)
        -- draw: runs after every diff

    scene :: Double -> Canvas.Canvas ()
    scene t = do
      -- a translucent wash instead of clearRect
      -- leaves motion trails
      Canvas.fillStyle (Canvas.color (RGBA 14 13 11 0.24))
      Canvas.fillRect (0, 0, 320, 220)
      let planets =
            [ RGB 255 184 74
            , RGB 240 138 36
            , RGB 226 83 31
            ]
      forM_ (zip [0 ..] planets) $ \(i, planet) -> do
        let phase = t * (1.6 - 0.4 * i) + i * 2.1
            x = 160 + (34 + 30 * i) * cos phase
            y = 110 + (22 + 19 * i) * sin phase
        Canvas.beginPath ()
        Canvas.arc (x, y, 7 - 1.5 * i, 0, 2 * pi)
        Canvas.fillStyle (Canvas.color planet)
        Canvas.fill ()
-- <<< canvas
canvasDemo :: Component Ctx () Double OrbitAction
canvasDemo = orbits
-----------------------------------------------------------------------------
canvasSource :: MisoString
canvasSource = """
  data OrbitAction = Frame Double

  -- Three planets orbit on a 2D canvas at 60 FPS.
  -- rAFSub delivers a requestAnimationFrame timestamp;
  -- the draw callback closes over the model.
  orbits
    :: Component ctx () Double OrbitAction
  orbits = (component 0 update view)
    { subs = [ rAFSub Frame ] }
    where
      update (Frame ms') = this .= ms' / 1000

      view _ () t =
        Canvas.canvas [ HP.width_ "320", HP.height_ "220" ]
          (\\_ -> pure ())
          -- init: runs once, no state needed
          (\\() -> scene t)
          -- draw: runs after every diff

      scene :: Double -> Canvas.Canvas ()
      scene t = do
        -- a translucent wash instead of clearRect
        -- leaves motion trails
        Canvas.fillStyle (Canvas.color (RGBA 14 13 11 0.24))
        Canvas.fillRect (0, 0, 320, 220)
        let planets =
              [ RGB 255 184 74
              , RGB 240 138 36
              , RGB 226 83 31
              ]
        forM_ (zip [0 ..] planets) $ \\(i, planet) -> do
          let phase = t * (1.6 - 0.4 * i) + i * 2.1
              x = 160 + (34 + 30 * i) * cos phase
              y = 110 + (22 + 19 * i) * sin phase
          Canvas.beginPath ()
          Canvas.arc (x, y, 7 - 1.5 * i, 0, 2 * pi)
          Canvas.fillStyle (Canvas.color planet)
          Canvas.fill ()
  """
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- The "Thinking in miso" bookmarks app ---------------------------------------
--
-- The application the guide builds, live. The JSON endpoint is mocked with
-- a local list and a short delay; everything else matches the guide.
-----------------------------------------------------------------------------
data Bookmark = Bookmark
  { bmId    :: Int
  , bmTitle :: MisoString
  , bmUrl   :: MisoString
  , bmTags  :: [MisoString]
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | The mocked "endpoint".
localBookmarks :: [Bookmark]
localBookmarks =
  [ Bookmark 1 "miso on GitHub" "https://github.com/dmjio/miso" ["haskell"]
  , Bookmark 2 "Lynx docs" "https://lynxjs.org" ["mobile"]
  , Bookmark 3 "Elm architecture" "https://guide.elm-lang.org/architecture/" ["haskell","reading"]
  , Bookmark 4 "Thinking in React" "https://react.dev/learn/thinking-in-react" ["reading"]
  , Bookmark 5 "GHC WebAssembly backend" "https://downloads.haskell.org/ghc/latest/docs/users_guide/wasm.html" ["haskell"]
  , Bookmark 6 "PrimJS" "https://github.com/lynx-family/primjs" ["mobile","reading"]
  ]
-----------------------------------------------------------------------------
data Remote a = Loading | Loaded a
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data BmModel = BmModel
  { _bmData  :: Remote [Bookmark]
  , _bmQuery :: MisoString
  , _bmTag   :: Maybe MisoString
  , _bmSel   :: Maybe Int
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data BmAction
  = BmInit
  | BmGot [Bookmark]
  | BmSearch MisoString
  | BmPick (Maybe MisoString)
  | BmSelect Int
-----------------------------------------------------------------------------
bookmarksDemo :: Component Ctx () BmModel BmAction
bookmarksDemo =
  (component (BmModel Loading "" Nothing Nothing) update view)
    { mount = Just BmInit }
  where
    update = \case
      BmInit     -> io (threadDelay 450000 >> pure (BmGot localBookmarks))
      BmGot bs   -> modify (\m -> m { _bmData = Loaded bs })
      BmSearch q -> modify (\m -> m { _bmQuery = q, _bmSel = Nothing })
      BmPick tg  -> modify (\m -> m { _bmTag = tg, _bmSel = Nothing })
      BmSelect i -> modify (\m -> m { _bmSel = if _bmSel m == Just i then Nothing else Just i })

    -- derived values, computed in view (never stored)
    visible m =
      [ bk
      | Loaded bs <- [_bmData m], bk <- bs
      , MS.null q || MS.isInfixOf q (MS.toLower (bmTitle bk))
      , maybe True (`elem` bmTags bk) (_bmTag m)
      ]
      where q = MS.toLower (_bmQuery m)

    allTags bs = nub (concatMap bmTags bs)

    view _ () m =
      H.div_ [ HP.class_ "bm-app" ]
        [ H.div_ [ HP.class_ "bm-head" ]
            [ H.input_
                [ HP.type_ "search", HP.placeholder_ "Search bookmarks…"
                , HP.value_ (_bmQuery m), HE.onInput BmSearch
                ]
            ]
        , case _bmData m of
            Loading -> H.div_ [ HP.class_ "bm-loading" ] [ "Loading bookmarks…" ]
            Loaded bs ->
              H.div_ [ HP.class_ "bm-body" ]
                [ H.div_ [ HP.class_ "bm-tags" ]
                    ( tagBtn Nothing ("all (" <> ms (length bs) <> ")")
                    : [ tagBtn (Just tg) (tg <> " (" <> ms (length [ () | bk <- bs, tg `elem` bmTags bk ]) <> ")")
                      | tg <- allTags bs
                      ]
                    )
                , H.div_ [ HP.class_ "bm-main" ]
                    [ H.ul_ [ HP.class_ "bm-list" ]
                        [ H.li_
                            [ key_ (bmId bk)
                            , HP.classList_ [ ("bm-row", True), ("selected", _bmSel m == Just (bmId bk)) ]
                            , HE.onClick (BmSelect (bmId bk))
                            ]
                            [ H.span_ [ HP.class_ "bm-title" ] [ text (bmTitle bk) ]
                            , H.span_ [ HP.class_ "bm-row-tags" ] [ text (MS.intercalate " · " (bmTags bk)) ]
                            ]
                        | bk <- visible m
                        ]
                    , case [ bk | Loaded bs' <- [_bmData m], bk <- bs', Just (bmId bk) == _bmSel m ] of
                        (bk:_) ->
                          H.div_ [ HP.class_ "bm-detail" ]
                            [ H.strong_ [] [ text (bmTitle bk) ]
                            , H.a_ [ HP.href_ (bmUrl bk), HP.target_ "_blank", HP.rel_ "noopener" ] [ text (bmUrl bk) ]
                            , H.span_ [ HP.class_ "muted" ] [ "tags: ", text (MS.intercalate " · " (bmTags bk)) ]
                            ]
                        [] -> vfrag []
                    ]
                ]
        ]
      where
        tagBtn tg label =
          H.button_
            [ HP.classList_ [ ("bm-tag", True), ("active", _bmTag m == tg) ]
            , HE.onClick (BmPick tg), HP.type_ "button"
            ]
            [ text label ]
-----------------------------------------------------------------------------
