{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI (run) where

import Brick.AttrMap (attrMap)
import qualified Brick.AttrMap as A
import Brick.BChan
  ( BChan,
    newBChan,
    writeBChan,
  )
import Brick.Forms
  ( Form,
    editTextField,
    formState,
    handleFormEvent,
    newForm,
    renderForm,
    updateFormState,
    (@@=),
  )
import qualified Brick.Main as M
import Brick.Types
  ( ViewportType (Vertical),
    Widget,
  )
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( cached,
    hBox,
    str,
    txt,
    updateAttrMap,
    vBox,
    vLimit,
    viewport,
    withAttr,
    (<+>),
  )
import Control.Concurrent (forkIO, threadDelay)
import Control.Debounce as Debounce
import Control.Monad (forever, guard, when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as DT
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Config as Config
import qualified Graphics.Vty.Platform.Unix as V
import qualified Graphics.Vty.Platform.Unix.Settings as Settings
import Lens.Micro ((&), (.~), (?~), (^.))
import Lens.Micro.TH
import Lib
  ( Options (Options, args, command, prompt, useStdin, watchFiles),
    getOutput,
  )
import qualified Lib as O (Options (input))
import System.FSNotify
import System.FilePath (splitFileName, takeFileName)
import System.Posix.IO
  ( OpenMode (..),
    defaultFileFlags,
    openFd,
  )
import qualified System.Posix.IO as IO (stdInput)
import System.Posix.Terminal (queryTerminal)

data Name
  = VP1
  | InputField
  | CachedText
  deriving (Ord, Show, Eq)

data State = State
  { _output :: DT.Text,
    options :: Options,
    _stdInput :: String,
    _errorMessage :: Maybe String,
    _input :: DT.Text,
    --  _search       :: DT.Text,
    _currentInput :: DT.Text
  }

makeLenses ''State

errorAttr :: A.AttrName
errorAttr = A.attrName "error"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
  [ (B.borderAttr, V.yellow `on` V.black),
    (errorAttr, fg V.red)
  ]

errorWidget :: String -> Widget Name
errorWidget msg =
  updateAttrMap (A.applyAttrMappings borderMappings) $
    B.borderWithLabel (withAttr errorAttr $ str "Error") $
      vLimit 5 $
        C.center $
          str msg

drawUi :: Form State e Name -> [Widget Name]
drawUi f = [ui]
  where
    ui = vBox $ pair : errorPane ++ [B.hBorder, form]
    form = renderForm f
    state = formState f
    errorMsg = state ^. errorMessage
    errorPane = maybe [] (\m -> [errorWidget m]) errorMsg
    pair =
      hBox
        [ viewport VP1 Vertical $
            cached CachedText $
              txt state._output
        ]

vp1Scroll :: M.ViewportScroll Name
vp1Scroll = M.viewportScroll VP1

mkForm :: String -> State -> Form State e Name
mkForm prompt = newForm [(str prompt <+>) @@= editTextField currentInput InputField (Just 1)]

appEvent :: T.BrickEvent Name MyEvents -> T.EventM Name (Form State MyEvents Name) ()
appEvent (T.AppEvent Rerun) =
  M.invalidateCacheEntry CachedText >> rerun
appEvent (T.VtyEvent (V.EvKey V.KDown [])) = M.vScrollBy vp1Scroll 1
appEvent (T.VtyEvent (V.EvKey V.KUp [])) = M.vScrollBy vp1Scroll (-1)
appEvent (T.VtyEvent (V.EvKey V.KPageDown [])) = M.vScrollPage vp1Scroll T.Down
appEvent (T.VtyEvent (V.EvKey V.KPageUp [])) = M.vScrollPage vp1Scroll T.Up
appEvent (T.VtyEvent (V.EvKey V.KHome [])) = M.vScrollToBeginning vp1Scroll
appEvent (T.VtyEvent (V.EvKey V.KEnd [])) = M.vScrollToEnd vp1Scroll
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent (T.VtyEvent (V.EvKey V.KEnter [])) = do
  M.invalidateCacheEntry CachedText
  state <- gets formState
  modify $ updateFormState (state & input .~ (state ^. currentInput))
  rerun
appEvent ev = handleFormEvent ev

rerun :: T.EventM Name (Form State e Name) ()
rerun =
  do
    state <- gets formState
    let text = state ^. input
        opt = state.options
        cmdargs = opt.command : args opt
    out <- liftIO do
      let stdin = state ^. stdInput
      runExceptT $ getOutput cmdargs (DT.unpack text) stdin
    modify $
      updateFormState
        ( case out of
            Right newOutput ->
              state
                & output .~ DT.pack newOutput
                & errorMessage .~ Nothing
            Left msg -> state & errorMessage ?~ msg
        )

-- custom event type
data MyEvents = Rerun

app :: M.App (Form State MyEvents Name) MyEvents Name
app =
  M.App
    { M.appDraw = drawUi,
      M.appStartEvent = rerun,
      M.appHandleEvent = appEvent,
      M.appAttrMap = const $ attrMap V.defAttr [],
      M.appChooseCursor = M.showFirstCursor
    }

watch :: [String] -> IO (Maybe (BChan MyEvents))
watch [] = pure Nothing
watch files = do
  bchan <- newBChan 10
  let grouped = NE.groupAllWith fst $ map splitFileName files
      groupedByDir = [(fst $ NE.head ne, NE.map snd ne) | ne <- grouped]
  sendRerun <-
    mkDebounce
      defaultDebounceSettings
        { debounceAction = writeBChan bchan Rerun
        }
  _ <- forkIO $
    withManager $ \mgr -> do
      traverse_
        ( \(dir, watchFiles) -> watchDir mgr dir (const True) \case
            (Modified f _ IsFile) ->
              do
                let fileName = takeFileName f
                putStrLn $ dir <> " ? " <> fileName
                guard $ fileName `elem` NE.toList watchFiles
                sendRerun
            _ -> pure ()
        )
        groupedByDir
      forever $ threadDelay 1000000
  return $ Just bchan

run :: Options -> IO DT.Text
run opts@Options {useStdin} = do
  isTTY <- queryTerminal IO.stdInput
  when (isTTY && useStdin) $ error "cannot use --from-stdin option when stdin is a TTY"
  stdin <- if useStdin then getContents else pure ""
  let initialInput = DT.pack $ O.input opts
  let initialState =
        State
          { _input = initialInput,
            _stdInput = stdin,
            options = opts,
            _output = "",
            _errorMessage = Nothing, -- _search = "",
            _currentInput = initialInput
          }
      f = mkForm (opts.prompt <> " ") initialState
      buildVty = do
        tty <- openFd "/dev/tty" ReadWrite defaultFileFlags
        def <- Settings.defaultSettings
        V.mkVtyWithSettings Config.defaultConfig $
          def
            { Settings.settingInputFd = tty,
              Settings.settingOutputFd = tty
            }
  initialVty <- buildVty
  notify <- watch $ watchFiles opts
  result <- M.customMain initialVty buildVty notify app f
  return $ formState result ^. output
