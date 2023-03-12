{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module UI (run) where

import           Control.Monad.Except
import           Control.Monad.State   (gets, modify)
import qualified Graphics.Vty          as V

import qualified Data.ByteString.Char8 as BS
import           Data.Foldable         (traverse_)
import qualified Data.Text             as DT

import           Lens.Micro            ((&), (.~), (?~), (^.))
import           Lens.Micro.TH

import           Brick.AttrMap         (attrMap)
import           Brick.BChan           (BChan, newBChan, writeBChan)
import           Brick.Forms           (Form, editTextField, formState,
                                        handleFormEvent, newForm, renderForm,
                                        updateFormState, (@@=))
import qualified Brick.Main            as M
import           Brick.Types           (ViewportType (Vertical), Widget)

import qualified Brick.AttrMap         as A
import qualified Brick.Types           as T
import           Brick.Util            (fg, on)
import qualified Brick.Widgets.Border  as B
import qualified Brick.Widgets.Center  as C
import           Brick.Widgets.Core    (cached, hBox, str, txt, updateAttrMap,
                                        vBox, vLimit, viewport, withAttr, (<+>))
import           Lib                   (Options (..), getOutput)
import           System.INotify
import qualified System.Posix.IO       as IO (stdInput)
import           System.Posix.IO       (OpenMode (..), defaultFileFlags, openFd)
import           System.Posix.Terminal (queryTerminal)

data Name = VP1
          | InputField
          | CachedText
          deriving (Ord, Show, Eq)

data State = State {
  _output       :: DT.Text,
  options       :: Options,
  _stdInput     :: String,
  _errorMessage :: Maybe String,
  _input        :: DT.Text,
--  _search       :: DT.Text,
  _currentInput :: DT.Text
  }

makeLenses ''State

errorAttr :: A.AttrName
errorAttr = A.attrName "error"

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
    [ (B.borderAttr,         V.yellow `on` V.black)
    , (errorAttr,            fg V.red)
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
        ui = vBox $ pair : errorPane ++ [ B.hBorder, form ]
        form = renderForm f
        state = formState f
        errorMsg = state^.errorMessage
        errorPane = maybe [] (\m -> [errorWidget m]) errorMsg
        pair = hBox [ viewport VP1 Vertical $
                      cached CachedText $ txt $ _output state
                    ]

vp1Scroll :: M.ViewportScroll Name
vp1Scroll = M.viewportScroll VP1

mkForm :: State -> Form State e Name
mkForm = newForm [ (str "> " <+>) @@= editTextField currentInput InputField (Just 1) ]

appEvent :: T.BrickEvent Name MyEvents -> T.EventM Name (Form State MyEvents Name) ()
appEvent (T.AppEvent Rerun) =
  M.invalidateCacheEntry CachedText -- >> modify rerun
appEvent (T.VtyEvent (V.EvKey V.KDown []))  = M.vScrollBy vp1Scroll 1
appEvent (T.VtyEvent (V.EvKey V.KUp []))    = M.vScrollBy vp1Scroll (-1)
appEvent (T.VtyEvent (V.EvKey V.KPageDown []))    = M.vScrollPage vp1Scroll T.Down
appEvent (T.VtyEvent (V.EvKey V.KPageUp []))    = M.vScrollPage vp1Scroll T.Up
appEvent (T.VtyEvent (V.EvKey V.KHome []))    = M.vScrollToBeginning vp1Scroll
appEvent (T.VtyEvent (V.EvKey V.KEnd []))    = M.vScrollToEnd vp1Scroll
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
        cmdargs = cmdline $ options state
    out <- liftIO do
      let stdin = state ^. stdInput
      runExceptT $ getOutput cmdargs (DT.unpack text) stdin
    modify $ updateFormState (case out of
      Right newOutput ->
         state & output .~ DT.pack newOutput
               & errorMessage .~ Nothing
      Left msg -> state & errorMessage ?~ msg)

-- custom event type
data MyEvents = Rerun

app :: M.App (Form State MyEvents Name) MyEvents Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = rerun
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.showFirstCursor
          }


watch :: [String] -> IO (Maybe (BChan MyEvents))
watch [] = pure Nothing
watch files = do
    inotify <- initINotify
    bchan <- newBChan 10
    print inotify
    let paths = map BS.pack files
    traverse_ (\file -> addWatch inotify [Modify] file (const $ writeBChan bchan Rerun)) paths
    return $ Just bchan

run :: Options -> IO DT.Text
run options = do
  isTTY <- queryTerminal IO.stdInput
  let fromStdin = useStdin options
  when (isTTY && fromStdin) $ error "cannot use --from-stdin option when stdin is a TTY"
  stdin <- if fromStdin then getContents else pure ""
  let initialState = State {
        _input = "", _stdInput = stdin, options = opts, _output = "", _errorMessage = Nothing, -- _search = "",
        _currentInput = ""
        }
      f = mkForm initialState
      buildVty = do
        tty <- openFd "/dev/tty" ReadWrite Nothing defaultFileFlags
        config <- V.standardIOConfig
        V.mkVty $ config { V.inputFd = Just tty, V.outputFd = Just tty }
  initialVty <- buildVty
  notify <- watch $ watchFiles opts
  result <- M.customMain initialVty buildVty notify app f
  return $ formState result ^. output
