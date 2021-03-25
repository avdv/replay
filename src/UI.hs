{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module UI (run) where

import           Control.Monad          (unless, void)
import           Control.Monad.Except
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid            ((<>))
import qualified Graphics.Vty           as V

import           Data.Foldable          (traverse_)
import qualified Data.Text              as DT
import qualified Data.ByteString.Char8  as BS

import           Lens.Micro             ((%~), (&), (.~), (^.), (?~))
import           Lens.Micro.TH

import           Brick.AttrMap          (attrMap)
import           Brick.BChan            (BChan, newBChan, writeBChan)
import           Brick.Forms            (Form, allFieldsValid, checkboxField,
                                         editPasswordField, editShowableField,
                                         editTextField, focusedFormInputAttr,
                                         formFocus, formState, handleFormEvent,
                                         invalidFields, invalidFormInputAttr,
                                         newForm, radioField, renderForm,
                                         setFieldValid, (@@=))
import qualified Brick.Main             as M
import           Brick.Types            (ViewportType (Both, Horizontal, Vertical),
                                         Widget)

import qualified Brick.AttrMap          as A
import qualified Brick.Types            as T
import           Brick.Util             (fg, on)
import qualified Brick.Widgets.Border   as B
import qualified Brick.Widgets.Center   as C
import           Brick.Widgets.Core     (cached, hBox, hLimit, str, txt, updateAttrMap,
                                         vBox, vLimit, viewport, withAttr)
import           Lib                    (Options (..), getOutput)
import           System.Posix.IO        (OpenMode (..), defaultFileFlags, openFd)
import           System.INotify

data Name = VP1
          | InputField
          | CachedText
          deriving (Ord, Show, Eq)

data State = State {
  _output       :: DT.Text,
  options       :: Options,
  _errorMessage :: Maybe String,
  _input        :: DT.Text,
  _search       :: DT.Text
  }

makeLenses ''State

errorAttr :: A.AttrName
errorAttr = "error"

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
        error = state^.errorMessage
        errorPane = maybe [] (\m -> [errorWidget m]) error
        pair = hBox [ viewport VP1 Vertical $
                      cached CachedText $ txt $ _output state
                    ]

vp1Scroll :: M.ViewportScroll Name
vp1Scroll = M.viewportScroll VP1

mkForm :: State -> Form State e Name
mkForm = newForm [ editTextField input InputField (Just 1) ]

appEvent :: Form State MyEvents Name -> T.BrickEvent Name MyEvents -> T.EventM Name (T.Next (Form State MyEvents Name))
appEvent f (T.AppEvent Rerun) = M.invalidateCacheEntry CachedText >> rerun f >>= M.continue
appEvent s (T.VtyEvent (V.EvKey V.KDown []))  = M.vScrollBy vp1Scroll 1 >> M.continue s
appEvent s (T.VtyEvent (V.EvKey V.KUp []))    = M.vScrollBy vp1Scroll (-1) >> M.continue s
appEvent s (T.VtyEvent (V.EvKey V.KPageDown []))    = M.vScrollPage vp1Scroll T.Down >> M.continue s
appEvent s (T.VtyEvent (V.EvKey V.KPageUp []))    = M.vScrollPage vp1Scroll T.Up >> M.continue s
appEvent s (T.VtyEvent (V.EvKey V.KHome []))    = M.vScrollToBeginning vp1Scroll >> M.continue s
appEvent s (T.VtyEvent (V.EvKey V.KEnd []))    = M.vScrollToEnd vp1Scroll >> M.continue s
appEvent s (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt s
appEvent f (T.VtyEvent (V.EvKey V.KEnter [])) = M.invalidateCacheEntry CachedText >> rerun f >>= M.continue
appEvent s ev = handleFormEvent ev s >>= M.continue


rerun :: Form State e Name -> T.EventM Name (Form State e Name)
rerun f =
  do
    out <- liftIO $ runExceptT $ getOutput cmdargs (DT.unpack text)
    let newState = case out of
          Right newOutput ->
            state & output .~ DT.pack newOutput
                  & errorMessage .~ Nothing
          Left msg -> state & errorMessage ?~ msg
    return $ mkForm newState
  where
    state = formState f
    text = state^.input
    cmdargs = cmdline $ options state

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

  let initialState = State {
        _input = ".", options = options, _output = "", _errorMessage = Nothing, _search = ""
        }
      f = mkForm initialState
      buildVty = do
        tty <- openFd "/dev/tty" ReadWrite Nothing defaultFileFlags
        config <- V.standardIOConfig
        V.mkVty $ config { V.inputFd = Just tty, V.outputFd = Just tty }
  initialVty <- buildVty
  notify <- watch $ watchFiles options
  result <- M.customMain initialVty buildVty notify app f
  return $ formState result ^. output

