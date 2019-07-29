{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module UI (run) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid            ((<>))
import qualified Graphics.Vty           as V

import qualified Data.Text              as DT

import           Lens.Micro             ((%~), (&), (.~), (^.))
import           Lens.Micro.TH

import           Brick.AttrMap          (attrMap)
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
import qualified Brick.Types            as T
import qualified Brick.Widgets.Border   as B
import qualified Brick.Widgets.Center   as C
import           Brick.Widgets.Core     (hBox, hLimit, str, vBox, vLimit,
                                         viewport)
import           Lib                    (Options(..), getOutput)

data Name = VP1
          | InputField
          deriving (Ord, Show, Eq)

data State = State {
  _output       :: String,
  options       :: Options,
  _errorMessage :: Maybe String,
  _input        :: DT.Text,
  _search       :: DT.Text
  }

makeLenses ''State

drawUi :: Form State e Name -> [Widget Name]
drawUi f = [ui]
    where
        ui = vBox [ pair, B.hBorder, form ]
        form = renderForm f
        state = formState f
        -- input = vBox [str "input" ]
        pair = hBox [ viewport VP1 Vertical $
                      vBox [str $ _output state]
                    ]

vp1Scroll :: M.ViewportScroll Name
vp1Scroll = M.viewportScroll VP1

mkForm :: State -> Form State e Name
mkForm = newForm [ editTextField input InputField (Just 1) ]

appEvent :: (Form State e Name) -> T.BrickEvent Name e -> T.EventM Name (T.Next (Form State e Name))
appEvent s (T.VtyEvent (V.EvKey V.KDown []))  = M.vScrollBy vp1Scroll 1 >> M.continue s
appEvent s (T.VtyEvent (V.EvKey V.KUp []))    = M.vScrollBy vp1Scroll (-1) >> M.continue s
appEvent s (T.VtyEvent (V.EvKey V.KPageDown []))    = M.vScrollPage vp1Scroll T.Down >> M.continue s
appEvent s (T.VtyEvent (V.EvKey V.KPageUp []))    = M.vScrollPage vp1Scroll T.Up >> M.continue s
appEvent s (T.VtyEvent (V.EvKey V.KHome []))    = M.vScrollToBeginning vp1Scroll >> M.continue s
appEvent s (T.VtyEvent (V.EvKey V.KEnd []))    = M.vScrollToEnd vp1Scroll >> M.continue s
appEvent s (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt s
appEvent f (T.VtyEvent (V.EvKey V.KEnter [])) =
  do
    out <- liftIO $ getOutput cmdargs (DT.unpack text)
    M.continue $ mkForm $ state & output .~ out
  where
    state = formState f
    text = state^.input
    cmdargs = cmdline $ options state


appEvent s ev = do
  s' <- handleFormEvent ev s
  M.continue s'

app :: M.App (Form State e Name) e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.neverShowCursor
          }


run :: Options -> IO()
run options = do
  output <- getOutput (cmdline options) "."
  let initialState = State {
        _input = ".", options = options, _output = output, _errorMessage = Nothing, _search = ""
        }
      f = mkForm initialState
  void $ M.defaultMain app f
