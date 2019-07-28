{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V

import qualified Data.Text as DT

import Lens.Micro.TH

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Types
  ( Widget
  , ViewportType(Horizontal, Vertical, Both)
  )
import Brick.AttrMap
  ( attrMap
  )
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )
import Brick.Widgets.Core
  ( hLimit
  , vLimit
  , hBox
  , vBox
  , viewport
  , str
  )
import System.Process
import System.Exit (ExitCode(..))

data Name = VP1
          | InputField
          deriving (Ord, Show, Eq)

data State = State {
  _output :: String,
    _errorMessage :: Maybe String,
  _input :: DT.Text,
    _search :: DT.Text
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
appEvent s (T.VtyEvent (V.EvKey V.KEnter [])) = do
  let cmdOut = do
        out <- getOutput $ DT.unpack text
        return $ DT.pack out
  M.continue $ output s cmdOut
  where
    text = _input $ formState s

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

getOutput :: String -> IO String
getOutput input = do
  let cmd = "/home/claudio/.nix-profile/bin/jq"
  let args = ["--color-output", input, "/home/claudio/Downloads/RBMN_API.json"]
  (exitc, stdout, err) <- readProcessWithExitCode cmd args ""
  case exitc of
    ExitFailure _ -> error err
    ExitSuccess -> pure stdout

main :: IO ()
main = do
  output <- getOutput "."
  let initialState = State { _input = ".", _output = output, _errorMessage = Nothing, _search = "" }
      f = mkForm initialState
  void $ M.defaultMain app f
