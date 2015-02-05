{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Control.Monad (mzero)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types (Parser)
import Text.Julius
import Yesod.WebSockets
import Turing.Examples
import Turing.Machine

data Cmd = Beginning | Backward | Forward | Poll deriving Read

data UpdM = UpdTape String | UpdTrans String
instance FromJSON UpdM where
  parseJSON (Object v) = cons =<< (v .: "type")
    where cons :: String -> Parser UpdM
          cons t = (if t == "tape" then UpdTape else UpdTrans) <$> v .: "value"
  parseJSON _ = mzero
instance ToJSON UpdM where
  toJSON um = object ["type" .= ty, "value" .= v]
    where (ty, v) = case um of
                     UpdTape s -> ("tape"::String, s)
                     UpdTrans s -> ("trans", s)

wsTuring :: WebSocketsT Handler ()
wsTuring = do sendTextData . encode . UpdTrans . showTrans . trans $
                defaultMachine
              evalLoop defaultMachine [defaultConfig]
    where defaultMachine = mWriteStr "Hello!"
          defaultConfig = mkConfig . map charToSym $  "_"
          evalLoop m [] = evalLoop m [defaultConfig]
          evalLoop m cfgs@(c:cs) = do
            sendTextData . encode . UpdTape . show $ c
            cmd <- receiveData
            case (read . T.unpack $ cmd) of
              Beginning -> evalLoop defaultMachine [defaultConfig]
              Backward -> evalLoop m cs
              Forward -> case step c (delta m) of
                           Just c' -> evalLoop m (c':cfgs)
                           Nothing -> evalLoop m cfgs
              Poll -> evalLoop m cfgs

getHomeR :: Handler Html
getHomeR = do
  webSockets wsTuring
  defaultLayout $ do
    setTitle "Turing simulator"

    -- JQuery
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"
    -- Bootstrap CSS
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
    -- Bootstrap JS
    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"

    -- Identifiers used in HTML/JS code
    butBeginning <- newIdent
    butBackward <- newIdent
    butTogglePlay <- newIdent
    butForward <- newIdent
    prTape <- newIdent
    prTrans <- newIdent

    $(widgetFile "home")
