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

data Cmd = Beginning | Backward | Forward | Poll | MHello | MSub deriving Read

data UpdM = UpdTape String | UpdTrans String | Stop
-- instance FromJSON UpdM where
--   parseJSON (Object v) = cons =<< (v .: "type")
--     where cons :: String -> Parser UpdM
--           cons t = (if t == "tape" then UpdTape else UpdTrans) <$> v .: "value"
--   parseJSON _ = mzero
instance ToJSON UpdM where
  toJSON um = object ["type" .= ty, "value" .= v]
    where (ty, v) = case um of
                     UpdTape s -> ("tape"::String, s)
                     UpdTrans s -> ("trans", s)
                     Stop -> ("stop", "")

wsTuring :: WebSocketsT Handler ()
wsTuring = startMachine mHello defaultConfig
    where startMachine m ic = do
            sendTextData . encode . UpdTrans . showTrans . trans $ m
            evalLoop m ic [ic]
          mHello = mWriteStr "Hello!"
          mSubConfig = mkConfig . map charToSym $ "_aaaaaa_aaa"
          defaultConfig = mkConfig . map charToSym $  "_"
          evalLoop m ic [] = evalLoop m ic [ic]
          evalLoop m ic cfgs@(c:cs) = do
            sendTextData . encode . UpdTape . show $ c
            cmd <- receiveData
            case (read . T.unpack $ cmd) of
              Beginning -> evalLoop m ic [ic]
              Backward -> evalLoop m ic cs
              Forward -> case step c (delta m) of
                           Just c' -> evalLoop m ic (c':cfgs)
                           Nothing -> do sendTextData . encode $ Stop
                                         evalLoop m ic cfgs
              Poll -> evalLoop m ic cfgs
              MHello -> startMachine mHello defaultConfig
              MSub -> startMachine mSub mSubConfig

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
    eHello <- newIdent
    eSub <- newIdent

    $(widgetFile "home")
