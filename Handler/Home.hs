{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Julius
import Yesod.WebSockets
import Turing.Examples
import Turing.Machine

data Cmd = Beginning | Backward | Forward | Poll | MHello | MSub deriving Read

data UpdM = UpdTape String | UpdTrans String | Stop | Bold Int
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
                     Bold n -> ("bold", show n)

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
            sendTextData . encode $ Bold transRow
            sendTextData . encode . UpdTape . show $ c
            cmd <- receiveData
            case read . T.unpack $ cmd of
              Beginning -> evalLoop m ic [ic]
              Backward -> evalLoop m ic cs
              Forward -> case step c (delta m) of
                           Just c' -> evalLoop m ic (c':cfgs)
                           Nothing -> do sendTextData . encode $ Stop
                                         evalLoop m ic cfgs
              Poll -> evalLoop m ic cfgs
              MHello -> startMachine mHello defaultConfig
              MSub -> startMachine mSub mSubConfig
            where transRow = fIdx (state c, tape c !! pos c) (Map.toList (trans m))
                  fIdx y xs = fromMaybe (-1) (fIdx' y xs)
                  fIdx' _ [] = Nothing
                  fIdx' (st1,sym1) (((st2,sym2),_):xs) =
                    if st1 == st2 && (sym1 == sym2 || sym2 == SymWild)
                    then Just 0
                    else fmap (+1) (fIdx' (st1,sym1) xs)

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
