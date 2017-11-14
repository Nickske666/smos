{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw
    ( smosDraw
    ) where

import Import

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Time
import Data.Tree hiding (drawForest, drawTree)

import Brick.Types as B
import Brick.Widgets.Center as B
import Brick.Widgets.Core as B
import Graphics.Vty.Input.Events (Key(..), Modifier(..))

import Cursor.Text
import Cursor.TextField
import Cursor.Tree

import Smos.Data

import Smos.Cursor
import Smos.Cursor.Entry
import Smos.Style
import Smos.Types

smosDraw :: SmosState -> [Widget ResourceName]
smosDraw SmosState {..} = [maybe drawNoContent renderCursor smosStateCursor]
  where
    renderCursor :: ACursor -> Widget ResourceName
    renderCursor cur =
        drawForest msel for <=> str (show rsel) <=>
        drawHistory smosStateKeyHistory
      where
        msel = Just rsel
        rsel = reverse $ selection $ selectAnyCursor cur
        for = smosFileForest $ rebuild cur

drawNoContent :: Widget n
drawNoContent =
    B.vCenterLayer $
    B.vBox $
    map B.hCenterLayer
        [ str "SMOS"
        , str " "
        , str "version 0.0.0"
        , str "by Tom Sydney Kerckhove"
        , str "Smos is open source and freely distributable"
        ]

drawForest :: Maybe [Int] -> Forest Entry -> Widget ResourceName
drawForest = foldForestSel drawTree $ padLeft (Pad 2) . B.vBox . map snd

drawTree :: Maybe [Int] -> Tree Entry -> Widget ResourceName
drawTree = foldTreeSel drawEntry drawForest (<=>)

drawEntry :: Maybe [Int] -> Entry -> Widget ResourceName
drawEntry msel =
    foldEntrySel
        drawTodoStateInfo
        drawHeader
        drawTags
        drawTimestamps
        drawProperties
        drawContents
        drawLogbook
        (\(mts, tsh) h tgs tss pss mc lb ->
             withSel msel $
             B.vBox
                 [ B.hBox $
                   intersperse (B.txt " ") $
                   [B.txt ">"] ++ maybeToList mts ++ [h, tgs]
                 , tss
                 , pss
                 , fromMaybe emptyWidget mc
                 , lb
                 , tsh
                 ])
        msel

drawTodoStateInfo ::
       Maybe [Int]
    -> StateHistory
    -> (Maybe (Widget ResourceName), Widget ResourceName)
drawTodoStateInfo msel sh =
    (drawTodoState msel sh, drawTodoStateHistory msel sh)

drawTodoState :: Maybe [Int] -> StateHistory -> Maybe (Widget n)
drawTodoState msel sh = do
    ts <- stateHistoryState sh
    pure $
        withSel msel $
        withAttr todoStateAttr $
        withAttr (todoStateSpecificAttr ts) $ B.txt $ todoStateText ts

drawTodoStateHistory :: Maybe [Int] -> StateHistory -> Widget n
drawTodoStateHistory msel sh =
    let es = unStateHistory sh
    in withSel msel $
       withAttr todoStateHistoryAttr $
       B.vBox $
       flip map es $ \StateHistoryEntry {..} ->
           hBox
               [ drawBoxedTimestamp stateHistoryEntryTimestamp
               , B.txt " "
               , case stateHistoryEntryNewState of
                     Just ts -> B.txt $ todoStateText ts
                     Nothing -> B.txt ""
               ]

drawHeader :: Maybe [Int] -> Header -> Widget ResourceName
drawHeader msel Header {..} = withAttr headerAttr $ withTextSel msel headerText

drawContents :: Maybe [Int] -> Contents -> Widget ResourceName
drawContents msel Contents {..} =
    withAttr contentsAttr $ withTextFieldSel msel contentsText

drawTags :: Maybe [Int] -> [Tag] -> Widget ResourceName
drawTags msel =
    withSel msel . foldTagsSel drawTag (B.hBox . addColons . map snd) msel
  where
    addColons ls =
        case ls of
            [] -> []
            _ -> colon : intersperse colon ls ++ [colon]
      where
        colon = B.txt ":"

drawTag :: Maybe [Int] -> Tag -> Widget ResourceName
drawTag msel Tag {..} = withAttr tagAttr $ withTextSel msel tagText

drawTimestamps ::
       Maybe [Int] -> HashMap TimestampName UTCTime -> Widget ResourceName
drawTimestamps msel tss =
    withSel msel $
    B.vBox $
    flip map (HM.toList tss) $ \(k, ts) ->
        B.hBox [B.txt $ timestampNameText k, B.txt ": ", drawTimestamp ts]

drawProperties ::
       Maybe [Int] -> HashMap PropertyName PropertyValue -> Widget ResourceName
drawProperties msel pss =
    withSel msel $
    B.vBox $
    flip map (HM.toList pss) $ \(k, p) ->
        B.hBox
            [ B.txt $ propertyNameText k
            , B.txt ": "
            , B.txt $ propertyValueText p
            ]

drawLogbook :: Maybe [Int] -> Logbook -> Widget n
drawLogbook msel = withSel msel . go
  where
    go lb =
        case lb of
            LogOpen start es ->
                B.hBox [str "[", drawTimestamp start, str "]"] <=>
                B.vBox (map drawLogbookEntry es)
            LogClosed es -> B.vBox $ map drawLogbookEntry es

drawLogbookEntry :: LogbookEntry -> Widget n
drawLogbookEntry LogbookEntry {..} =
    B.hBox
        [ drawBoxedTimestamp logbookEntryStart
        , str "--"
        , drawBoxedTimestamp logbookEntryEnd
        ]

drawBoxedTimestamp :: UTCTime -> Widget n
drawBoxedTimestamp ts = B.hBox [str "[", drawTimestamp ts, str "]"]

drawTimestamp :: UTCTime -> Widget n
drawTimestamp = B.str . formatTime defaultTimeLocale "%F %R"

withSel :: Maybe [Int] -> Widget n -> Widget n
withSel msel =
    case msel of
        Nothing -> id
        Just [] -> withAttr selectedAttr
        Just _ -> id

withTextSel :: Maybe [Int] -> Text -> Widget ResourceName
withTextSel =
    foldTextSel $ \mix t ->
        case mix of
            Nothing -> B.txt t
            Just ix_ ->
                withAttr selectedAttr $
                B.showCursor textCursorName (B.Location (ix_, 0)) $ B.txt t

withTextFieldSel :: Maybe [Int] -> Text -> Widget ResourceName
withTextFieldSel =
    foldTextFieldSel $ \mixs t ->
        let ls = T.splitOn "\n" t
            textOrSpace t_ =
                if T.null t
                    then B.txt " "
                    else B.txt t_
            tw = B.vBox $ map textOrSpace ls
        in case mixs of
               Nothing -> tw
               Just (yix_, xix_) ->
                   withAttr selectedAttr $
                   B.showCursor textCursorName (B.Location (xix_, yix_)) tw

drawHistory :: [KeyPress] -> Widget n
drawHistory = strWrap . unwords . map showKeypress . reverse
  where
    showKeypress (KeyPress key mods) =
        case mods of
            [] -> showKey key
            _ -> intercalate "-" $ map showMod mods ++ [showKey key]
    showKey (KChar c) = [c]
    showKey (KFun i) = "F" ++ show i
    showKey k = show k
    showMod MShift = "S"
    showMod MCtrl = "C"
    showMod MMeta = "M"
    showMod MAlt = "A"
