{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Actions
    ( save
    , stop
    -- * Entry and tree actions
    , insertTreeAbove
    , insertTreeBelow
    , insertTreeChild
    , deleteCurrentHeader
    , moveUp
    , moveDown
    , moveLeft
    , moveRight
    , clockIn
    , clockOut
    -- * Header actions
    , enterHeader
    , headerInsert
    , headerRemove
    , headerDelete
    , headerLeft
    , headerRight
    , headerStart
    , headerEnd
    , exitHeader
    -- * Todo state actions
    , enterTodoState
    , todoStateClear
    , todoStateSet
    , exitTodoState
    -- * Helper functions to define your own actions
    , modifyEntryM
    , modifyEntry
    , modifyHeaderM
    , modifyHeader
    , modifyTodoState
    , modifyCursor
    , modifyMCursor
    , withEntryCursor
    , withHeaderCursor
    , withStateCursor
    , withFullMod
    , withAgendaFilesMod
    , module Control.Monad.Reader
    , module Control.Monad.State
    ) where

import Import

import Control.Monad.Reader
import Control.Monad.State

import Data.Time

import Smos.Data

import Smos.Cursor
import Smos.Types

{-# ANN module ("HLint: ignore Use fromMaybe" :: String) #-}

emptyTree :: SmosTree
emptyTree = SmosTree {treeEntry = newEntry "", treeForest = SmosForest []}

initEntryCursor :: Maybe ACursor
initEntryCursor =
    let fc = makeForestCursor $ SmosForest []
        fc' = forestCursorInsertAtStart emptyTree fc
        mtc' = forestCursorSelectFirst fc'
    in (AnEntry . treeCursorEntry) <$> mtc'

save :: SmosM ()
save = do
    file <- gets smosStateFilePath
    mcur <- gets smosStateCursor
    let sf =
            case mcur of
                Nothing -> SmosFile $ SmosForest []
                Just cur -> rebuild cur
    writeSmosFile file sf

insertTreeAbove :: SmosM ()
insertTreeAbove =
    modifyMCursor $ \mcur ->
        case mcur of
            Nothing -> initEntryCursor
            Just (AnEntry ec) ->
                let tc = entryCursorParent ec
                    tc' = treeCursorInsertAbove tc emptyTree
                    ec' = treeCursorEntry tc'
                in Just $ AnEntry ec'
            _ -> mcur

insertTreeBelow :: SmosM ()
insertTreeBelow =
    modifyMCursor $ \mcur ->
        case mcur of
            Nothing -> initEntryCursor
            Just (AnEntry ec) ->
                let tc = entryCursorParent ec
                    tc' = treeCursorInsertBelow tc emptyTree
                    ec' = treeCursorEntry tc'
                in Just $ AnEntry ec'
            _ -> mcur

insertTreeChild :: SmosM ()
insertTreeChild =
    modifyMCursor $ \mcur ->
        case mcur of
            Nothing -> initEntryCursor
            Just (AnEntry ec) ->
                let tc = entryCursorParent ec
                    tc' = treeCursorInsertChildAtStart emptyTree tc
                    ec' = treeCursorEntry tc'
                    fc' = treeCursorForest tc'
                    mtc' = forestCursorSelectFirst fc'
                    mec' = treeCursorEntry <$> mtc'
                in Just $ maybe (AnEntry ec') AnEntry mec'
            _ -> mcur

deleteCurrentHeader :: SmosM ()
deleteCurrentHeader =
    modifyMCursor $ \mcur ->
        case mcur of
            Just (AnEntry ec) ->
                let tc = entryCursorParent ec
                    eft = treeCursorDeleteCurrent tc
                    mec' =
                        case eft of
                            Left fc ->
                                case forestCursorParent fc of
                                    Nothing -> Nothing
                                    Just tc_ -> Just $ treeCursorEntry tc_
                            Right tc_ -> Just $ treeCursorEntry tc_
                in AnEntry <$> mec'
            _ -> mcur

moveUp :: SmosM ()
moveUp =
    modifyEntry $ \ec ->
        let tc = entryCursorParent ec
            -- First try to go to the last element recursively of the previous node
            recursivelyDeepestOf t =
                let fc = treeCursorForest t
                in case forestCursorSelectLast fc
                    -- Then try to go to the directly previous node
                         of
                       Nothing -> t
                       Just t_ -> recursivelyDeepestOf t_
            tc' =
                case treeCursorSelectPrev tc of
                    Just t_ -> recursivelyDeepestOf t_
                    Nothing -- Then try to go up
                     ->
                        let fc = treeCursorParent tc
                        in case forestCursorParent fc
                        -- If all else fails, stay where we are
                                 of
                               Nothing -> tc
                               Just tc_ -> tc_
            ec' = treeCursorEntry tc'
        in ec'

moveDown :: SmosM ()
moveDown =
    modifyEntry $ \ec ->
        let tc = entryCursorParent ec
            -- First try to go down the current tree's forrest
            goNextViaDown t =
                case forestCursorElems $ treeCursorForest t of
                    [] -> Nothing
                    (tc_:_) -> Just tc_
            goNextViaUp t
                -- Then try to go to the direct next element
             =
                case treeCursorSelectNext t of
                    Just tc_ -> tc_
                    Nothing
                        -- Then try to go recursively upward until there is a next element
                     ->
                        let fc = treeCursorParent t
                        in case forestCursorParent fc of
                               Just tc_ -> goNextViaUp tc_
                               -- If all else fails, stay where we are
                               Nothing -> tc -- CHECKME
            tc' =
                case goNextViaDown tc of
                    Nothing -> goNextViaUp tc
                    Just tc_ -> tc_
            ec' = treeCursorEntry tc'
        in ec'

moveLeft :: SmosM ()
moveLeft =
    modifyEntry $ \ec ->
        let tc = entryCursorParent ec
            tc' = fromMaybe tc $ forestCursorParent $ treeCursorParent tc
            ec' = treeCursorEntry tc'
        in ec'

moveRight :: SmosM ()
moveRight =
    modifyEntry $ \ec ->
        let tc = entryCursorParent ec
            fc = treeCursorForest tc
            tc' = fromMaybe tc $ forestCursorSelectFirst fc
            ec' = treeCursorEntry tc'
        in ec'

clockIn :: SmosM ()
clockIn = do
    now <- liftIO getCurrentTime
    withFullMod $ clockOutMod now
    modifyEntryM $ entryCursorClockIn now

clockOut :: SmosM ()
clockOut = do
    now <- liftIO getCurrentTime
    withFullMod $ clockOutMod now
    withAgendaFilesMod $ const $ clockOutMod now

clockOutMod :: UTCTime -> (SmosFile -> SmosFile)
clockOutMod now = SmosFile . gof . smosFileForest
  where
    gof sf = SmosForest {smosTrees = map got $ smosTrees sf}
    got SmosTree {..} =
        SmosTree {treeEntry = goe treeEntry, treeForest = gof treeForest}
    goe e =
        e
        { entryLogbook =
              fromMaybe (entryLogbook e) $ clockOutAt now $ entryLogbook e
        }

enterHeader :: SmosM ()
enterHeader =
    modifyCursor $ \cur ->
        case cur of
            AnEntry ec -> AHeader $ entryCursorHeader ec
            _ -> cur

headerInsert :: Char -> SmosM ()
headerInsert c = modifyHeader $ \hc -> headerCursorInsert c hc

headerRemove :: SmosM ()
headerRemove = modifyHeaderM headerCursorRemove

headerDelete :: SmosM ()
headerDelete = modifyHeaderM headerCursorDelete

headerLeft :: SmosM ()
headerLeft = modifyHeaderM headerCursorLeft

headerRight :: SmosM ()
headerRight = modifyHeaderM headerCursorRight

headerStart :: SmosM ()
headerStart = modifyHeader headerCursorStart

headerEnd :: SmosM ()
headerEnd = modifyHeader headerCursorEnd

exitHeader :: SmosM ()
exitHeader =
    modifyCursor $ \cur ->
        case cur of
            AHeader hc -> AnEntry $ headerCursorParent hc
            _ -> cur

enterTodoState :: SmosM ()
enterTodoState =
    modifyCursor $ \cur ->
        case cur of
            AnEntry ec -> AState $ entryCursorState ec
            _ -> cur

todoStateClear :: SmosM ()
todoStateClear = modifyTodoState stateCursorClear

todoStateSet :: TodoState -> SmosM ()
todoStateSet = modifyTodoState . stateCursorSetState

exitTodoState :: SmosM ()
exitTodoState =
    modifyCursor $ \cur ->
        case cur of
            AState hc -> AnEntry $ stateCursorParent hc
            _ -> cur

modifyEntryM :: (EntryCursor -> Maybe EntryCursor) -> SmosM ()
modifyEntryM func = modifyEntry $ \hc -> fromMaybe hc $ func hc

modifyEntry :: (EntryCursor -> EntryCursor) -> SmosM ()
modifyEntry func =
    modifyCursor $ \cur ->
        case cur of
            AnEntry h -> AnEntry $ func h
            _ -> cur

modifyHeaderM :: (HeaderCursor -> Maybe HeaderCursor) -> SmosM ()
modifyHeaderM func = modifyHeader $ \hc -> fromMaybe hc $ func hc

modifyHeader :: (HeaderCursor -> HeaderCursor) -> SmosM ()
modifyHeader func =
    modifyCursor $ \cur ->
        case cur of
            AHeader h -> AHeader $ func h
            _ -> cur

modifyTodoState :: (StateCursor -> StateCursor) -> SmosM ()
modifyTodoState func =
    modifyCursor $ \cur ->
        case cur of
            AState h -> AState $ func h
            _ -> cur

modifyCursor :: (ACursor -> ACursor) -> SmosM ()
modifyCursor func = modifyMCursor $ \mc -> func <$> mc

modifyMCursor :: (Maybe ACursor -> Maybe ACursor) -> SmosM ()
modifyMCursor func =
    modify $ \ss -> ss {smosStateCursor = func $ smosStateCursor ss}

withEntryCursor :: (EntryCursor -> SmosM ()) -> SmosM ()
withEntryCursor func = do
    ss <- get
    case smosStateCursor ss of
        Just (AnEntry fc) -> func fc
        _ -> pure ()

withHeaderCursor :: (HeaderCursor -> SmosM ()) -> SmosM ()
withHeaderCursor func = do
    ss <- get
    case smosStateCursor ss of
        Just (AHeader fc) -> func fc
        _ -> pure ()

withStateCursor :: (StateCursor -> SmosM ()) -> SmosM ()
withStateCursor func = do
    ss <- get
    case smosStateCursor ss of
        Just (AState fc) -> func fc
        _ -> pure ()

withFullMod :: (SmosFile -> SmosFile) -> SmosM ()
withFullMod func =
    modify $ \ss ->
        case smosStateCursor ss of
            Nothing -> ss
            Just cur ->
                let sf = rebuild cur
                    sel = makeASelection $ selectAnyCursor cur
                    sf' = func sf
                    cur' = selectACursor $ reselect sel sf'
                in ss {smosStateCursor = cur'}

withAgendaFilesMod :: (Path Abs File -> SmosFile -> SmosFile) -> SmosM ()
withAgendaFilesMod func = do
    getAgendaFiles <- asks configAgendaFiles
    files <- liftIO getAgendaFiles
    forM_ files $ \file -> do
        errOrSF <- readSmosFile file
        let result =
                case errOrSF of
                    Nothing -> Nothing
                    Just (Left _) -> Nothing
                    Just (Right sf) -> Just $ func file sf
        case result of
            Nothing -> pure ()
            Just sf' -> writeSmosFile file sf'
