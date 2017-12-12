{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor
    ( SmosFileCursor(..)
    , makeSmosFileCursor
    , SmosFileView(..)
    , smosFileCursorAL
    , AnyCursor(..)
    , makeAnyCursor
    , reselectCursor
    , ACursor(..)
    , selectACursor
    , selectAnyCursor
    , View(..)
    , Rebuild(..)
    , Build(..)
    , Select(..)
    , module Smos.Cursor.Contents
    , module Smos.Cursor.Entry
    , module Smos.Cursor.Header
    , module Smos.Cursor.State
    , module Smos.Cursor.Tag
    , module Smos.Cursor.Tags
    , module Smos.Cursor.Timestamp
    , module Smos.Cursor.TimestampName
    , module Smos.Cursor.Timestamps
    ) where

import Import

import Lens.Micro

import Cursor.Class
import Cursor.Select
import Cursor.Tree

import Smos.Data

import Smos.Cursor.Contents
import Smos.Cursor.Entry
import Smos.Cursor.Header
import Smos.Cursor.State
import Smos.Cursor.Tag
import Smos.Cursor.Tags
import Smos.Cursor.Timestamp
import Smos.Cursor.TimestampName
import Smos.Cursor.Timestamps
import Smos.View

makeSmosFileCursor :: SmosFile -> Maybe SmosFileCursor
makeSmosFileCursor sf = SmosFileCursor <$> selectACursor (makeAnyCursor sf)

newtype SmosFileView = SmosFileView
    { smosFileViewForest :: Select (ForestView EntryView)
    } deriving (Show, Eq, Generic)

instance View SmosFileView where
    type Source SmosFileView = SmosFile
    source = SmosFile . source . selectValue . smosFileViewForest
    view SmosFile {..} =
        SmosFileView {smosFileViewForest = select $ view smosFileForest}

instance Selectable SmosFileView where
    applySelection msel sfv =
        sfv
        { smosFileViewForest =
              select $
              applySelection msel $ selectValue $ smosFileViewForest sfv
        }

newtype SmosFileCursor = SmosFileCursor
    { fileCursorA :: ACursor
    } deriving (Show, Eq, Generic)

instance Validity SmosFileCursor

instance Rebuild SmosFileCursor where
    type ReBuilding SmosFileCursor = SmosFileView
    rebuild SmosFileCursor {..} =
        SmosFileView {smosFileViewForest = rebuild fileCursorA}
    selection = selection . fileCursorA

smosFileCursorAL :: Lens' SmosFileCursor ACursor
smosFileCursorAL = lens fileCursorA $ \fc a -> fc {fileCursorA = a}

data AnyCursor
    = AnyForest (ForestCursor EntryCursor)
    | AnyTree (TreeCursor EntryCursor)
    | AnyEntry EntryCursor
    | AnyHeader HeaderCursor
    | AnyContents ContentsCursor
    | AnyState StateCursor
    | AnyTags TagsCursor
    | AnyTimestamps TimestampsCursor
    deriving (Show, Eq, Generic)

instance Validity AnyCursor

instance Rebuild AnyCursor where
    type ReBuilding AnyCursor = Select (ForestView EntryView)
    rebuild ac =
        case ac of
            AnyForest fc -> rebuild fc
            AnyTree tc -> rebuild tc
            AnyEntry ec -> rebuild ec
            AnyHeader hc -> rebuild hc
            AnyContents cc -> rebuild cc
            AnyState sc -> rebuild sc
            AnyTags tsc -> rebuild tsc
            AnyTimestamps tsc -> rebuild tsc
    selection (AnyForest fc) = selection fc
    selection (AnyTree tc) = selection tc
    selection (AnyEntry ec) = selection ec
    selection (AnyHeader hc) = selection hc
    selection (AnyContents cc) = selection cc
    selection (AnyState sc) = selection sc
    selection (AnyTags tsc) = selection tsc
    selection (AnyTimestamps tsc) = selection tsc

data ACursor
    = AnEntry EntryCursor
    | AHeader HeaderCursor
    | AContents ContentsCursor
    | ATags TagsCursor
    | ATimestamps TimestampsCursor
    deriving (Show, Eq, Generic)

instance Validity ACursor

instance Rebuild ACursor where
    type ReBuilding ACursor = Select (ForestView EntryView)
    rebuild ac =
        case ac of
            AnEntry ec -> rebuild ec
            AHeader hc -> rebuild hc
            AContents cc -> rebuild cc
            ATags tc -> rebuild tc
            ATimestamps tsc -> rebuild tsc
    selection (AnEntry ec) = selection ec
    selection (AHeader hc) = selection hc
    selection (AContents cc) = selection cc
    selection (ATags tc) = selection tc
    selection (ATimestamps tsc) = selection tsc

makeAnyCursor :: SmosFile -> AnyCursor
makeAnyCursor SmosFile {..} = AnyForest $ makeForestCursor' smosFileForest

reselectCursor :: [Int] -> SmosFile -> AnyCursor
reselectCursor s = go (reverse s) . makeAnyCursor
  where
    go sel (AnyForest fc) = gof sel fc
    go sel (AnyTree tc) = got sel tc
    go sel (AnyEntry ec) = goe sel ec
    go sel (AnyHeader hc) = goh sel hc
    go sel (AnyContents cc) = goc sel cc
    go sel (AnyState sc) = gos sel sc
    go sel (AnyTags tsc) = gotgs sel tsc
    go sel (AnyTimestamps tsc) = gotss sel tsc
    gof sel fc =
        withSel sel (AnyForest fc) $ \ix_ sel_ ->
            fromMaybe (AnyForest fc) $
            got sel_ <$> forestCursorElems fc `atMay` ix_
    got sel tc =
        withSel sel (AnyTree tc) $ \ix_ sel_ ->
            case ix_ of
                0 -> goe sel_ $ treeCursorValue tc
                1 -> gof sel_ $ treeCursorForest tc
                _ -> AnyTree tc
    goe sel e =
        withSel sel (AnyEntry e) $ \ix_ sel_ ->
            case ix_ of
                0 -> gos sel_ $ entryCursorState e
                1 -> goh sel_ $ entryCursorHeader e
                2 -> maybe (AnyEntry e) (gotgs sel_) $ entryCursorTags e
                3 -> maybe (AnyEntry e) (gotss sel_) $ entryCursorTimestamps e
                -- 4: properties
                5 -> maybe (AnyEntry e) (goc sel_) $ entryCursorContents e
                -- 6: clock
                _ -> AnyEntry e
    goh _ = AnyHeader
    goc _ = AnyContents
    gos _ = AnyState
    gotgs _ = AnyTags
    gotss _ = AnyTimestamps
    withSel :: [Int] -> a -> (Int -> [Int] -> a) -> a
    withSel sel a func =
        case sel of
            [] -> a
            (ix_:rest) -> func ix_ rest

selectACursor :: AnyCursor -> Maybe ACursor
selectACursor ac =
    case ac of
        AnyForest fc -> AnEntry . treeCursorValue <$> forestCursorSelectFirst fc
        AnyTree tc -> Just $ AnEntry $ treeCursorValue tc
        AnyEntry ec -> Just $ AnEntry ec
        AnyHeader hc -> Just $ AHeader hc
        AnyContents cc -> Just $ AContents cc
        AnyState sc -> Just $ AnEntry $ stateCursorParent sc
        AnyTags tsc -> Just $ ATags tsc
        AnyTimestamps tsc -> Just $ ATimestamps tsc

selectAnyCursor :: ACursor -> AnyCursor
selectAnyCursor ac =
    case ac of
        AnEntry hc -> AnyEntry hc
        AHeader hc -> AnyHeader hc
        AContents cc -> AnyContents cc
        ATags tc -> AnyTags tc
        ATimestamps tsc -> AnyTimestamps tsc
