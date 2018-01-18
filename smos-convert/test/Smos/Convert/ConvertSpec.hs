{-# LANGUAGE OverloadedStrings #-}

module Smos.Convert.ConvertSpec where

import TestImport

import Smos.Convert.EntryTree
import Smos.Convert.Gen ()
import Smos.Convert.Time
import Smos.Data.Types

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

import qualified Data.HashMap.Lazy as HM
import Data.OrgMode.Types

import Data.GenValidity.Time.LocalTime ()

spec :: Spec
spec = do
    describe "toLocalTime" $ do
        it "produces valid instances" $ producesValidsOnValids toLocalTime
        it "properly converts orgmode's datetimes into localtimes" $
            let ymd = YearMonthDay 2010 5 1
                hm = Nothing
                datetime = DateTime ymd Nothing hm Nothing Nothing
                day = fromGregorian 2010 5 1
                localtime = LocalTime day $ TimeOfDay 0 0 0
            in toLocalTime datetime `shouldBe` localtime
    describe "toEntryTree" $
        it "unit test" $
        let myTitle = "this is the title"
            myTags = ["tag1", "tag2"]
            myState = "READY"
            now = UTCTime (ModifiedJulianDay 0) $ secondsToDiffTime 0
            myContents = "This is the content"
            myPropertyName = "Title"
            myPropertyValue = "Some book"
            mySection =
                Section
                { sectionTimestamp = Nothing
                , sectionPlannings = Plns HM.empty
                , sectionClocks = []
                , sectionProperties =
                      Properties $ HM.singleton myPropertyName myPropertyValue
                , sectionLogbook = Logbook []
                , sectionDrawers = []
                , sectionParagraph = myContents
                }
            headline =
                Headline
                { depth = Depth 2
                , stateKeyword = Just $ StateKeyword myState
                , priority = Just A
                , title = myTitle
                , timestamp = Nothing
                , stats = Nothing
                , tags = myTags
                , section = mySection
                , subHeadlines = []
                }
            entry =
                Entry
                { entryHeader = Header myTitle
                , entryContents = Just $ Contents myContents
                , entryTimestamps = HM.empty
                , entryProperties =
                      HM.singleton (PropertyName myPropertyName) $
                      PropertyValue myPropertyValue
                , entryStateHistory =
                      StateHistory
                          [StateHistoryEntry (Just $ TodoState myState) now]
                , entryTags = Tag <$> myTags
                , entryLogbook = LogClosed []
                }
            entryTree = Node entry []
        in toEntryTree utc now headline `shouldBe` entryTree
