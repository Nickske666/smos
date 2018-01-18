module Smos.Convert.Logbook where

import Smos.Convert.Time

import qualified Smos.Data.Types as Smos

import qualified Data.OrgMode.Types as Org

import Data.Time.Clock
import Data.Time.LocalTime

getLogbook :: TimeZone -> Org.Logbook -> Smos.Logbook
getLogbook timezone logbook =
    Smos.LogClosed $ toLogbookEntry timezone <$> Org.unLogbook logbook

toLogbookEntry :: TimeZone -> Org.Clock -> Smos.LogbookEntry
toLogbookEntry timezone clock =
    case Org.unClock clock of
        (Nothing, _) -> error "This is a clock without a start time"
        (_, Nothing) -> error "This is a clock without a duration"
        (Just start, Just duration) ->
            let entryStart = toUTCTime timezone $ Org.tsTime start
                entryEnd = addUTCTime (toNominalDiffTime duration) entryStart
            in Smos.LogbookEntry entryStart entryEnd
