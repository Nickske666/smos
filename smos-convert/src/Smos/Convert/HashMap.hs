{-# LANGUAGE RecordWildCards #-}

module Smos.Convert.HashMap where

import Smos.Convert.Time

import Smos.Data.Types

import qualified Data.OrgMode.Types as Org

import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)
import Data.Hashable

import Data.Time.LocalTime

import Data.Graph.Inductive.Query.Monad ((><))

mapHashMap ::
       (Eq c, Hashable c) => (a -> c) -> (b -> d) -> HashMap a b -> HashMap c d
mapHashMap f1 f2 hashmap = HM.fromList $ (><) f1 f2 <$> HM.toList hashmap

getProperties :: Org.Properties -> HashMap PropertyName PropertyValue
getProperties Org.Properties {..} =
    mapHashMap PropertyName PropertyValue unProperties

getTimestamps :: TimeZone -> Org.Plannings -> HashMap TimestampName Timestamp
getTimestamps timezone (Org.Plns hashmap) =
    mapHashMap toTimestampName (toTimestamp timezone) hashmap
