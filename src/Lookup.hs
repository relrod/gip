{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Lookup where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B
import Data.GeoIP2
import qualified Data.IP as IP
import qualified Data.Text as T
import Web.Scotty.Trans

data UIGeoResult
  = UIGeoResult GeoResult
  | UIGeoResultError String
  deriving (Eq, Show)

newtype UILocation = UILocation Location deriving (Eq, Show)

instance ToJSON UIGeoResult where
  toJSON (UIGeoResult (GeoResult con conCode iso country loc city postal subdiv)) =
    object [ "continent" .= con
           , "continent_code" .= conCode
           , "country_iso" .= iso
           , "country" .= country
           , "location" .= fmap UILocation loc
           , "city" .= city
           , "postal_code" .= postal
           , "subdivisions" .= subdiv
           ]
  toJSON (UIGeoResultError err) =
    object [ "error" .= err ]

instance ToJSON UILocation where
  toJSON (UILocation (Location lat lon tz accuracy)) =
    object [ "latitude" .= lat
           , "longitude" .= lon
           , "timezone" .= tz
           , "accuracy" .= accuracy
           ]

prettyJson :: (ToJSON a, ScottyError e, Monad m) => a -> ActionT e m ()
prettyJson v = do
    setHeader "Content-Type" "application/json; charset=utf-8"
    raw $ prettyJson' v

prettyJson' :: ToJSON a => a -> B.ByteString
prettyJson' = encodePretty' (defConfig { confIndent = Spaces 2 })
