{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.GeoIP2
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Lucid
import Lucid.Html5
import Network.SockAddr (showSockAddr)
import Network.Wai (remoteHost)
import Text.Read (readMaybe)
import Web.Scotty

import Lookup

baseTemplate :: Html () -> Html ()
baseTemplate h =
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [ name_ "viewport"
            , content_ "width=device-width, initial-scale=1, shrink-to-fit=no"
            ]
      link_ [ rel_ "stylesheet"
            , href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
            , integrity_ "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
            , crossorigin_ "anonymous"
            ]
      link_ [ rel_ "stylesheet"
            , href_ "https://unpkg.com/leaflet@1.5.1/dist/leaflet.css"
            , integrity_ "sha512-xwE/Az9zrjBIphAcBb3F6JVqxf46+CDLwfLMHloNu6KEQCAWi6HcDUbeOfBIptF7tcCzusKFjFw2yuvEpDL9wQ=="
            , crossorigin_ ""
            ]
      link_ [ rel_ "stylesheet"
            , href_ "https://fonts.googleapis.com/css?family=Open+Sans:300,400,700|Cormorant&subset=latin,latin-ext"
            ]
      style_ "html, body { font-family: 'Open Sans', sans-serif; }\
             \.serif { font-family: 'Cormorant', serif; }\
             \a.link-unstyled {text-decoration: none !important; color: #000;} "
      title_ "GeoIP UI"
    body_ $ do
      --navbar
      h
      footer
      script_ [ src_ "https://code.jquery.com/jquery-3.3.1.min.js"
              ] ("" :: T.Text)
      script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js"
              , integrity_ "sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1"
              , crossorigin_ "anonymous"
              ] ("" :: T.Text)
      script_ [ src_ "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"
              , integrity_ "sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM"
              , crossorigin_ "anonymous"
              ] ("" :: T.Text)
      script_ [ src_ "https://unpkg.com/leaflet@1.5.1/dist/leaflet.js"
              , integrity_ "sha512-GffPMF3RvMeYyc1LWMHtK8EbPv0iNZ8/oTtHPx9/cc2ILxQ+u905qIwdpULaqDkyBKgOaB57QTMg7ztg8Jm2Og=="
              , crossorigin_ ""
              ] ("" :: T.Text)
      script_ [ src_ "https://cdn.jsdelivr.net/gh/google/code-prettify@master/loader/run_prettify.js"
              ] ("" :: T.Text)
      script_ "var map = L.map('map');\
              \function updateMap(data) {\
              \  if (!data['location'] || typeof data['location'] == 'undefined') return;\
              \  $('#map').css('height', $('.ohno').height());\
              \  map.setView([data['location']['latitude'], data['location']['longitude']], 13);\
              \}\
              \L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {\
              \  attribution: '&copy; <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a> contributors'\
              \}).addTo(map);\
              \$(function() {\
              \  if (!$('.ohno').text()) $('#result').hide();\
              \  else updateMap(JSON.parse($('.ohno').text())); \
              \  $('form').submit(function(e) {\
              \    if (!$('#query').val()) return;\
              \    $('#result').show();\
              \    history.pushState('data', '', '/ip/' + $('#query').val());\
              \    $.get('/ip/' + $('#query').val() + '/json', function(data) {\
              \      $('.ohno').html(PR.prettyPrintOne(JSON.stringify(data, null, 2)), 'js');\
              \      updateMap(data);\
              \    });\
              \  });\
              \});"

navbar :: Html ()
navbar =
  nav_ [class_ "navbar navbar-expand-lg navbar-light bg-light"] $
    a_ [class_ "navbar-brand", href_ "/"] "GeoIP"

footer :: Html ()
footer = do
  div_ [id_ "footer", class_ "serif text-center"] $ do
    small_ "© 2019 Rick Elrod "
    small_ $ do
      "This product includes GeoLite2 data created by MaxMind, "
      "available from "
      a_ [href_ "https://www.maxmind.com"] "https://www.maxmind.com"
      "."

index :: TL.Text -> B.ByteString -> Html ()
index host jsonRes = baseTemplate $ do
  div_ [class_ "container"] $ do
    div_ [class_ "row mt-3"] $
      div_ [class_ "col-12"] $
        a_ [href_ "/", class_ "link-unstyled"] $
          h1_ [class_ "display-4 serif"] "IP Address Geolocation."
    hr_ []
    div_ [class_ "row"] $
      div_ [class_ "col-12"] $ do
        form_ $ do
          div_ [class_ "input-group"] $ do
            div_ [class_ "input-group-prepend"] $
              span_ [class_ "input-group-text"] "/ip/"
            input_ [ type_ "text"
                   , id_ "query"
                   , class_ "form-control form-control-lg"
                   , placeholder_ (TL.toStrict host)
                   , autofocus_
                   ]
            div_ [class_ "input-group-append"] $ do
              span_ [class_ "input-group-text"] "/json"
              input_ [ type_ "submit"
                     , class_ "btn btn-primary"
                     , value_ "Query"
                     ]
    div_ [id_ "result", class_ "row mt-3"] $ do
      div_ [class_ "col-6"] $
        div_ [class_ "jsonthing"] $
          pre_ [class_ "ohno prettyprint lang-js"] (toHtml jsonRes)
      div_ [class_ "col-6", id_ "map"] ""

main :: IO ()
main = do
  db <- openGeoDB "GeoLite2-City.mmdb"
  scotty 3000 $ do
    get "/" $ do
      req <- request
      xff <- header "x-forwarded-for"
      let visitorIp = fromMaybe (TL.pack . showSockAddr . remoteHost $ req) xff
      html . renderText $ index visitorIp ""
    get "/ip/:ip" $ do
      ip <- param "ip"
      let res = findGeoData db "en" (read ip)
      let jsonRes = case res of
            Left err -> prettyJson' (UIGeoResultError err)
            Right res -> prettyJson' (UIGeoResult res)
      html . renderText $ index (TL.pack ip) jsonRes
    get "/ip/:ip/json" $ do
      ip <- param "ip"
      let res = findGeoData db "en" (read ip)
      case res of
        Left err -> json (UIGeoResultError err)
        Right res -> json (UIGeoResult res)
    get "/ip/:ip/json/pretty" $ do
      ip <- param "ip"
      let res = findGeoData db "en" (read ip)
      case res of
        Left err -> prettyJson (UIGeoResultError err)
        Right res -> prettyJson (UIGeoResult res)
