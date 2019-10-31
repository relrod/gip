#!/usr/bin/env bash
set -e
set -x
wget https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.tar.gz
tar -xvf GeoLite2-City.tar.gz '*/GeoLite2-City.mmdb' --strip 1
rm -v GeoLite2-City.tar.gz
