#!/usr/bin/bash

for file in ~/src/liblouis/tables/*.{ctb,utb,uti,tbl,cti,dis}; do ./target/debug/louis-parser-handrolled $file > /dev/null; done
