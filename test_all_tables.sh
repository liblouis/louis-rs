#!/usr/bin/bash

for file in ~/src/liblouis/tables/*.{ctb,utb,uti,tbl,cti,dis}; do LOUIS_TABLE_PATH=/home/eglic/src/liblouis/tables ./target/debug/louis-parser-handrolled $file > /dev/null; done
