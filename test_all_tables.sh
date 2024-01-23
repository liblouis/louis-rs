#!/usr/bin/bash

for file in ~/src/liblouis/tables/*.{ctb,utb,uti,tbl,cti,dis}; do LOUIS_TABLE_PATH=~/src/liblouis/tables ./target/debug/louis parse $file > /dev/null; done
