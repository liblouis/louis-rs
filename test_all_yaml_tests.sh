#!/usr/bin/bash

for file in ~/src/liblouis/tests/yaml/*.yaml; do LOUIS_TABLE_PATH=~/src/liblouis/tables ./target/debug/louis-parser-handrolled check $file > /dev/null || echo $file; done
for file in ~/src/liblouis/tests/braille-specs/*.yaml; do LOUIS_TABLE_PATH=~/src/liblouis/tables ./target/debug/louis-parser-handrolled check $file > /dev/null || echo $file; done
