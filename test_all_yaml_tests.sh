#!/usr/bin/bash

for file in ~/src/liblouis/tests/yaml/*.yaml; do ./target/debug/louis-parser-handrolled check $file > /dev/null || echo $file; done
for file in ~/src/liblouis/tests/braille-specs/*.yaml; do ./target/debug/louis-parser-handrolled check $file > /dev/null || echo $file; done
