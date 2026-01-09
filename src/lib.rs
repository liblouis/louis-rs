/*! A library to translate text to braille and back

This library is a rust-only implementation of [liblouis](https://liblouis.io).

## Status

The API of this library is not stable yet. Breaking changes may occur
in any version.

## Usage

```no_run
use std::path::Path;

use louis::parser;
use louis::translator::TranslationPipeline;
use louis::Direction;

# fn main() -> Result<(), Box<dyn std::error::Error>> {
let file = Path::new("en-us-g1.ctb");
let rules = parser::table_expanded(file).expect("Failed to parse table");
let pipeline = TranslationPipeline::compile(&rules, Direction::Forward)?;
let braille = pipeline.translate("hello world");
assert_eq!(braille, "⠓⠑⠇⠇⠕⠀⠺⠕⠗⠇⠙");
# Ok(())
# }
```

*/

pub mod parser;
pub mod translator;

pub use parser::Direction;
pub use translator::TranslationPipeline;
