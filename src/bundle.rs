//! Serialization of parsed, include-expanded [`AnchoredRule`]s into a compact
//! distributable format: bincode-encoded rules, gzip-compressed.
//!
//! This is the format agreed on in <https://github.com/liblouis/louis-rs/pull/5>: the
//! `TranslationPipeline` itself can't be serialized (`hyphenation::Standard` has no serde
//! support), so instead we bundle the parsed rules and recompile the pipeline from them on load.

use std::{
    fs,
    io::{self, Read, Write},
    path::Path,
};

use flate2::{Compression, read::GzDecoder, write::GzEncoder};

use crate::parser::AnchoredRule;

/// An error that occurred while serializing, deserializing, or loading a table bundle.
#[derive(thiserror::Error, Debug)]
pub enum BundleError {
    #[error("could not read or write bundle")]
    Io(#[from] io::Error),
    #[error("could not encode or decode bundle")]
    Encoding(#[from] bincode::Error),
}

/// Serialize expanded rules to gzip-compressed bincode.
pub fn serialize_rules(rules: &[AnchoredRule]) -> Result<Vec<u8>, BundleError> {
    let encoded = bincode::serialize(rules)?;
    let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
    encoder.write_all(&encoded)?;
    Ok(encoder.finish()?)
}

/// Deserialize rules previously produced by [`serialize_rules`].
pub fn deserialize_rules(bytes: &[u8]) -> Result<Vec<AnchoredRule>, BundleError> {
    let mut decoder = GzDecoder::new(bytes);
    let mut decoded = Vec::new();
    decoder.read_to_end(&mut decoded)?;
    Ok(bincode::deserialize(&decoded)?)
}

/// Load rules from a bundle file on disk.
pub fn table_from_bundle(path: &Path) -> Result<Vec<AnchoredRule>, BundleError> {
    let bytes = fs::read(path)?;
    deserialize_rules(&bytes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{self};

    #[test]
    fn round_trips_a_table() {
        let table = "letter a 1\nletter b 12\n";
        let rules = parser::table(table, None).unwrap();
        let bytes = serialize_rules(&rules).unwrap();
        let decoded = deserialize_rules(&bytes).unwrap();
        assert_eq!(rules, decoded);
    }
}
