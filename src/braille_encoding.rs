//! Braille encodings: which characters stand for the braille cells.

use std::path::{Path, PathBuf};

use search_path::SearchPath;

use crate::{
    Direction, Error, TableError,
    parser::{self, AnchoredRule, Rule},
    translator::DisplayTable,
};

/// Which characters stand for the braille cells, such as ASCII braille or a national
/// encoding. Without one, braille is read and written as Unicode braille (U+2800 to U+28FF).
///
/// # Coming from liblouis
///
/// A braille encoding is what liblouis calls a display table, and it is loaded from one: a
/// `.dis` file of `display` rules, such as `en-us-brf.dis` for ASCII braille (BRF). liblouis
/// takes the display table from the start of the table list, or from a table that includes
/// one. louis-rs ignores display tables in a table list instead, and takes the encoding
/// explicitly: it is loaded on its own and given to a [`Translator`](crate::Translator) with
/// [`with_braille_encoding`](crate::Translator::with_braille_encoding). One encoding can serve
/// translators in both directions.
#[derive(Debug, Clone)]
pub struct BrailleEncoding(Vec<AnchoredRule>);

impl BrailleEncoding {
    /// Load the encoding from the display table `table`, looked up in the `LOUIS_TABLE_PATH`
    /// search path.
    ///
    /// # Errors
    ///
    /// [`Error::Table`] if the table or one it includes cannot be found, read or parsed.
    pub fn new(table: impl AsRef<Path>) -> Result<Self, Error> {
        Self::load(table.as_ref(), &SearchPath::new_or("LOUIS_TABLE_PATH", "."))
    }

    /// Like [`Self::new`], but looking the table up in `search_path` instead.
    ///
    /// # Errors
    ///
    /// As for [`Self::new`].
    pub fn with_search_path(
        table: impl AsRef<Path>,
        search_path: impl IntoIterator<Item = impl Into<PathBuf>>,
    ) -> Result<Self, Error> {
        let search_path: Vec<PathBuf> = search_path.into_iter().map(Into::into).collect();
        Self::load(table.as_ref(), &SearchPath::from(search_path))
    }

    /// The encoding of display table source held in memory, which cannot include other
    /// tables.
    ///
    /// # Errors
    ///
    /// [`Error::Table`] if the source cannot be parsed or contains an `include`.
    pub fn from_table_source(table: &str) -> Result<Self, Error> {
        let rules = parser::table(table, None).map_err(Error::Table)?;
        if rules
            .iter()
            .any(|rule| matches!(rule.rule, Rule::Include { .. }))
        {
            return Err(Error::Table(vec![TableError::IncludesNotSupportedInMemory]));
        }
        Ok(Self(rules))
    }

    fn load(table: &Path, search_path: &SearchPath) -> Result<Self, Error> {
        let rules = parser::table_expanded_in(table, search_path).map_err(Error::Table)?;
        Ok(Self(rules))
    }

    /// The encoding's `display` rules, compiled for translating in `direction`
    pub(crate) fn compile(&self, direction: Direction) -> DisplayTable {
        DisplayTable::compile(&self.0, direction)
    }
}
