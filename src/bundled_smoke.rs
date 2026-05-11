#[cfg(all(test, feature = "bundled-tables"))]
mod tests {
    use crate::{Direction, Translator};

    #[test]
    fn bundled_en_us_g1() {
        let t = Translator::new_bundled(&["en-us-g1.ctb"], Direction::Forward).unwrap();
        assert_eq!(t.translate("hello world").unwrap(), "⠓⠑⠇⠇⠕⠀⠺⠕⠗⠇⠙");
    }
}
