
#[derive(Debug)]
pub enum Indication {
    Word,
    Capitalization,
    Number,
    Strong,
    Emphasis,
    Computer,
}

#[derive(Debug)]
pub struct IndicationContext {
    start: Indication,
    end: Indication,
}
