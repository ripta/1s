use string_interner::{DefaultSymbol, StringInterner};

#[derive(Clone, Debug)]
pub struct SymbolManager {
    interner: StringInterner,
}

impl SymbolManager {
    pub fn new() -> SymbolManager {
        return SymbolManager {
            interner: StringInterner::default(),
        };
    }

    pub fn get(&mut self, s: &str) -> DefaultSymbol {
        return self.interner.get_or_intern(s);
    }
}
