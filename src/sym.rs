use string_interner::{DefaultSymbol, StringInterner, Symbol};

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

    pub fn reserve(&mut self, s: &str) {
        self.interner.get_or_intern(s);
    }
}
