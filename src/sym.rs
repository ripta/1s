use string_interner::{DefaultSymbol, StringInterner, Symbol};

#[derive(Clone, Debug)]
pub struct SymbolManager {
    interner: StringInterner,
}

const SYM_SINGLETON: &str = "singleton";
const SYM_FALSE: &str = "false";
const SYM_TRUE: &str = "true";

impl SymbolManager {
    pub fn new() -> SymbolManager {
        let mut interner = StringInterner::default();
        interner.get_or_intern_static(SYM_SINGLETON);
        interner.get_or_intern_static(SYM_FALSE);
        interner.get_or_intern_static(SYM_TRUE);

        return SymbolManager { interner };
    }

    pub fn get(&mut self, s: &str) -> DefaultSymbol {
        return self.interner.get_or_intern(s);
    }

    pub fn get_false(&mut self) -> DefaultSymbol {
        return self.interner.get_or_intern_static(SYM_FALSE);
    }

    pub fn get_singleton(&mut self) -> DefaultSymbol {
        return self.interner.get_or_intern_static(SYM_SINGLETON);
    }

    pub fn get_true(&mut self) -> DefaultSymbol {
        return self.interner.get_or_intern_static(SYM_TRUE);
    }
}
