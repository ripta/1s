use std::collections::{HashMap, HashSet};
use string_interner::{DefaultSymbol, StringInterner, Symbol};

#[derive(Clone, Debug)]
pub struct SymbolManager {
    interner: StringInterner,
    props: HashMap<DefaultSymbol, HashMap<DefaultSymbol, DefaultSymbol>>,
    attrs: HashMap<DefaultSymbol, HashSet<DefaultSymbol>>,
}

const SYM_PROP: &str = "prop";
const SYM_ATTR: &str = "attr";
const SYM_STATIC: &str = "static";

const SYM_CLASS: &str = "class";
const SYM_SINGLETON: &str = "singleton";
const SYM_MULTITON: &str = "multiton";
const SYM_INSTANCE_OF: &str = "instance_of";

const SYM_BOOL: &str = "bool";
const SYM_FALSE: &str = "false";
const SYM_TRUE: &str = "true";

impl SymbolManager {
    pub fn new() -> SymbolManager {
        let mut interner = StringInterner::default();

        let mut props = HashMap::new();
        let mut attrs = HashMap::new();

        let s_attr = interner.get_or_intern_static(SYM_ATTR);
        attrs.insert(s_attr, HashSet::from([s_attr]));

        let s_prop = interner.get_or_intern_static(SYM_PROP);
        attrs.insert(s_prop, HashSet::from([s_attr]));

        let s_static = interner.get_or_intern_static(SYM_STATIC);
        attrs.insert(s_static, HashSet::from([s_attr, s_static]));

        let s_iof = interner.get_or_intern_static(SYM_INSTANCE_OF);
        attrs.insert(s_iof, HashSet::from([s_prop]));

        let s_singleton = interner.get_or_intern_static(SYM_SINGLETON);
        attrs.insert(s_singleton, HashSet::from([s_attr]));

        let s_multiton = interner.get_or_intern_static(SYM_MULTITON);
        attrs.insert(s_multiton, HashSet::from([s_attr]));

        let s_class = interner.get_or_intern_static(SYM_CLASS);
        props.insert(s_class, HashMap::from([(s_iof, s_class)]));

        let s_bool = interner.get_or_intern_static(SYM_BOOL);
        props.insert(s_bool, HashMap::from([(s_iof, s_class)]));
        attrs.insert(s_bool, HashSet::from([s_static]));

        let s_false = interner.get_or_intern_static(SYM_FALSE);
        props.insert(s_false, HashMap::from([(s_iof, s_bool)]));
        attrs.insert(s_false, HashSet::from([s_multiton, s_static]));

        let s_true = interner.get_or_intern_static(SYM_TRUE);
        props.insert(s_true, HashMap::from([(s_iof, s_bool)]));
        attrs.insert(s_true, HashSet::from([s_multiton, s_static]));

        return SymbolManager { interner, props, attrs };
    }

    pub fn get(&mut self, s: &str) -> DefaultSymbol {
        return self.interner.get_or_intern(s);
    }

    pub fn get_bool(&mut self, b: bool) -> DefaultSymbol {
        if b {
            return self.get_true();
        }
        return self.get_false();
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

    pub fn find(&mut self, sym: DefaultSymbol) -> Option<&str> {
        return self.interner.resolve(sym);
    }

    pub fn attribute(&mut self, sym: &DefaultSymbol, attr: &DefaultSymbol) -> bool {
        return match self.attrs.get(sym) {
            None => false,
            Some(ats) => ats.contains(attr),
        };
    }

    pub fn property(&mut self, sym: &DefaultSymbol, key: &DefaultSymbol) -> Option<&DefaultSymbol> {
        return self.props.get(sym)?.get(key);
    }
}
