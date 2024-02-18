use crate::state;
use crate::state::State;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
#[derive(Debug, Clone)]
pub struct Driver {
    state: State,
    error_string: String,
}

fn set_panic_hook() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
impl Driver {
    pub fn new() -> Driver {
        set_panic_hook();

        let state = State::new();
        return Driver {
            state,
            error_string: "".to_string(),
        };
    }

    pub fn error_string(&self) -> String {
        return self.error_string.to_string();
    }

    pub fn has_error(&self) -> bool {
        return !self.error_string.is_empty();
    }

    pub fn run_string(&mut self, content: String, trace_exec: bool) -> bool {
        match state::run_string(self.state.clone(), content, trace_exec) {
            Ok(state) => {
                self.state = state;
                self.error_string = "".to_string();
                true
            }
            Err(e) => {
                self.error_string = format!("{:?}", e);
                false
            }
        }
    }

    pub fn render_stack(&self) -> String {
        let mut s = String::new();
        let _ = self.state.render_stack(&mut s);
        return s;
    }
}
