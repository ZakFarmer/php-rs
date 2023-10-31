use std::rc::Rc;

use compiler::{symbol_table::SymbolTable, Compiler};

use object::Object;
use parser::{ast::Node, Parser};
use vm::{Vm, GLOBALS_SIZE};
use wasm_bindgen::{prelude::wasm_bindgen, JsValue};

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

pub struct ExecutionState {
    pub constants: Vec<Rc<Object>>,
    pub globals: Vec<Rc<Object>>,
    pub symbol_table: SymbolTable,
}

impl ExecutionState {
    pub fn new() -> Self {
        Self {
            constants: vec![],
            globals: vec![Rc::new(Object::Null); GLOBALS_SIZE],
            symbol_table: SymbolTable::new(),
        }
    }
}

static mut CURRENT_EXECUTION_STATE: Option<ExecutionState> = None;

#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();

    Ok(())
}

#[wasm_bindgen]
pub fn init_state() {
    unsafe {
        CURRENT_EXECUTION_STATE = Some(ExecutionState::new());
    }
}

fn execute(input: &str, state: &mut ExecutionState) -> Result<String, String> {
    let mut parser = Parser::new(input);

    let program = parser
        .parse_program()
        .map_err(|e| format!("Error parsing: {}", e))?;
    parser
        .check_errors()
        .map_err(|e| format!("Parsing error: {}", e))?;

    let mut compiler =
        Compiler::new_with_state(state.constants.clone(), state.symbol_table.clone());
    let bytecode = compiler
        .compile(&Node::Program(program))
        .map_err(|e| format!("Compilation error: {}", e))?;

    let mut vm = Vm::new_with_globals_store(bytecode, state.globals.clone());
    vm.run().map_err(|e| format!("VM run error: {}", e))?;

    // Persist the updated state back to CURRENT_EXECUTION_STATE
    state.constants = compiler.constants;
    state.symbol_table = compiler.symbol_table;
    state.globals = vm.globals.clone();

    let last_popped = vm.last_popped_stack_elem();

    match last_popped.as_ref() {
        Object::Integer(i) => return Ok(i.to_string()),
        Object::Boolean(b) => return Ok(b.to_string()),
        Object::String(s) => return Ok(s.to_string()),
        Object::Null => return Ok("null".to_string()),
        _ => {}
    }

    Ok("".to_string())
}

#[wasm_bindgen]
pub fn parse(input: &str) -> String {
    let state = unsafe {
        CURRENT_EXECUTION_STATE
            .as_mut()
            .expect("State not initialized!")
    };

    match execute(input, state) {
        Ok(result) => result,
        Err(error) => error,
    }
}
