use compiler::Compiler;
use lexer::Lexer;
use parser::{Parser, ast::Node};
use vm::Vm;
use wasm_bindgen::{prelude::wasm_bindgen, JsValue};
use web_sys::console;

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    let input = "$a = 1; $b = 2; $c = $a + $b; $c;";

    console::log_1(&JsValue::from_str(&format!("Input: {}", input)));

    let result = parse(input);

    console::log_1(&JsValue::from_str(&format!("Result: {}", result)));

    Ok(())
}

#[wasm_bindgen]
pub fn compile(input: &str) -> String {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program().unwrap();
    parser.check_errors().unwrap();

    let mut compiler = Compiler::new();

    let bytecode = compiler.compile(&Node::Program(program)).unwrap();

    format!("{:?}", bytecode)
}

#[wasm_bindgen]
pub fn parse(input: &str) -> String {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program().unwrap();
    parser.check_errors().unwrap();

    let mut compiler = Compiler::new();

    let bytecode = compiler.compile(&Node::Program(program)).unwrap();

    let mut vm = Vm::new(bytecode);

    match vm.run() {
        Ok(_) => {
            let last_popped = vm.last_popped_stack_elem();
            last_popped.to_string()
        }
        Err(err) => {
            format!("Error: {}", err)
        }
    }
}