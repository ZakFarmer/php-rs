use std::collections::HashMap;

use anyhow::Error;
use compiler::symbol_table::{Symbol, SymbolScope, SymbolTable};

#[test]
fn test_define() -> Result<(), Error> {
    let _expected = HashMap::from([
        (
            "a".to_string(),
            Symbol {
                name: "a".to_string(),
                scope: SymbolScope::Global,
                index: 0,
            },
        ),
        (
            "b".to_string(),
            Symbol {
                name: "b".to_string(),
                scope: SymbolScope::Global,
                index: 1,
            },
        ),
        (
            "c".to_string(),
            Symbol {
                name: "c".to_string(),
                scope: SymbolScope::Global,
                index: 2,
            },
        ),
        (
            "d".to_string(),
            Symbol {
                name: "d".to_string(),
                scope: SymbolScope::Global,
                index: 3,
            },
        ),
    ]);

    let mut global = SymbolTable::new();

    let a = global.define("a");

    if a.name != "a" {
        panic!("a.name not 'a'. got={}", a.name);
    }

    let b = global.define("b");

    if b.name != "b" {
        panic!("b.name not 'b'. got={}", b.name);
    }

    Ok(())
}

#[test]
fn test_resolve_global() -> Result<(), Error> {
    let mut global = SymbolTable::new();

    global.define("a");
    global.define("b");

    let expected = vec![
        (
            "a".to_string(),
            Symbol {
                name: "a".to_string(),
                scope: SymbolScope::Global,
                index: 0,
            },
        ),
        (
            "b".to_string(),
            Symbol {
                name: "b".to_string(),
                scope: SymbolScope::Global,
                index: 1,
            },
        ),
    ];

    for (name, expected_symbol) in expected {
        let symbol = global.resolve(&name);

        if symbol.is_none() {
            panic!("symbol for {} is None", name);
        }

        let symbol = symbol.unwrap();

        if symbol.name != expected_symbol.name {
            panic!(
                "symbol.name not {}. got={}",
                expected_symbol.name, symbol.name
            );
        }
    }

    Ok(())
}
