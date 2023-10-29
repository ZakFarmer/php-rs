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
                scope: SymbolScope::Local,
                index: 0,
            },
        ),
        (
            "d".to_string(),
            Symbol {
                name: "d".to_string(),
                scope: SymbolScope::Local,
                index: 1,
            },
        ),
        (
            "e".to_string(),
            Symbol {
                name: "e".to_string(),
                scope: SymbolScope::Local,
                index: 0,
            },
        ),
        (
            "f".to_string(),
            Symbol {
                name: "f".to_string(),
                scope: SymbolScope::Local,
                index: 1,
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

    let mut first_local = SymbolTable::new_enclosed(global.clone());

    let c = first_local.define("c");

    if c.name != "c" {
        panic!("c.name not 'c'. got={}", c.name);
    }

    let d = first_local.define("d");

    if d.name != "d" {
        panic!("d.name not 'd'. got={}", d.name);
    }

    let mut second_local = SymbolTable::new_enclosed(first_local.clone());

    let e = second_local.define("e");

    if e.name != "e" {
        panic!("e.name not 'e'. got={}", e.name);
    }

    let f = second_local.define("f");

    if f.name != "f" {
        panic!("f.name not 'f'. got={}", f.name);
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
