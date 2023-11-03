#![deny(elided_lifetimes_in_paths)]

use anyhow::Error;
use inkwell::{context::Context, types::IntType, OptimizationLevel};
use llvm::Llvm;

pub mod builder;
mod expression;
mod identifier;
mod statement;
pub mod symbol_table;
