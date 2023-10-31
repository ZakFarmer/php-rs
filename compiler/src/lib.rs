use std::rc::Rc;

use anyhow::Error;
use opcode::{Instructions, Opcode};
use parser::ast::{BlockStatement, Expression, Literal, Node, Statement};
use symbol_table::{Symbol, SymbolScope, SymbolTable};
use token::{Token, TokenType};

pub mod codegen;
pub mod jit;
pub mod symbol_table;

#[derive(Clone, PartialEq)]
pub struct Bytecode {
    pub instructions: opcode::Instructions,
    pub constants: Vec<Rc<object::Object>>,
}

impl std::fmt::Debug for Bytecode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut bytecode_string = String::new();

        for (i, instruction) in self.instructions.0.iter().enumerate() {
            let op = Opcode::from(*instruction);

            bytecode_string.push_str(&format!("{:04} {}\n", i, op));
        }

        write!(f, "{}", bytecode_string)
    }
}

#[derive(Clone, Debug)]
pub struct CompilationScope {
    pub instructions: opcode::Instructions,
    pub last_instruction: EmittedInstruction,
    pub previous_instruction: EmittedInstruction,
}

pub struct Compiler {
    pub constants: Vec<Rc<object::Object>>,
    pub symbol_table: SymbolTable,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope {
            instructions: opcode::Instructions::default(),
            last_instruction: EmittedInstruction {
                opcode: opcode::Opcode::OpNull,
                position: 0,
            },
            previous_instruction: EmittedInstruction {
                opcode: opcode::Opcode::OpNull,
                position: 0,
            },
        };

        let mut symbol_table = SymbolTable::new();

        for (i, builtin) in object::builtins::BUILTINS.iter().enumerate() {
            symbol_table.define_builtin(i, builtin.0.to_string());
        }

        Self {
            constants: Vec::new(),
            symbol_table,
            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    pub fn new_with_state(
        constants: Vec<Rc<object::Object>>,
        mut symbol_table: SymbolTable,
    ) -> Self {
        for (i, builtin) in object::builtins::BUILTINS.iter().enumerate() {
            symbol_table.define_builtin(i, builtin.0.to_string());
        }

        Self {
            constants,
            symbol_table,
            ..Self::new()
        }
    }

    pub fn enter_scope(&mut self) {
        let scope = CompilationScope {
            instructions: opcode::Instructions::default(),
            last_instruction: EmittedInstruction {
                opcode: Opcode::OpNull,
                position: 0,
            },
            previous_instruction: EmittedInstruction {
                opcode: Opcode::OpNull,
                position: 0,
            },
        };

        self.scopes.push(scope);
        self.scope_index += 1;
        self.symbol_table = SymbolTable::new_enclosed(self.symbol_table.clone());
    }

    pub fn exit_scope(&mut self) -> opcode::Instructions {
        let instructions = self.current_instructions().clone();

        self.scopes.pop();
        self.scope_index -= 1;

        let outer = self.symbol_table.outer.as_ref().unwrap().as_ref().clone();

        self.symbol_table = outer;

        instructions
    }

    fn load_symbol(&mut self, symbol: &Symbol) {
        match symbol.scope {
            SymbolScope::Global => {
                self.emit(Opcode::OpGetGlobal, vec![symbol.index]);
            }
            SymbolScope::Local => {
                self.emit(Opcode::OpGetLocal, vec![symbol.index]);
            }
            SymbolScope::Builtin => {
                self.emit(Opcode::OpGetBuiltin, vec![symbol.index]);
            }
            SymbolScope::Free => {
                self.emit(Opcode::OpGetFree, vec![symbol.index]);
            }
            SymbolScope::Function => {
                self.emit(Opcode::OpCurrentClosure, vec![]);
            }
            _ => {}
        }
    }

    pub fn scopes(&self) -> &Vec<CompilationScope> {
        &self.scopes
    }

    pub fn scope_index(&self) -> usize {
        self.scope_index
    }

    fn add_constant(&mut self, obj: object::Object) -> usize {
        self.constants.push(obj.into());

        (self.constants.len() - 1) as usize
    }

    fn change_operand(&mut self, position: usize, operand: usize) {
        let op = Opcode::from(self.current_instructions().0[position]);

        let new_instruction = opcode::make(op, &vec![operand]);

        self.replace_instruction(position, new_instruction);
    }

    pub fn add_instructions(&mut self, instructions: &Instructions) -> usize {
        let position = self.current_instructions().0.len();

        let new_instruction = self.scopes[self.scope_index]
            .instructions
            .merge_instructions(instructions);

        self.scopes[self.scope_index].instructions = new_instruction;
        position
    }

    pub fn current_instructions(&self) -> &opcode::Instructions {
        &self.scopes[self.scope_index].instructions
    }

    fn replace_instruction(&mut self, position: usize, new_instruction: opcode::Instructions) {
        let instructions = &mut self.scopes[self.scope_index].instructions;

        for (i, instruction) in new_instruction.0.iter().enumerate() {
            instructions.0[position + i] = *instruction;
        }
    }

    fn set_last_instruction(&mut self, op: opcode::Opcode, position: usize) -> Result<(), Error> {
        let previous = self.scopes[self.scope_index].last_instruction.clone();
        let last = EmittedInstruction {
            opcode: op,
            position,
        };

        self.scopes[self.scope_index].last_instruction = last;
        self.scopes[self.scope_index].previous_instruction = previous;

        Ok(())
    }

    fn replace_last_pop_with_return(&mut self) {
        let last_position = self.scopes[self.scope_index].last_instruction.position;
        self.replace_instruction(last_position, opcode::make(Opcode::OpReturnValue, &vec![]));

        self.scopes[self.scope_index].last_instruction.opcode = Opcode::OpReturnValue;
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions().clone(),
            constants: self.constants.clone(),
        }
    }

    pub fn emit(&mut self, op: opcode::Opcode, operands: Vec<usize>) -> usize {
        let instructions = opcode::make(op, &operands);

        let index = self.add_instructions(&instructions);

        _ = self.set_last_instruction(op, index);

        index
    }

    pub fn compile(&mut self, node: &Node) -> Result<Bytecode, Error> {
        match node {
            Node::Program(p) => {
                for statement in &p.statements {
                    self.compile_statement(statement)?;
                }
            }
            Node::Statement(s) => {
                self.compile_statement(s)?;
            }
            Node::Expression(e) => {
                self.compile_expression(e)?;
            }
        }

        Ok(self.bytecode())
    }

    fn compile_block_statement(&mut self, block: &BlockStatement) -> Result<(), Error> {
        for statement in block.statements.iter() {
            self.compile_statement(statement)?;
        }

        Ok(())
    }

    fn compile_statement(&mut self, s: &Statement) -> Result<(), Error> {
        match s {
            Statement::Assign(assignment) => {
                self.compile_expression(&assignment.value)?;

                let symbol = self.symbol_table.define(&assignment.token.value);

                self.emit(
                    if symbol.scope == SymbolScope::Global {
                        Opcode::OpSetGlobal
                    } else {
                        Opcode::OpSetLocal
                    },
                    vec![symbol.index],
                );

                Ok(())
            }
            Statement::Return(return_statement) => {
                self.compile_expression(&return_statement.return_value)?;

                self.emit(opcode::Opcode::OpReturnValue, vec![]);

                Ok(())
            }
            Statement::Expr(expression) => {
                self.compile_expression(expression)?;

                self.emit(Opcode::OpPop, vec![]);

                Ok(())
            }
            _ => Err(Error::msg("compile_statement: unimplemented")),
        }
    }

    fn compile_operands(
        &mut self,
        left: &Box<Expression>,
        right: &Box<Expression>,
        operator: Token,
    ) -> Result<(), Error> {
        match operator.token_type {
            TokenType::Lt => {
                self.compile_expression(right)?;
                self.compile_expression(left)?;
            }
            _ => {
                self.compile_expression(left)?;
                self.compile_expression(right)?;
            }
        }
        Ok(())
    }

    fn compile_expression(&mut self, e: &Expression) -> Result<(), Error> {
        match e {
            Expression::Identifier(identifier) => {
                let symbol = self.symbol_table.resolve(identifier.token.value.clone());

                match symbol {
                    Some(symbol) => {
                        self.load_symbol(&symbol);
                    }
                    None => {
                        return Err(Error::msg(format!(
                            "undefined variable: {}",
                            identifier.token.value
                        )));
                    }
                }

                Ok(())
            }
            Expression::Function(function_literal) => {
                self.enter_scope();

                for parameter in function_literal.parameters.iter() {
                    self.symbol_table.define(&parameter.token.value);
                }

                self.compile_block_statement(&function_literal.body)?;

                if self.last_instruction_is(Opcode::OpPop) {
                    self.replace_last_pop_with_return();
                }

                if !self.last_instruction_is(Opcode::OpReturnValue) {
                    self.emit(Opcode::OpReturn, vec![]);
                }

                let num_locals = self.symbol_table.num_definitions;
                let instructions = self.exit_scope();

                let compiled_function =
                    Rc::from(object::CompiledFunction::new(instructions, num_locals));

                let operands =
                    vec![self.add_constant(object::Object::CompiledFunction(compiled_function))];

                self.emit(Opcode::OpConst, operands);

                Ok(())
            }
            Expression::Call(call_expression) => {
                self.compile_expression(&call_expression.function)?;

                for argument in call_expression.arguments.iter() {
                    self.compile_expression(argument)?;
                }

                self.emit(Opcode::OpCall, vec![call_expression.arguments.len()]);

                Ok(())
            }
            Expression::If(if_expression) => {
                self.compile_expression(&if_expression.condition)?;

                // dummy value that will be overwritten later
                let jnt_position = self.emit(Opcode::OpJumpNotTruthy, vec![9999]);

                self.compile_block_statement(&if_expression.consequence)?;

                if self.last_instruction_is(Opcode::OpPop) {
                    self.remove_last_pop();
                }

                let j_position = self.emit(Opcode::OpJump, vec![9999]);
                let after_consequence_position = self.current_instructions().0.len();
                self.change_operand(jnt_position, after_consequence_position);

                if if_expression.alternative.is_none() {
                    self.emit(opcode::Opcode::OpNull, vec![]);
                } else {
                    self.compile_block_statement(if_expression.alternative.as_ref().unwrap())?;

                    if self.last_instruction_is(Opcode::OpPop) {
                        self.remove_last_pop();
                    }
                }

                let after_alternative_position = self.current_instructions().0.len();
                self.change_operand(j_position, after_alternative_position);

                Ok(())
            }
            Expression::Index(index_expression) => {
                self.compile_expression(&index_expression.left)?;
                self.compile_expression(&index_expression.index)?;

                self.emit(opcode::Opcode::OpIndex, vec![]);

                Ok(())
            }
            Expression::Infix(infix_expression) => {
                self.compile_operands(
                    &infix_expression.left,
                    &infix_expression.right,
                    infix_expression.operator.clone(),
                )?;

                match infix_expression.operator.token_type {
                    TokenType::Plus => self.emit(opcode::Opcode::OpAdd, vec![]),
                    TokenType::Minus => self.emit(opcode::Opcode::OpSub, vec![]),
                    TokenType::Asterisk => self.emit(opcode::Opcode::OpMul, vec![]),
                    TokenType::Slash => self.emit(opcode::Opcode::OpDiv, vec![]),
                    TokenType::Gt | TokenType::Lt => {
                        self.emit(opcode::Opcode::OpGreaterThan, vec![])
                    }
                    TokenType::Eq => self.emit(opcode::Opcode::OpEqual, vec![]),
                    TokenType::NotEq => self.emit(opcode::Opcode::OpNotEqual, vec![]),
                    _ => {
                        return Err(Error::msg(
                            "compile_expression: unimplemented infix operator",
                        ))
                    }
                };

                Ok(())
            }
            Expression::Prefix(prefix_expression) => {
                self.compile_expression(&prefix_expression.right)?;

                match prefix_expression.operator.token_type {
                    TokenType::Bang => self.emit(opcode::Opcode::OpBang, vec![]),
                    TokenType::Minus => self.emit(opcode::Opcode::OpMinus, vec![]),
                    _ => {
                        return Err(Error::msg(
                            "compile_expression: unimplemented prefix operator",
                        ))
                    }
                };

                Ok(())
            }
            Expression::Literal(literal_expression) => match literal_expression {
                Literal::Array(array) => {
                    for element in array.elements.iter() {
                        self.compile_expression(element)?;
                    }

                    self.emit(opcode::Opcode::OpArray, vec![array.elements.len()]);

                    Ok(())
                }
                Literal::Boolean(boolean) => {
                    if *boolean {
                        self.emit(opcode::Opcode::OpTrue, vec![]);

                        Ok(())
                    } else {
                        self.emit(opcode::Opcode::OpFalse, vec![]);

                        Ok(())
                    }
                }
                Literal::Integer(value) => {
                    let integer = object::Object::Integer(*value);

                    let constant = self.add_constant(integer);

                    self.emit(opcode::Opcode::OpConst, vec![constant]);

                    Ok(())
                }
                Literal::String(value) => {
                    let string = object::Object::String(value.clone());

                    let constant = self.add_constant(string);

                    self.emit(opcode::Opcode::OpConst, vec![constant]);

                    Ok(())
                }
                _ => {
                    return Err(Error::msg("compile_expression: unimplemented"));
                }
            },
            _ => {
                return Err(Error::msg("compile_expression: unimplemented"));
            }
        }
    }

    fn last_instruction_is(&self, op: Opcode) -> bool {
        if self.current_instructions().0.is_empty() {
            return false;
        }

        &self.scopes[self.scope_index].last_instruction.opcode == &op
    }

    fn remove_last_pop(&mut self) {
        let last = self.scopes[self.scope_index].last_instruction.clone();
        let previous = self.scopes[self.scope_index].previous_instruction.clone();

        let old_instructions = self.current_instructions().0.clone();
        let new_instructions = old_instructions[..last.position].to_vec();

        self.scopes[self.scope_index].instructions.0 = new_instructions;
        self.scopes[self.scope_index].last_instruction = previous;
    }
}

#[derive(Clone, Copy, Debug)]
pub struct EmittedInstruction {
    pub opcode: opcode::Opcode,
    pub position: usize,
}
