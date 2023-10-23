use std::rc::Rc;

use anyhow::Error;
use opcode::Opcode;
use parser::ast::{Expression, IntegerLiteral, Literal, Node, Statement, BooleanLiteral};

pub struct Bytecode {
    pub instructions: opcode::Instructions,
    pub constants: Vec<Rc<object::Object>>,
}

pub struct Compiler {
    instructions: opcode::Instructions,
    constants: Vec<Rc<object::Object>>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: opcode::Instructions::default(),
            constants: Vec::new(),
        }
    }

    fn add_constant(&mut self, obj: object::Object) -> usize {
        self.constants.push(obj.into());

        return (self.constants.len() - 1) as usize;
    }

    fn add_instructions(&mut self, instructions: opcode::Instructions) -> usize {
        let position = self.instructions.0.len();

        self.instructions.0.extend(instructions.0);

        return position;
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn emit(&mut self, op: opcode::Opcode, operands: Vec<usize>) -> usize {
        let instructions = opcode::make(op, &operands);

        self.add_instructions(instructions)
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

        return Ok(self.bytecode());
    }

    fn compile_statement(&mut self, s: &Statement) -> Result<(), Error> {
        match s {
            Statement::Return(r) => {
                self.compile_expression(&r.return_value)?;

                return Ok(());
            }
            Statement::Expr(e) => {
                self.compile_expression(e)?;

                self.emit(Opcode::OpPop, vec![]);
                
                return Ok(());
            }
            _ => {
                return Err(Error::msg("compile_statement: unimplemented"));
            }
        }
    }

    fn compile_expression(&mut self, e: &Expression) -> Result<(), Error> {
        match e {
            Expression::Infix(infix) => {
                self.compile_expression(&infix.left)?;
                self.compile_expression(&infix.right)?;

                match infix.operator.as_str() {
                    "+" => {
                        self.emit(opcode::Opcode::OpAdd, vec![]);

                        return Ok(());
                    }
                    "-" => {
                        self.emit(opcode::Opcode::OpSub, vec![]);

                        return Ok(());
                    }
                    "*" => {
                        self.emit(opcode::Opcode::OpMul, vec![]);

                        return Ok(());
                    }
                    "/" => {
                        self.emit(opcode::Opcode::OpDiv, vec![]);

                        return Ok(());
                    }
                    ">" => {
                        self.emit(opcode::Opcode::OpGreaterThan, vec![]);

                        return Ok(());
                    }
                    "==" => {
                        self.emit(opcode::Opcode::OpEqual, vec![]);

                        return Ok(());
                    }
                    "!=" => {
                        self.emit(opcode::Opcode::OpNotEqual, vec![]);

                        return Ok(());
                    }
                    _ => {
                        return Err(Error::msg("compile_expression: unimplemented"));
                    }
                }
            }
            Expression::Literal(literal) => match literal {
                Literal::Boolean(boolean) => match boolean {
                    BooleanLiteral { value: true, .. } => {
                        self.emit(opcode::Opcode::OpTrue, vec![]);

                        return Ok(());
                    }
                    BooleanLiteral { value: false, .. } => {
                        self.emit(opcode::Opcode::OpFalse, vec![]);

                        return Ok(());
                    }
                }
                Literal::Integer(IntegerLiteral { value, .. }) => {
                    let integer = object::Object::Integer(*value);

                    let constant = self.add_constant(integer);

                    self.emit(opcode::Opcode::OpConst, vec![constant]);

                    return Ok(());
                }
                _ => {
                    return Err(Error::msg("compile_expression: unimplemented"));
                }
            }
            _ => {
                return Err(Error::msg("compile_expression: unimplemented"));
            }
        }
    }
}
