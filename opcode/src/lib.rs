use std::collections::HashMap;

use byteorder::{BigEndian, ByteOrder, WriteBytesExt};
use lazy_static::lazy_static;

pub fn lookup(opcode: Opcode) -> &'static OpcodeDefinition {
    DEFINITIONS.get(&opcode).unwrap()
}

pub fn make(op: Opcode, operands: &Vec<usize>) -> Instructions {
    let mut instructions = Vec::new();
    instructions.push(op as u8);
    let widths = &DEFINITIONS.get(&op).unwrap().operand_widths;

    for (o, w) in operands.into_iter().zip(widths) {
        match w {
            2 => {
                instructions.write_u16::<BigEndian>(*o as u16).unwrap();
            }
            1 => {
                instructions.write_u8(*o as u8).unwrap();
            }
            _ => {
                panic!("unsupported operand width {}", w)
            }
        }
    }

    return Instructions(instructions);
}

pub fn concat_instructions(expected: &Vec<Instructions>) -> Instructions {
    let mut out = Instructions(Vec::new());

    for instruction in expected {
        out = out.merge_instructions(instruction)
    }

    return out;
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Instructions(pub Vec<u8>);

impl std::fmt::Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut instructions_string = String::new();
        let mut i = 0;

        while i < self.0.len() {
            let op = self.0[i];
            let opcode = Opcode::from(op);
            let definition = lookup(self.0[i].into());

            let (operands, read) = read_operands(&definition, &self.0[i + 1..]);

            instructions_string.push_str(&format!(
                "{:04} {}\n",
                i,
                Self::format_instructions(&definition, &operands)
            ));

            i += 1 + read;
        }

        write!(f, "{}", instructions_string)
    }
}

impl Instructions {
    pub fn format_instructions(definition: &OpcodeDefinition, operands: &Vec<usize>) -> String {
        match definition.operand_widths.len() {
            2 => format!(
                "{} {} {}",
                definition.name,
                operands[0],
                operands[1]
            ),
            1 => format!("{} {}", definition.name, operands[0]),
            0 => format!("{}", definition.name),
            _ => unimplemented!(),
        }
    }

    pub fn merge_instructions(&self, other: &Instructions) -> Instructions {
        let ins = vec![self, other];

        return Instructions(ins.iter().fold(vec![], |sum, &i| {
            [sum.as_slice(), i.0.as_slice()].concat()
        }));
    }
}

pub struct OpcodeDefinition {
    pub name: &'static str,
    pub operand_widths: Vec<usize>,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub enum Opcode {
    /// 0x00 -  Push a constant onto the stack
    OpConst,
    /// 0x01 -  Add two integers
    OpAdd,
    /// 0x02 -  Pop the top element from the stack
    OpPop,
    /// 0x03 -  Subtract two integers
    OpSub,
    /// 0x04 -  Multiply two integers
    OpMul,
    /// 0x05 -  Divide two integers
    OpDiv,
    /// 0x06 -  Push true onto the stack
    OpTrue,
    /// 0x07 -  Push false onto the stack
    OpFalse,
    /// 0x08 -  Check if two integers are equal
    OpEqual,
    /// 0x09 -  Check if two integers are not equal
    OpNotEqual,
    /// 0x0A -  Check if the first integer is greater than the second integer
    OpGreaterThan,
    /// 0x0B -  Negate the integer
    OpMinus,
    /// 0x0C -  Negate the boolean
    OpBang,
    /// 0x0D -  Jump if the top element of the stack is not truthy
    OpJumpNotTruthy,
    /// 0x0E -  Jump to a specific position
    OpJump,
    /// 0x0F -  Push null onto the stack
    OpNull,
    /// 0x10 -  Get a global variable
    OpGetGlobal,
    /// 0x11 -  Set a global variable
    OpSetGlobal,
    /// 0x12 -  Create an array
    OpArray,
    /// 0x13 -  Create a hash
    OpHash,
    /// 0x14 -  Index into an array
    OpIndex,
    /// 0x15 -  Call a function
    OpCall,
    /// 0x16 -  Return a value from a function
    OpReturnValue,
    /// 0x17 -  Return from a function
    OpReturn,
    /// 0x18 -  Get a local variable
    OpGetLocal,
    /// 0x19 -  Set a local variable
    OpSetLocal,
    /// 0x1A -  Create a closure
    OpClosure,
    /// 0x1B -  Get a free variable
    OpGetFree,
    /// 0x1C -  Current closure
    OpCurrentClosure,
}

impl From<u8> for Opcode {
    fn from(opcode: u8) -> Self {
        match opcode {
            0x00 => Opcode::OpConst,
            0x01 => Opcode::OpAdd,
            0x02 => Opcode::OpPop,
            0x03 => Opcode::OpSub,
            0x04 => Opcode::OpMul,
            0x05 => Opcode::OpDiv,
            0x06 => Opcode::OpTrue,
            0x07 => Opcode::OpFalse,
            0x08 => Opcode::OpEqual,
            0x09 => Opcode::OpNotEqual,
            0x0A => Opcode::OpGreaterThan,
            0x0B => Opcode::OpMinus,
            0x0C => Opcode::OpBang,
            0x0D => Opcode::OpJumpNotTruthy,
            0x0E => Opcode::OpJump,
            0x0F => Opcode::OpNull,
            0x10 => Opcode::OpGetGlobal,
            0x11 => Opcode::OpSetGlobal,
            0x12 => Opcode::OpArray,
            0x13 => Opcode::OpHash,
            0x14 => Opcode::OpIndex,
            0x15 => Opcode::OpCall,
            0x16 => Opcode::OpReturnValue,
            0x17 => Opcode::OpReturn,
            0x18 => Opcode::OpGetLocal,
            0x19 => Opcode::OpSetLocal,
            0x1A => Opcode::OpClosure,
            0x1B => Opcode::OpGetFree,
            0x1C => Opcode::OpCurrentClosure,
            _ => panic!("Opcode not found: {}", opcode),
        }
    }
}

impl std::fmt::Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let definition = lookup(*self);

        write!(f, "{}", definition.name)
    }
}

pub fn read_operands(def: &OpcodeDefinition, ins: &[u8]) -> (Vec<usize>, usize) {
    let mut operands = Vec::with_capacity(def.operand_widths.len());
    let mut offset = 0;

    for w in &def.operand_widths {
        match w {
            2 => {
                operands.push(BigEndian::read_u16(&ins[offset..offset + 2]) as usize);
                offset = offset + 2;
            }
            1 => {
                operands.push(ins[offset] as usize);
                offset = offset + 1;
            }
            0 => {}
            _ => {
                panic!("unsupported operand width {} for read", w)
            }
        }
    }

    return (operands, offset);
}

lazy_static! {
    pub static ref DEFINITIONS: HashMap<Opcode, OpcodeDefinition> = {
        let mut definitions = HashMap::new();

        definitions.insert(
            Opcode::OpConst,
            OpcodeDefinition {
                name: "OpConst",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            Opcode::OpAdd,
            OpcodeDefinition {
                name: "OpAdd",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpPop,
            OpcodeDefinition {
                name: "OpPop",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpSub,
            OpcodeDefinition {
                name: "OpSub",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpMul,
            OpcodeDefinition {
                name: "OpMul",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpDiv,
            OpcodeDefinition {
                name: "OpDiv",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpTrue,
            OpcodeDefinition {
                name: "OpTrue",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpFalse,
            OpcodeDefinition {
                name: "OpFalse",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpEqual,
            OpcodeDefinition {
                name: "OpEqual",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpNotEqual,
            OpcodeDefinition {
                name: "OpNotEqual",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpGreaterThan,
            OpcodeDefinition {
                name: "OpGreaterThan",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpMinus,
            OpcodeDefinition {
                name: "OpMinus",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpBang,
            OpcodeDefinition {
                name: "OpBang",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpJumpNotTruthy,
            OpcodeDefinition {
                name: "OpJumpNotTruthy",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            Opcode::OpJump,
            OpcodeDefinition {
                name: "OpJump",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            Opcode::OpNull,
            OpcodeDefinition {
                name: "OpNull",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpGetGlobal,
            OpcodeDefinition {
                name: "OpGetGlobal",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            Opcode::OpSetGlobal,
            OpcodeDefinition {
                name: "OpSetGlobal",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            Opcode::OpArray,
            OpcodeDefinition {
                name: "OpArray",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            Opcode::OpHash,
            OpcodeDefinition {
                name: "OpHash",
                operand_widths: vec![2],
            },
        );
        definitions.insert(
            Opcode::OpIndex,
            OpcodeDefinition {
                name: "OpIndex",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpCall,
            OpcodeDefinition {
                name: "OpCall",
                operand_widths: vec![1],
            },
        );
        definitions.insert(
            Opcode::OpReturnValue,
            OpcodeDefinition {
                name: "OpReturnValue",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpReturn,
            OpcodeDefinition {
                name: "OpReturn",
                operand_widths: vec![],
            },
        );
        definitions.insert(
            Opcode::OpGetLocal,
            OpcodeDefinition {
                name: "OpGetLocal",
                operand_widths: vec![1],
            },
        );
        definitions.insert(
            Opcode::OpSetLocal,
            OpcodeDefinition {
                name: "OpSetLocal",
                operand_widths: vec![1],
            },
        );
        definitions.insert(
            Opcode::OpClosure,
            OpcodeDefinition {
                name: "OpClosure",
                operand_widths: vec![2, 1],
            },
        );
        definitions.insert(
            Opcode::OpGetFree,
            OpcodeDefinition {
                name: "OpGetFree",
                operand_widths: vec![1],
            },
        );
        definitions.insert(
            Opcode::OpCurrentClosure,
            OpcodeDefinition {
                name: "OpCurrentClosure",
                operand_widths: vec![],
            },
        );

        definitions
    };
}
