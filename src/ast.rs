use crate::token::Token;

pub enum Node {
    Expression(Expression),
    Program(Program),
    Statement(Statement),
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Node::Expression(expression) => write!(f, "{}", expression),
            Node::Program(program) => write!(f, "{}", program),
            Node::Statement(statement) => write!(f, "{}", statement),
        }
    }
}

pub enum Literal {
    Integer(Integer),
    Boolean(Boolean),
    String(StringLiteral)
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Literal::Integer(Integer { token, value }) => write!(f, "{}", value),
            Literal::Boolean(Boolean { token, value }) => write!(f, "{}", value),
            Literal::String(StringLiteral { token, value }) => write!(f, "{}", value),
        }
    }
}

pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Infix(InfixExpression),
    Prefix(PrefixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Identifier(identifier) => write!(f, "{}", identifier),
            Expression::Literal(literal) => write!(f, "{}", literal),
            Expression::Infix(InfixExpression {
                token,
                left,
                operator,
                right,
            }) => write!(f, "({} {} {})", left, operator, right),
            Expression::Prefix(PrefixExpression {
                token,
                operator,
                right,
            }) => write!(f, "({}{})", operator, right),
            Expression::If(IfExpression {
                token,
                condition,
                consequence,
                alternative,
            }) => {
                if let Some(alternative) = alternative {
                    write!(f, "if {} {{\n{}\n}} else {{\n{}\n}}", condition, consequence, alternative)
                } else {
                    write!(f, "if {} {{\n{}\n}}", condition, consequence)
                }
            },
            Expression::Function(FunctionLiteral {
                token,
                parameters,
                body,
            }) => {
                let params = parameters.iter().map(|p| p.to_string()).collect::<Vec<String>>();

                write!(f, "fn({}) {{\n{}\n}}", params.join(", "), body)
            },
            Expression::Call(CallExpression {
                token,
                function,
                arguments,
            }) => {
                let mut arguments_string = String::new();

                for (index, argument) in arguments.iter().enumerate() {
                    arguments_string.push_str(&format!("{}", argument));

                    if index < arguments.len() - 1 {
                        arguments_string.push_str(", ");
                    }
                }

                write!(f, "{}({})", function, arguments_string)
            }
        }
    }
}

pub enum Statement {
    Assign(Assignment),
    Expr(Expression),
    Return(ReturnStatement),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Statement::Assign(Assignment {
                token,
                name,
                value,
            }) => write!(f, "{} {} = {}", token, name, value),
            Statement::Expr(expression) => write!(f, "{}", expression),
            Statement::Return(ReturnStatement {
                token,
                return_value,
            }) => write!(f, "{} {}", token, return_value),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Default for Program {
    fn default() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut program_string = String::new();

        for statement in &self.statements {
            program_string.push_str(&format!("{}", statement));
        }

        write!(f, "{}", program_string)
    }
}

// LITERALS
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

pub struct Integer {
    pub token: Token,
    pub value: i64,
}

pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

// EXPRESSIONS
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

// STATEMENTS
pub struct Assignment {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl std::fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut block_string = String::new();

        for statement in &self.statements {
            block_string.push_str(&format!("{}\n", statement));
        }

        write!(f, "{}", block_string)
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}