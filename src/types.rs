use std::{fmt::Display, collections::HashMap};

use crate::typecheck::find_expression_type;

pub type BindingRequests = HashMap<String, Type>;
pub type EnumeratedTokens = Vec<(Token, usize, usize)>;

#[derive(Debug, PartialEq, Clone)]
pub struct TExpression {
    exprssion_type: Option<Result<Type, String>>,
    pub expression: Expression,
}


impl TExpression {
    pub fn new(expr: Expression) -> Self {
        TExpression {
            exprssion_type: None,
            expression: expr
        }
    }

    pub fn get_type(&mut self, state: &BindingRequests) -> Result<Type, String> {
        if self.exprssion_type.is_none() {
            self.exprssion_type = Some(find_expression_type(&self.expression, &state))
        }
        self.exprssion_type.clone().unwrap()
    }
}


#[derive(Debug, Clone)]
pub enum Statement {
    Expr(TExpression),
    Print(TExpression),
    Assignment(String, TExpression),
    Block(Vec<Result<Statement, String>>, BindingRequests),
    If(
        TExpression,                             // if <expr>
        Result<Box<Statement>, String>,         // <stmt>
        Option<Result<Box<Statement>, String>>  // (else <stmt>)
    ),
    // While(Expression, Box<Statement>),
}

pub struct Program {
    pub instrucrions: Vec<Result<Statement, String>>,
    pub binds: BindingRequests,
}

impl Program {
    pub fn new(
        instrucrions: Vec<Result<Statement, String>>,
        binds: BindingRequests,
    ) -> Self {
        Program { instrucrions, binds }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    Grouping(Box<Expression>),
    Unary(Operator, Box<Expression>),
    Binary(Box<Expression>, Operator, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Tuple(Vec<Expression>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Literal(Literal),
    Identifier(String),
    Keyword(Keyword),
    Operator(Operator),

    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Float(f64),
    Integer(i64),
    Boolean(bool),
    String(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {

// TODO: Missing Operators:
//  *|

    // takse l-expressions, and supplies it as first argument
    // <expression> *| <function call>

    Eq,         // ==
    Neq,        // !=
    Greater,    // >
    GreaterEq,  // >=
    Lesser,     // <
    LesserEq,   // <=
    Plus,       // +
    Minus,      // -
    Multip,     // *
    Div,        // /
    Semicolon,  // ;
    Not,        // !
    And,        // &&
    Or,         // ||
    BitAnd,     // &
    BitOr,      // |
    Shl,        // <<
    Shr,        // >>
    Modulo,     // %

    Assign,     //  =
    AssingInc,  // +=
    AssignDec,  // -=
    AssignMul,  // *=
    AssignDiv,  // /=

                // condition
    IfExpr,     // ? <expr if true>
    ElseExpr,   // : <expr if false>

    ModStream,  // ::

    OpBrace,    // {
    CloBrace,   // }
    OpParenth,  // (
    CloParenth, // )
    OpArr,      // [
    CloArr,     // ]

    Separator   // ,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Let,
    For,
    If,
    Else ,
    Func,
}

#[derive(Debug, PartialEq)]
pub struct TokenInfo {
    pub token: Token,
    pub column: usize,
}

#[derive(Debug, PartialEq)]
pub struct LineTokens {
    pub number: usize,
    pub value: String,
    pub tokens: Vec<TokenInfo>
}


impl TokenInfo {
    pub fn new(token: Token, column: usize) -> Self {
        TokenInfo { token, column }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Tuple(Vec<Type>),
    String,
    Int,
    Float,
    Bool,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self{
            Type::Tuple(vs) => write!(f, "Tuple ({})", vs.iter().map(|s| format!("{}", s)).collect::<Vec<String>>().join(", ")),
            Type::String => write!(f, "String"),
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
        }
    }
}


impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(token) => write!(f, "{}", token),
            Expression::Grouping(e) => write!(f, "( {} )", e),
            Expression::Unary(op, exp) => write!(f, "{}( {} )", op, exp),
            Expression::Binary(l, op, r) => write!(f, "{} {} {}", l, op, r),
            Expression::Conditional(c, l, r) => write!(f, "{} ? {} : {} ", c, l, r),
            Expression::Variable(ident) => write!(f, "{}", ident),
            Expression::Tuple(_) => unimplemented!(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Literal(lt) => write!(f, "{}", lt),
            Token::Identifier(id) => write!(f, "{}", id),
            Token::Keyword(s) => write!(f, "{:?}", s),
            Token::Operator(op) => write!(f, "{}", op),
            Token::Eof => write!(f, " [EOF]"),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Eq => write!(f, "=="),
            Operator::Neq => write!(f, "!="),
            Operator::Greater => write!(f, ">"),
            Operator::GreaterEq => write!(f, ">="),
            Operator::Lesser => write!(f, "<"),
            Operator::LesserEq => write!(f, "<="),
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Multip => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Semicolon => write!(f, ";"),
            Operator::Not => write!(f, "!"),
            Operator::And => write!(f, "&&"),
            Operator::Or => write!(f, "||"),
            Operator::BitAnd => write!(f, "&"),
            Operator::BitOr => write!(f, "|"),
            Operator::Shl => write!(f, "<<"),
            Operator::Shr => write!(f, ">>"),
            Operator::Modulo => write!(f, "%"),
            Operator::Assign => write!(f, "="),
            Operator::AssingInc => write!(f, "+="),
            Operator::AssignDec => write!(f, "-="),
            Operator::AssignMul => write!(f, "*="),
            Operator::AssignDiv => write!(f, "/="),
            Operator::IfExpr => write!(f, "?"),
            Operator::ElseExpr => write!(f, ":"),
            Operator::ModStream => write!(f, "::"),
            Operator::OpBrace => write!(f, "{{"),
            Operator::CloBrace => write!(f, "}}"),
            Operator::OpParenth => write!(f, "("),
            Operator::CloParenth => write!(f, ")"),
            Operator::OpArr => write!(f, "["),
            Operator::CloArr => write!(f, "]"),
            Operator::Separator => write!(f, ",")
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Float(v) => write!(f, "{}", v),
            Literal::Integer(v) => write!(f, "{}", v),
            Literal::Boolean(v) => write!(f, "{}", v),
            Literal::String(v) => write!(f, "{}", v),
        }
    }
}
