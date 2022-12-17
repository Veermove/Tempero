use crate::{lex::{Operator, Token}, parse::Expression};


#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Tuple(Vec<Type>),
    String,
    Int,
    Float,
    Bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedExpression {
    Literal(Token, Type),
    Grouping(Box<Expression>, Type),
    Unary(Token, Box<Expression>, Type),
    Binary(Box<Expression>, Token, Box<Expression>, Type),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>, Type),
    Tuple(Vec<Expression>, Type)
}

pub fn parse_types(expr: Expression) -> Result<TypedExpression, String> {
    let exp_type = find_expression_type(&expr)?;
    return Ok(match expr {
        Expression::Literal(l) => TypedExpression::Literal(l, exp_type),
        Expression::Grouping(g) => TypedExpression::Grouping(g, exp_type),
        Expression::Unary(o, e) => TypedExpression::Unary(o, e, exp_type),
        Expression::Binary(el, o, er) => TypedExpression::Binary(el, o, er, exp_type),
        Expression::Conditional(p, et, ef) => TypedExpression::Conditional(p, et, ef, exp_type),
        Expression::Tuple(t) => TypedExpression::Tuple(t, exp_type)
    });
}

pub fn find_expression_type(expr: &Expression) -> Result<Type, String> {
    match expr {
        Expression::Literal(token) 
            => find_type_for_literal(token),
        Expression::Grouping(nested) 
            => find_expression_type(nested),
        Expression::Unary(operator, expression) => {
            let expected = find_expected_type_for_operator_token(operator)?;
            let actual = find_expression_type(expression)?;
            if expected.contains(&actual) { 
                Ok(actual) 
            } else { 
                Err(format!("Mismatched types - expected one of {:?} vecause of operator {:?}, but got {:?}.",
                    expected, operator, actual
                )) 
            } 
        },
        Expression::Binary(left, operator, right) => {
            let expected = find_expected_type_for_operator_token(operator)?;
            let left = find_expression_type(left)?;
            let right = find_expression_type(right)?;
            if expected.contains(&left) && expected.contains(&right) { 
                Ok(left) 
            } else { 
                Err(format!("Mismatched types - expected one of {:?} vecause of operator {:?}, but got {:?}.",
                    expected, operator, if expected.contains(&left) { right } else { left }
                )) 
            } 
        },
        Expression::Conditional(predicate, t, f) => {
            let p = find_expression_type(predicate)?;
            if  p!= Type::Bool {
                return Err(format!("Mismatched type - predicate in ?: operator must be of type Bool, not {:?}.", p)) 
            };
            let (if_true, if_false) = (find_expression_type(t)?, find_expression_type(f)?);
            if if_true == if_false {
                return Ok(if_true);
            } else {
                return Err(format!("Mismatched types - expressions in ?: operator must be of same type, <expr if true>: {:?}, <expr if false>: {:?}", if_true, if_false)) 
            }
        },
        Expression::Tuple(exprs) => {
            let mut types = Vec::new();
            for ex in exprs {
                types.push(find_expression_type(ex)?);
            }
            return Ok(Type::Tuple(types));
        },
    }
}

fn find_type_for_literal(token: &Token) -> Result<Type, String> {
    match token {
        Token::FloatLiteral(_) => Ok(Type::Float),
        Token::IntegerLiteral(_) => Ok(Type::Int),
        Token::BooleanLiteral(_) => Ok(Type::Bool),
        Token::StringLiteral(_) => Ok(Type::String),
        _ => Err("Can't find type for this token".to_owned())
    }
}

fn find_expected_type_for_operator_token(token: &Token) -> Result<Vec<Type>, String> {
    if let Token::Operator(operator) = token {
        use Type::*;
        match operator {
            Operator::Eq => Ok(vec![Float, Int, Bool, String, Tuple(vec![])]),
            Operator::Neq => Ok(vec![Float, Int, Bool, String, Tuple(vec![])]),
            Operator::Greater => Ok(vec![Bool]),
            Operator::GreaterEq => Ok(vec![Bool]),
            Operator::Lesser => Ok(vec![Bool]),
            Operator::LesserEq => Ok(vec![Bool]),
            Operator::Plus => Ok(vec![Int, Float]),
            Operator::Minus => Ok(vec![Int, Float]),
            Operator::Multip => Ok(vec![Int, Float]),
            Operator::Div => Ok(vec![Int, Float]),
            Operator::Semicolon => todo!(),
            Operator::Not => Ok(vec![Bool]),
            Operator::And => Ok(vec![Bool]),
            Operator::Or => Ok(vec![Bool]),
            Operator::BitAnd => Ok(vec![Int]),
            Operator::BitOr => Ok(vec![Int]),
            Operator::Shl => Ok(vec![Int]),
            Operator::Shr => Ok(vec![Int]),
            Operator::Modulo => Ok(vec![Int]),
            Operator::Assign => todo!(),
            Operator::AssingInc => todo!(),
            Operator::AssignDec => todo!(),
            Operator::AssignMul => todo!(),
            Operator::AssignDiv => todo!(),
            Operator::IfExpr => todo!(),
            Operator::ElseExpr => todo!(),
            Operator::ModStream => todo!(),
            Operator::OpBrace => todo!(),
            Operator::CloBrace => todo!(),
            Operator::OpParenth => todo!(),
            Operator::CloParenth => todo!(),
            Operator::OpArr => todo!(),
            Operator::CloArr => todo!(),
            Operator::Separator => todo!(),
        }
    } else {
        Err("Can't infer type for this token".to_owned())
    }
}

pub trait Typed {
    fn get_type(&self) -> Result<Type, String>;
}

impl Typed for Expression {
    fn get_type(&self) -> Result<Type, String> {
        find_expression_type(self)
    }
}
