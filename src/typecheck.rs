use crate::{lex::{Operator, Token, Literal}, parse::Expression};


#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Tuple(Vec<Type>),
    String,
    Int,
    Float,
    Bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TExpression {
    pub exprssion_type: Type,
    pub expression: Expression,
}

impl TExpression {
    pub fn new(expr: Expression) -> Result<Self, String> {
        Ok(TExpression { exprssion_type: find_expression_type(&expr)?, expression: expr })
    }
}

pub fn find_expression_type(expr: &Expression) -> Result<Type, String> {
    match expr {
        Expression::Literal(token)
            => find_type_for_literal(token),
        Expression::Grouping(nested)
            => find_expression_type(nested),
        Expression::Unary(operator, expression) => {
            let actual = find_expression_type(expression)?;
            let allowed_operators = find_un_operator_for_types(&actual);
            if let Token::Operator(op) = operator {
                if allowed_operators.contains(op) {
                    Ok(actual)
                } else {
                    Err(format!("{:?} is not supported for expression of type: {:?}.", operator, actual))
                }
            } else {
                Err(format!("{:?} is not a valid unary operator.", operator))
            }
        },
        Expression::Binary(left, operator, right) => {
            let actual_l = find_expression_type(left)?;
            let actual_r = find_expression_type(right)?;
            let allowed_operators = find_bin_operator_for_types(&actual_l, &actual_r);

            if let Token::Operator(op) = operator {
                if allowed_operators.contains(op) {
                    Ok(find_type_produced_by_bin_operator(op, &actual_l, &actual_r))
                } else {
                    Err(format!("{:?} is not supported for types: {:?} [{:?}] {:?}.", operator, actual_l, operator, actual_r))
                }
            } else {
                Err(format!("{:?} is not a valid binary operator.", operator))
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
                return Err(format!("Mismatched types - expressions in [?:] operator must be of same type, <expr if true>: {:?}, <expr if false>: {:?}", if_true, if_false))
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
        Token::Literal(Literal::Float(_)) => Ok(Type::Float),
        Token::Literal(Literal::Integer(_)) => Ok(Type::Int),
        Token::Literal(Literal::Boolean(_)) => Ok(Type::Bool),
        Token::Literal(Literal::String(_)) => Ok(Type::String),
        _ => Err("Can't find type for this token".to_owned())
    }
}

fn find_bin_operator_for_types(left: &Type, right: &Type) -> Vec<Operator> {
    use Operator::*;
    let mut allowed_operators = vec![Eq, Neq];
    if (left == &Type::Float || left == &Type::Int)
        && (right == &Type::Float || right == &Type::Int) {
        allowed_operators.append(&mut vec![Greater, GreaterEq, Lesser, LesserEq, Plus, Minus, Multip, Div]);
    }
    if left == &Type::String && right == &Type::String {
        if !allowed_operators.contains(&Operator::Plus) {
            allowed_operators.push(Operator::Plus)
        }
    }
    if left == &Type::Int && right == &Type::Int {
        allowed_operators.append(&mut vec![Shl, Shr, Modulo, BitAnd, BitOr]);
    }
    if left == &Type::Bool && right == &Type::Bool {
        allowed_operators.append(&mut vec![And, Or]);
    }
    allowed_operators
}

fn find_un_operator_for_types(exp: &Type) -> Vec<Operator> {
    use Type::*;
    match exp {
        Bool => vec![Operator::Not],
        Float | Int => vec![Operator::Minus],
        _ => vec![]
    }
}

fn find_type_produced_by_bin_operator(operator: &Operator, left: &Type, right: &Type) -> Type {
    use Type::*;
    use Operator::*;
    match operator {
        Eq => Bool,
        Neq => Bool,
        Greater => Bool,
        GreaterEq => Bool,
        Lesser => Bool,
        LesserEq => Bool,
        Plus => left.clone(),
        Minus => left.clone(),
        Multip => left.clone(),
        Div => left.clone(),
        Not => Bool,
        And => Bool,
        Or => Bool,
        BitAnd => Int,
        BitOr => Int,
        Shl => Int,
        Shr => Int,
        Modulo => Int,
        _ => unimplemented!()
    }
}

fn find_expected_type_for_operator_token(token: &Token) -> Result<(Vec<Type>, Vec<Type>), String> {
    // if let Token::Operator(operator) = token {
    //     use Type::*;
    //     match operator {
    //         Operator::Eq => Ok((vec![Bool, Float, Int, String, Tuple(vec![])], Bool)),
    //         Operator::Neq => Ok((vec![Float, Int, String, Tuple(vec![])], Bool)),
    //         Operator::Greater => Ok((vec![Float, Int, String], Bool)),
    //         Operator::GreaterEq => Ok((vec![Float, Int, String], Bool)),
    //         Operator::Lesser => Ok((vec![Float, Int, String], Bool)),
    //         Operator::LesserEq => Ok((vec![Float, Int, String], Bool)),
    //         Operator::Plus => Ok((vec![Int, Float], )),
    //         Operator::Minus => Ok((vec![Int, Float], )),
    //         Operator::Multip => Ok((vec![Int, Float], )),
    //         Operator::Div => Ok((vec![Int, Float], )),
    //         Operator::Semicolon => todo(),
    //         Operator::Not => Ok((vec![Bool], )),
    //         Operator::And => Ok((vec![Bool], )),
    //         Operator::Or => Ok((vec![Bool], )),
    //         Operator::BitAnd => Ok((vec![Int], )),
    //         Operator::BitOr => Ok((vec![Int], )),
    //         Operator::Shl => Ok((vec![Int], )),
    //         Operator::Shr => Ok((vec![Int], )),
    //         Operator::Modulo => Ok((vec![Int], )),
    //         Operator::Assign => todo!(),
    //         Operator::AssingInc => todo!(),
    //         Operator::AssignDec => todo!(),
    //         Operator::AssignMul => todo!(),
    //         Operator::AssignDiv => todo!(),
    //         Operator::IfExpr => todo!(),
    //         Operator::ElseExpr => todo!(),
    //         Operator::ModStream => todo!(),
    //         Operator::OpBrace => todo!(),
    //         Operator::CloBrace => todo!(),
    //         Operator::OpParenth => todo!(),
    //         Operator::CloParenth => todo!(),
    //         Operator::OpArr => todo!(),
    //         Operator::CloArr => todo!(),
    //         Operator::Separator => todo!(),
    //     }
    // } else {
    //     Err("Can't infer type for this token".to_owned())
    // }
    unimplemented!()
}

pub trait Typed {
    fn get_type(&self) -> Result<Type, String>;
}

impl Typed for Expression {
    fn get_type(&self) -> Result<Type, String> {
        find_expression_type(self)
    }
}
