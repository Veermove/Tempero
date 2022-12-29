use crate::{lex::{Operator, Token, Literal}, exprparse::Expression};


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
        Ok(TExpression {
            exprssion_type: expr.get_type()?,
            expression: expr
        })
    }
}

pub fn find_expression_type(expression: &Expression) -> Result<Type, String> {
    fn find_exp_type_inner(expr: &Expression) -> Result<Type, String> {
        match expr {
            Expression::Literal(token)
                => find_type_for_literal(token),
            Expression::Grouping(nested)
                => find_exp_type_inner(nested),
            Expression::Unary(operator, expression) => {
                let actual = find_exp_type_inner(expression)?;
                let allowed_operators = find_un_operator_for_types(&actual);
                if allowed_operators.contains(operator) {
                    Ok(actual)
                } else {
                    Err(format!("{:?} is not supported for expression of type: {:?}.", operator, actual))
                }
            },
            Expression::Binary(left, operator, right) => {
                let actual_l = find_exp_type_inner(left)?;
                let actual_r = find_exp_type_inner(right)?;
                let allowed_operators = find_bin_operator_for_types(&actual_l, &actual_r);
                if allowed_operators.contains(operator) {
                    Ok(find_type_produced_by_bin_operator(operator, &actual_l, &actual_r))
                } else {
                    Err(format!("{:?} is not supported for types: {:?} [{:?}] {:?}.", operator, actual_l, operator, actual_r))
                }
            },
            Expression::Conditional(predicate, t, f) => {
                let p = find_exp_type_inner(predicate)?;
                if  p!= Type::Bool {
                    return Err(format!("Mismatched type - predicate in ?: operator must be of type Bool, not {:?}.", p))
                };
                let (if_true, if_false) = (find_exp_type_inner(t)?, find_exp_type_inner(f)?);
                if if_true == if_false {
                    return Ok(if_true);
                } else {
                    return Err(format!("Mismatched types - expressions in [?:] operator must be of same type, but was Bool ? {:?} : {:?}", if_true, if_false))
                }
            },
            Expression::Tuple(exprs) => {
                let mut types = Vec::new();
                for ex in exprs {
                    types.push(find_exp_type_inner(ex)?);
                }
                return Ok(Type::Tuple(types));
            },
        }
    }

    find_exp_type_inner(expression)
        .map_err(|msg| format!("Error in expression: {}. {}", expression, msg))
}

fn find_type_for_literal(token: &Token) -> Result<Type, String> {
    match token {
        Token::Literal(Literal::Float(_)) => Ok(Type::Float),
        Token::Literal(Literal::Integer(_)) => Ok(Type::Int),
        Token::Literal(Literal::Boolean(_)) => Ok(Type::Bool),
        Token::Literal(Literal::String(_)) => Ok(Type::String),
        _ => Err(format!("Can't find type for token: {:?}", token))
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

pub trait Typed {
    fn get_type(&self) -> Result<Type, String>;
}

impl Typed for Expression {
    fn get_type(&self) -> Result<Type, String> {
        find_expression_type(self)
    }
}
