use crate::{
    lex::{Literal, Token, Operator},
    parse::Expression,
    typecheck::find_expression_type
};

pub fn eval_exp(exp: &Expression) -> Result<Literal, String> {

    let res = {
        match exp {
            Expression::Literal(val) => match val {
                Token::Literal(Literal::Float(v)) => Ok(Literal::Float(*v)),
                Token::Literal(Literal::Integer(v)) => Ok(Literal::Integer(*v)),
                Token::Literal(Literal::Boolean(v)) => Ok(Literal::Boolean(*v)),
                Token::Literal(Literal::String(v)) => Ok(Literal::String(v.clone())),
                t => Err(format!("Mismatched token: {:?} is not a Literal as expected.", t)),
            },
            Expression::Grouping(exp) => eval_exp(exp),
            Expression::Unary(op, expr) => match op {
                Token::Operator(Operator::Not) => {
                    let exp = eval_exp(expr)?;
                    if let Literal::Boolean(v) = exp {
                        Ok(Literal::Boolean(!v))
                    } else {
                        Err(format!(
                            "Type mismatch for  '!' (unary operator), expected Boolean, but got expression of type: {:?}",
                            find_expression_type(expr)?
                        ))
                    }
                },
                Token::Operator(Operator::Minus) => {
                    let exp = eval_exp(expr)?;
                    if let Literal::Integer(v) = exp {
                        Ok(Literal::Integer(-v))
                    } else if let Literal::Float(v) = exp {
                        Ok(Literal::Float(-v))
                    } else {
                        Err(format!(
                            "Type mismatch for  '-' (unary operator), expected Integer or Float, but got expression of type: {:?}",
                            find_expression_type(expr)?
                        ))
                    }
                },
                t =>  Err(format!("Mismatched token: {:?} is not a unary operator as expected.", t)),
            }
            Expression::Binary(left, op, right) => match op {
                Token::Operator(Operator::Eq) => eval_eq(left, right),
                Token::Operator(Operator::Neq) => eval_neq(left, right),
                Token::Operator(Operator::Greater) => eval_greater(left, right),
                Token::Operator(Operator::GreaterEq) => eval_greatereq(left, right),
                Token::Operator(Operator::Lesser) => eval_lesser(left, right),
                Token::Operator(Operator::LesserEq) => eval_lessereq(left, right),
                Token::Operator(Operator::Plus) => eval_plus(left, right),
                Token::Operator(Operator::Minus) => eval_minus(left, right),
                Token::Operator(Operator::Multip) => eval_multip(left, right),
                Token::Operator(Operator::Div) => eval_div(left, right),
                Token::Operator(Operator::And) => eval_and(left, right),
                Token::Operator(Operator::Or) => eval_or(left, right),
                Token::Operator(Operator::BitAnd) => eval_bitand(left, right),
                Token::Operator(Operator::BitOr) => eval_bitor(left, right),
                Token::Operator(Operator::Shl) => eval_shl(left, right),
                Token::Operator(Operator::Shr) => eval_shr(left, right),
                Token::Operator(Operator::Modulo) => eval_modulo(left, right),
                _ => Err(format!("Mismatched token: {:?} is not a binary operator as expected.", op)),
            }
            Expression::Conditional(predicate, te, fe) => {
                let pred = eval_exp(predicate)?;
                if let Literal::Boolean(true) = pred {
                    eval_exp(te)
                } else if let Literal::Boolean(false) = pred {
                    eval_exp(fe)
                } else {
                    Err(format!("Type mismatch for ? :, expected Boolean predicate, but got expression of type: {:?}", find_expression_type(predicate)?))
                }
            }
            Expression::Tuple(_) => unimplemented!(),
        }
    };
    res
}

fn eval_eq(left: &Expression, right: &Expression) -> Result<Literal, String> {
    Ok(Literal::Boolean(eval_exp(left)? == eval_exp(right)?))

}

fn eval_neq(left: &Expression, right: &Expression) -> Result<Literal, String> {
    Ok(Literal::Boolean(eval_exp(left)? != eval_exp(right)?))
}

fn eval_greater(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l > (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean(l  as i64 > r)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean(l > r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l > r)),
        (_, _) => Err(format!("Type mismatch for >, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_greatereq(left: &Expression, right: &Expression, ) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l >= (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean(l  as i64 >= r)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean(l >= r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l >= r)),
        (_, _) => Err(format!("Type mismatch for >=, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_lesser(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l < (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean((l  as i64) < r)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean(l < r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l < r)),
        (_, _) => Err(format!("Type mismatch for <, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_lessereq(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l <= (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean((l  as i64) <= r)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean(l <= r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l <= r)),
        (_, _) => Err(format!("Type mismatch for <=, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_plus(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Integer(l + (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Float(l + r as f64)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l + r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Float(l + r)),
        (Ok(Literal::String(l)), Ok(Literal::String(r))) => Ok(Literal::String(format!("{}{}", l, r))),
        (_, _) => Err(format!("Type mismatch for +, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_minus(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Integer(l - (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Float(l - r as f64)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l - r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Float(l - r)),
        (_, _) => Err(format!("Type mismatch for -, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_multip(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Integer(l * (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Float(l * r as f64)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l * r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Float(l * r)),
        (_, _) => Err(format!("Type mismatch for *, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_div(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (_, Ok(Literal::Integer(0))) => Err("Cannot divide by 0".to_owned()),
        (_, Ok(Literal::Float(0.0))) => Err("Cannot divide by 0".to_owned()),
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Integer(l / (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Float(l / r as f64)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l / r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Float(l / r)),
        (_, _) => Err(format!("Type mismatch for /, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}



fn eval_and(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Boolean(true)), Ok(Literal::Boolean(true))) => Ok(Literal::Boolean(true)),
        (Ok(Literal::Boolean(_)), Ok(Literal::Boolean(_))) => Ok(Literal::Boolean(false)),
        (_, _) => Err(format!("Type mismatch for &&, expected Boolean, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_or(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Boolean(false)), Ok(Literal::Boolean(false))) => Ok(Literal::Boolean(false)),
        (Ok(Literal::Boolean(_)), Ok(Literal::Boolean(_))) => Ok(Literal::Boolean(true)),
        (_, _) => Err(format!("Type mismatch for ||, expected Boolean, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_bitand(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l & r)),
        (_, _) => Err(format!("Type mismatch for &, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_bitor(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l | r)),
        (_, _) => Err(format!("Type mismatch for |, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_shl(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(&left), eval_exp(&right)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l << r)),
        (_, _) => Err(format!("Type mismatch for <<, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_shr(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l >> r)),
        (_, _) => Err(format!("Type mismatch for >>, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_modulo(left: &Expression, right: &Expression) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left), find_expression_type(&right));

    match (eval_exp(left), eval_exp(right)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l % r)),
        (_, _) => Err(format!("Type mismatch for >>, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}



















