use crate::{
    lex::{Literal, Operator},
    exprparse::Expression,
    typecheck::find_expression_type, stmtparse::State
};

pub fn eval_exp(exp: &Expression, state: &State) -> Result<Literal, String> {

    let res = {
        match exp {
            Expression::Literal(val) => Ok(val.clone()),
            Expression::Variable(var_name) => resolve_variable(var_name, state),
            Expression::Grouping(exp) => eval_exp(exp, state),
            Expression::Unary(op, expr) => match op {
                Operator::Not => {
                    let exp = eval_exp(expr, state)?;
                    if let Literal::Boolean(v) = exp {
                        Ok(Literal::Boolean(!v))
                    } else {
                        Err(format!(
                            "Type mismatch for  '!' (unary operator), expected Boolean, but got expression of type: {:?}",
                            find_expression_type(expr, state)?
                        ))
                    }
                },
                Operator::Minus => {
                    let exp = eval_exp(expr, state)?;
                    if let Literal::Integer(v) = exp {
                        Ok(Literal::Integer(-v))
                    } else if let Literal::Float(v) = exp {
                        Ok(Literal::Float(-v))
                    } else {
                        Err(format!(
                            "Type mismatch for  '-' (unary operator), expected Integer or Float, but got expression of type: {:?}",
                            find_expression_type(expr, state)?
                        ))
                    }
                },
                t =>  Err(format!("Mismatched token: {:?} is not a unary operator as expected.", t)),
            }
            Expression::Binary(left, op, right) => match op {
                Operator::Eq => eval_eq(left, right, state),
                Operator::Neq => eval_neq(left, right, state),
                Operator::Greater => eval_greater(left, right, state),
                Operator::GreaterEq => eval_greatereq(left, right, state),
                Operator::Lesser => eval_lesser(left, right, state),
                Operator::LesserEq => eval_lessereq(left, right, state),
                Operator::Plus => eval_plus(left, right, state),
                Operator::Minus => eval_minus(left, right, state),
                Operator::Multip => eval_multip(left, right, state),
                Operator::Div => eval_div(left, right, state),
                Operator::And => eval_and(left, right, state),
                Operator::Or => eval_or(left, right, state),
                Operator::BitAnd => eval_bitand(left, right, state),
                Operator::BitOr => eval_bitor(left, right, state),
                Operator::Shl => eval_shl(left, right, state),
                Operator::Shr => eval_shr(left, right, state),
                Operator::Modulo => eval_modulo(left, right, state),
                _ => Err(format!("Mismatched token: {:?} is not a binary operator as expected.", op)),
            }
            Expression::Conditional(predicate, te, fe) => {
                let pred = eval_exp(predicate, state)?;
                if let Literal::Boolean(true) = pred {
                    eval_exp(te, state)
                } else if let Literal::Boolean(false) = pred {
                    eval_exp(fe, state)
                } else {
                    Err(format!("Type mismatch for ? :, expected Boolean predicate, but got expression of type: {:?}", find_expression_type(predicate, state)?))
                }
            }
            Expression::Tuple(_) => unimplemented!(),
        }
    };
    res
}

fn resolve_variable(variable_name: &String, state: &State) -> Result<Literal, String> {
    if state.contains_key(variable_name.as_str()) {
        state.get(variable_name.as_str())
            .map(|s| s.value.clone())
            .ok_or(format!("Cannot resolve variable: {}", variable_name))
            .and_then(|exp| eval_exp(&exp.expression, state))
    } else {
        Err(format!("Cannot resolve variable: {}", variable_name))
    }
}

fn eval_eq(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    Ok(Literal::Boolean(eval_exp(left, state)? == eval_exp(right, state)?))

}

fn eval_neq(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    Ok(Literal::Boolean(eval_exp(left, state)? != eval_exp(right, state)?))
}

fn eval_greater(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (
        find_expression_type(&left, state),
        find_expression_type(&right, state)
    );

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l > (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean(l  as i64 > r)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean(l > r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l > r)),
        (_, _) => Err(format!("Type mismatch for >, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_greatereq(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l >= (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean(l  as i64 >= r)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean(l >= r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l >= r)),
        (_, _) => Err(format!("Type mismatch for >=, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_lesser(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l < (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean((l  as i64) < r)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean(l < r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l < r)),
        (_, _) => Err(format!("Type mismatch for <, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_lessereq(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l <= (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean((l  as i64) <= r)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Boolean(l <= r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Boolean(l <= r)),
        (_, _) => Err(format!("Type mismatch for <=, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_plus(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Integer(l + (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Float(l + r as f64)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l + r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Float(l + r)),
        (Ok(Literal::String(l)), Ok(Literal::String(r))) => Ok(Literal::String(format!("{}{}", l, r))),
        (_, _) => Err(format!("Type mismatch for +, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_minus(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Integer(l - (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Float(l - r as f64)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l - r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Float(l - r)),
        (_, _) => Err(format!("Type mismatch for -, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_multip(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Integer(l * (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Float(l * r as f64)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l * r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Float(l * r)),
        (_, _) => Err(format!("Type mismatch for *, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_div(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (_, Ok(Literal::Integer(0))) => Err("Cannot divide by 0".to_owned()),
        (_, Ok(Literal::Float(0.0))) => Err("Cannot divide by 0".to_owned()),
        (Ok(Literal::Integer(l)), Ok(Literal::Float(r))) => Ok(Literal::Integer(l / (r as i64))),
        (Ok(Literal::Float(l)), Ok(Literal::Integer(r))) => Ok(Literal::Float(l / r as f64)),
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l / r)),
        (Ok(Literal::Float(l)), Ok(Literal::Float(r))) => Ok(Literal::Float(l / r)),
        (_, _) => Err(format!("Type mismatch for /, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}



fn eval_and(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Boolean(true)), Ok(Literal::Boolean(true))) => Ok(Literal::Boolean(true)),
        (Ok(Literal::Boolean(_)), Ok(Literal::Boolean(_))) => Ok(Literal::Boolean(false)),
        (_, _) => Err(format!("Type mismatch for &&, expected Boolean, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_or(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Boolean(false)), Ok(Literal::Boolean(false))) => Ok(Literal::Boolean(false)),
        (Ok(Literal::Boolean(_)), Ok(Literal::Boolean(_))) => Ok(Literal::Boolean(true)),
        (_, _) => Err(format!("Type mismatch for ||, expected Boolean, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_bitand(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l & r)),
        (_, _) => Err(format!("Type mismatch for &, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_bitor(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l | r)),
        (_, _) => Err(format!("Type mismatch for |, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_shl(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l << r)),
        (_, _) => Err(format!("Type mismatch for <<, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_shr(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l >> r)),
        (_, _) => Err(format!("Type mismatch for >>, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}

fn eval_modulo(left: &Expression, right: &Expression, state: &State) -> Result<Literal, String> {
    let (l_t, r_t) = (find_expression_type(&left, state), find_expression_type(&right, state));

    match (eval_exp(left, state), eval_exp(right, state)) {
        (Ok(Literal::Integer(l)), Ok(Literal::Integer(r))) => Ok(Literal::Integer(l % r)),
        (_, _) => Err(format!("Type mismatch for >>, expected Float or Integer, but got {:?}", vec![l_t?, r_t?] )),
    }
}



















