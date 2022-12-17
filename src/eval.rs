use crate::{lex::{Literal, Token, Operator}, parse::Expression};

pub fn eval_exp(exp: Expression) -> Result<Literal, String> {

    let res = {
        match exp {
            Expression::Literal(val) => match val {
                Token::FloatLiteral(v) => Ok(Literal::Float(v)),
                Token::IntegerLiteral(v) => Ok(Literal::Integer(v)),
                Token::BooleanLiteral(v) => Ok(Literal::Boolean(v)),
                Token::StringLiteral(v) => Ok(Literal::String(v)),
                _ => unreachable!(),
            },
            Expression::Grouping(exp) => eval_exp(*exp),
            Expression::Unary(op, exp) => match op {
                Token::Operator(Operator::Not) => {
                    let exp = eval_exp(*exp)?;
                    if let Literal::Boolean(v) = exp {
                        Ok(Literal::Boolean(!v))
                    } else {
                        unreachable!()
                    }
                },
                Token::Operator(Operator::Minus) => {
                    let exp = eval_exp(*exp)?;
                    if let Literal::Integer(v) = exp {
                        Ok(Literal::Integer(-v))
                    } else if let Literal::Float(v) = exp {
                        Ok(Literal::Float(-v))
                    } else {
                        unreachable!()
                    }
                },
                _ => unreachable!(),
            }
            Expression::Binary(left, op, right) => match op {
                Token::Operator(Operator::) =>
            }
            Expression::Conditional(pred, te, fe) => {
                let pred = eval_exp(*pred)?;
                if let Literal::Boolean(true) = pred {
                    eval_exp(*te)
                } else if let Literal::Boolean(false) = pred {
                    eval_exp(*fe)
                } else {
                    unreachable!()
                }
            }
            Expression::Tuple(_) => unimplemented!(),
        }
    };
    res
}

fn eval_eq(left: Expression, right: Expression) -> Result<Literal, String> {
    Ok(Literal::Boolean(eval_exp(left)? == eval_exp(right)?))

}

fn eval_neq(left: Expression, right: Expression) -> Result<Literal, String> {
    Ok(Literal::Boolean(eval_exp(left)? != eval_exp(right)?))
}

fn eval_greater(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_greatereq(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_lesser(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_lessereq(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_plus(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_minus(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_multip(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_div(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_not(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_and(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_or(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_bitand(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_bitor(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_shl(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_shr(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}

fn eval_modulo(left: Expression, right: Expression) -> Result<Literal, String> {
    match (eval_exp(left), eval_exp(right)) {

    }
}
