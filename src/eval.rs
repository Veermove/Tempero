use crate::{typedExpr::TExpression, lex::{Literal, Token, Operator}, parse::Expression};

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
            Expression::Binary() => unimplemented!(),
            Expression::Conditional() => unimplemented!(),
            Expression::Tuple() => unimplemented!(),
        }
    }
}
