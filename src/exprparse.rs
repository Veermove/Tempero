use std::fmt::Display;

use crate::lex::{self, Operator, Literal, Token};


pub type EnumeratedTokens = Vec<(Token, usize, usize)>;


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

pub fn prase_expressions(expression_tokens: EnumeratedTokens) -> (Vec<Expression>, usize, Vec<String>) {
    let mut exps = Vec::new();
    let mut errors = Vec::new();
    let mut st_index = 0;

    while st_index < expression_tokens.len() {
        match parse_expr(&expression_tokens, st_index) {
            Ok((exp, ind)) => {
                // dbg!(&exps);
                st_index = ind;
                exps.push(exp);
            }
            Err(msg) => {
                errors.push(msg);
                st_index = find_next_start(&expression_tokens, st_index);
            }
        }
    }
    return (exps, st_index, errors);
}

pub fn parse_expr(tokens: &EnumeratedTokens, index: usize) -> Result<(Expression, usize), String> {
    parse_conditional_exp(tokens, index)
}

fn parse_conditional_exp(tokens: &EnumeratedTokens, index: usize) -> Result<(Expression, usize), String> {
    let (condition, mut current_index) = parse_ors(tokens, index)?;
    'outer: loop {
        match tokens.get(current_index) {
            Some((Token::Operator(Operator::IfExpr), c, l)) => {
                let (if_expr, ci) = parse_expr(tokens, current_index + 1)?;
                current_index = ci;

                match tokens.get(current_index) {
                    Some((Token::Operator(Operator::ElseExpr), _, _)) => {
                        let (else_expr, ci) = parse_expr(tokens, current_index + 1)?;
                        current_index = ci;
                        return Ok((Expression::Conditional(Box::new(condition), Box::new(if_expr), Box::new(else_expr)), current_index));
                    },
                    _ => {
                        return Err(lex::report_error("Missing token - malformed conditional expression: missing condition-false branch ".to_string(), *c, *l))
                    },
                }
            },
            _ => {
                break 'outer;
            },
        }
    }
    return Ok((condition, current_index));
}

fn parse_ors(tokens: &EnumeratedTokens, index: usize) -> Result<(Expression, usize), String> {
    let (mut ret_left, mut current_index) = parse_ands(tokens, index)?;

    'outer: loop {
        match tokens.get(current_index) {
            Some((Token::Operator(Operator::Or), _, _)) => {
                let parse_result = parse_ands(tokens, current_index + 1)?;
                let right = parse_result.0;
                current_index = parse_result.1;

                ret_left = Expression::Binary(Box::new(ret_left), Operator::Or, Box::new(right));
            },
            _ => { break 'outer; },

        }
    }

    return Ok((ret_left, current_index));
}

fn parse_ands(tokens: &EnumeratedTokens, index: usize) -> Result<(Expression, usize), String> {
    let (mut ret_left, mut current_index) = parse_eqs(tokens, index)?;

    'outer: loop {
        match tokens.get(current_index) {
            Some((Token::Operator(Operator::And), _, _)) => {
                let parse_result = parse_eqs(tokens, current_index + 1)?;
                let right = parse_result.0;
                current_index = parse_result.1;

                ret_left = Expression::Binary(Box::new(ret_left), Operator::And, Box::new(right));
            },
            _ => { break 'outer; },
        }
    }

    return Ok((ret_left, current_index));
}

fn parse_eqs(tokens: &EnumeratedTokens, index: usize) -> Result<(Expression, usize), String> {
    let (mut ret_left, mut current_index) = prase_comparisons(tokens, index)?;

    'outer: loop {
        match tokens.get(current_index) {
            Some((Token::Operator(operator), _, _)) if *operator == Operator::Eq || *operator == Operator::Neq => {
                let parse_result = prase_comparisons(tokens, current_index + 1)?;
                let right = parse_result.0;
                current_index = parse_result.1;

                ret_left = Expression::Binary(Box::new(ret_left), *operator, Box::new(right));
            }
            _ => {
                break 'outer;
            }
        }
    }
    return Ok((ret_left, current_index));
}

fn prase_comparisons(tokens: &EnumeratedTokens, index: usize) -> Result<(Expression, usize), String> {
    let (mut ret_left, mut current_index) = prase_terms(tokens, index)?;

    use Operator::*;
    'outer: loop {
        match tokens.get(current_index) {
            Some((Token::Operator(operator), _, _)) if
                *operator == Greater
                || *operator == GreaterEq
                || *operator == Lesser
                || *operator == LesserEq =>
            {
                let parse_res = prase_terms(tokens, current_index + 1)?;
                let right = parse_res.0;
                current_index = parse_res.1;

                ret_left = Expression::Binary(Box::new(ret_left), operator.clone(), Box::new(right));
            }
            _ => {
                break 'outer;
            }
        }
    }
    return Ok((ret_left, current_index));
}

fn prase_terms(tokens: &EnumeratedTokens, index: usize) -> Result<(Expression, usize), String> {
    let (mut ret_left, mut current_index) = parse_factors(tokens, index)?;

    use Operator::*;
    'outer: loop {
        match tokens.get(current_index) {
            Some((Token::Operator(operator), _, _)) if *operator == Plus || *operator == Minus => {
                let parse_res = parse_factors(tokens, current_index + 1)?;
                let right = parse_res.0;
                current_index = parse_res.1;

                ret_left = Expression::Binary(Box::new(ret_left), operator.clone(), Box::new(right));
            }
            _ => {
                break 'outer;
            }
        }
    }
    return Ok((ret_left, current_index));
}

fn parse_factors(tokens: &EnumeratedTokens, index: usize) -> Result<(Expression, usize), String> {
    let (mut ret_left, mut current_index) = prase_unary(tokens, index)?;

    use Operator::*;
    'outer: loop {
        match tokens.get(current_index) {
            Some((Token::Operator(operator), _, _)) if
                *operator == Multip
                || *operator == Div
                || *operator == Shl
                || *operator == Shr
                || *operator == Modulo =>
            {
                let parse_res = prase_unary(tokens, current_index + 1)?;
                let right = parse_res.0;
                current_index = parse_res.1;

                ret_left = Expression::Binary(Box::new(ret_left), operator.clone(), Box::new(right));
            }
            _ => {
                break 'outer;
            }
        }
    }
    return Ok((ret_left, current_index));
}

fn prase_unary(tokens: &EnumeratedTokens, index: usize) -> Result<(Expression, usize), String> {
    use Operator::*;
    match tokens.get(index) {
        Some((Token::Operator(operator), _, _))
            if *operator == Not || *operator == Minus =>
        {
            let parse_res = prase_unary(tokens, index + 1)?;
            let right = parse_res.0;
            let current_index = parse_res.1;

            return Ok((Expression::Unary(*operator, Box::new(right)), current_index))
        }
        _ => {
            return prase_primary(tokens, index);
        }
    }
}

fn prase_primary(tokens: &EnumeratedTokens, index: usize) -> Result<(Expression, usize), String> {
    let current_token = tokens.get(index);

    match current_token {
        Some((Token::Identifier(ident), _, _))
            => Ok((Expression::Variable(ident.to_string()), index + 1)),

        Some((Token::Literal(v), _, _))
            => Ok((Expression::Literal(v.clone()), index + 1)),

        Some((Token::Operator(Operator::OpParenth), l, c)) => {
            let (expr, n_index) = parse_expr(tokens, index + 1)?;
            let after_token = tokens.get(n_index);


            match after_token {
                Some((Token::Operator(Operator::CloParenth), _, _)) => { // Grouping
                    return Ok((Expression::Grouping(Box::new(expr)), n_index + 1));
                },
                Some((Token::Operator(Operator::Separator), l, c)) => { // Tuples
                    let mut next_index = n_index + 1;
                    let mut tuple_vals = vec![expr];
                    return loop {
                        let (parsed_expr, ni) = parse_expr(tokens, next_index)?;
                        tuple_vals.push(parsed_expr);
                        next_index = ni;

                        match tokens.get(next_index) {
                            Some((Token::Operator(Operator::Separator), _, _)) => {
                                next_index = next_index + 1
                            },
                            Some((Token::Operator(Operator::CloParenth), _, _)) => {
                                break Ok((Expression::Tuple(tuple_vals), next_index + 1));
                            },
                            _ => {
                                break Err(lex::report_error("Expected ')' that was never found. ".to_owned(), *l, *c));
                            }
                        };
                    }
                },
                _ => {
                    return Err(lex::report_error("Expected ')' that was never found. ".to_owned(), *l, *c));
                }
            }
        },
        Some((_, l, c)) => return Err(lex::report_error("Unexpected token: Expected Exprssion.".to_owned(), *l, *c)),
        None => return Err("Unexpected token: Expected Exprssion.".to_owned()),
    }
}

pub fn find_next_start(tokens: &EnumeratedTokens, index: usize) -> usize {
    tokens.as_slice()
        .iter()
        .enumerate()
        .skip(index)
        .find(|(_, (token, _, _))| match token {
            Token::Eof | Token::Keyword(_) | Token::Operator(Operator::Semicolon) => true,
            _ => false
        })
        .map(|(i, _)| i + 1)
        .unwrap_or(tokens.len() + 1)
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
