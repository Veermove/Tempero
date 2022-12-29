use std::fmt::Display;

use crate::lex::{Token, Operator, self, Literal};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(Token),
    Grouping(Box<Expression>),
    Unary(Operator, Box<Expression>),
    Binary(Box<Expression>, Operator, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Tuple(Vec<Expression>)
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(token) => write!(f, "{}", token),
            Expression::Grouping(e) => write!(f, "( {} )", e),
            Expression::Unary(op, exp) => write!(f, "{}( {} )", op, exp),
            Expression::Binary(l, op, r) => write!(f, "{} {} {}", l, op, r),
            Expression::Conditional(c, l, r) => write!(f, "{} ? {} : {} ", c, l, r),
            Expression::Tuple(vals) => unimplemented!(),
        }
        //  write!(f, "({}, {}, {})", self.a, self.b, self.c)
    }
}

pub type EnumeratedTokens = Vec<(Token, usize, usize)>;

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
        Some((Token::Literal(Literal::Boolean(_)), _, _))
        | Some((Token::Literal(Literal::Float(_)), _, _))
        | Some((Token::Literal(Literal::Integer(_)), _, _))
        | Some((Token::Literal(Literal::String(_)), _, _)) => {
            let (token, _, _) = current_token.expect("Unr!");
            return Ok((Expression::Literal(token.clone()), index + 1))
        },
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

mod tests {
//     #![allow(unused_imports)]
//     use crate::{lex::{TokenInfo, Token, Keyword, Operator, self, Literal, to_simple_format}, Expression, exprparse::{parse_expr, find_next_start}};



// #[test]
// fn should_parse_literals_lvl_grouped_ungrouped() {
//     let place = Place::new((0, 0));
//     let tokens = vec![
//         TokenInfo::new(Token::Literal(Literal::Boolean(true)), place.clone()),
//     ];

//     let res = parse_expr(&to_simple_format(tokens), 0);

//     assert_eq!(res, Ok((Expression::Literal(Token::Literal(Literal::Boolean(true))), 1)), "Ungrouped");

//     let tokens = vec![
//         TokenInfo::new(Token::Operator(Operator::OpParenth), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Boolean(true)), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::CloParenth), place.clone()),
//     ];

//     let res = parse_expr(&tokens, 0);

//     assert_eq!(res, Ok((Expression::Grouping(Box::new(Expression::Literal(Token::Literal(Literal::Boolean(true))))), 3)), "Grouped");
// }

// #[test]
// fn should_parse_unary_operator_lvl_grouped_ungrouped() {
//     let place = Place::new((0, 0));
//     let tokens = vec![
//         TokenInfo::new(Token::Operator(Operator::Not), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Float(1.0)), place.clone()),
//     ];

//     let res = parse_expr(&tokens, 0);

//     assert_eq!(res, Ok((Expression::Unary(
//         Token::Operator(Operator::Not), Box::new(Expression::Literal(Token::Literal(Literal::Float(1.0))))), 2)), "Ungrouped");

//     let tokens = vec![
//         TokenInfo::new(Token::Operator(Operator::Minus), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::OpParenth), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Boolean(true)), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::CloParenth), place.clone()),
//     ];

//     let res = parse_expr(&tokens, 0);

//     assert_eq!(res, Ok(
//         (Expression::Unary(
//             Token::Operator(Operator::Minus),
//             Box::new(Expression::Grouping(Box::new(Expression::Literal(Token::Literal(Literal::Boolean(true))))))),
//         4)
//         ), "Grouped");
// }

// #[test]
// fn should_parse_factor_lvl_grouped_ungrouped() {
//     let place = Place::new((0, 0));
//     let tokens = vec![
//         TokenInfo::new(Token::Literal(Literal::Float(1.0)), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::Multip), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Float(1.0)), place.clone()),
//     ];

//     let res = parse_expr(&tokens, 0);

//     assert_eq!(res, Ok((Expression::Binary(Box::new(Expression::Literal(Token::Literal(Literal::Float(1.0)))),
//         Token::Operator(Operator::Multip), Box::new(Expression::Literal(Token::Literal(Literal::Float(1.0))))), 3)), "Ungrouped");

//     let expr = "!(-1 * 2) / 3".to_owned();
//     let lex_res = lex::lex_content(expr).unwrap();
//     assert!(lex_res.len() == 1);
//     for (r, _) in lex_res {
//         let res = parse_expr(&r, 0);
//         use Operator::*;
//         assert_eq!(res, Ok((
//             Expression::Binary(
//                 Box::new(Expression::Unary(
//                     Token::Operator(Not),
//                     Box::new(Expression::Grouping(
//                         Box::new(Expression::Binary(
//                             Box::new(Expression::Unary(Token::Operator(Minus), Box::new(Expression::Literal(Token::Literal(Literal::Integer(1)))))),
//                             Token::Operator(Multip),
//                             Box::new(Expression::Literal(Token::Literal(Literal::Integer(2))))))
//                     ))
//                 )),
//                 Token::Operator(Div),
//                 Box::new(Expression::Literal(Token::Literal(Literal::Integer(3))))
//             ),
//             9))
//         , "Grouped and nested")
//     }
//     //

//     // assert_eq!(res, Ok((Expression::Grouping(Box::new(Expression::Literal(Token::Literal(Literal::Boolean(true))))), 3)), "Grouped");
// }

// #[test]
// fn should_parse_tuples() {

//     let expr = "(1, true, !(false || true), (1, 2), 1 + 2)\n(1, (2), ((2, 3)))".to_owned();

//     use Expression::*;
//     let expected = vec![
//         Ok((
//             Tuple(vec![
//                 Literal(Token::Literal(crate::Literal::Integer(1))),
//                 Literal(Token::Literal(crate::Literal::Boolean(true))),
//                 Unary(
//                     Token::Operator(Operator::Not),
//                     Box::new(Grouping(Box::new(Binary(
//                         Box::new(Literal(Token::Literal(crate::Literal::Boolean(false)))),
//                         Token::Operator(Operator::Or),
//                         Box::new(Literal(Token::Literal(crate::Literal::Boolean(true))))
//                     ))))
//                 ),
//                 Tuple(vec![Literal(Token::Literal(crate::Literal::Integer(1))), Literal(Token::Literal(crate::Literal::Integer(2)))]),
//                 Binary(
//                     Box::new(Literal(Token::Literal(crate::Literal::Integer(1)))),
//                     Token::Operator(Operator::Plus),
//                     Box::new(Literal(Token::Literal(crate::Literal::Integer(2))))
//                 )
//             ]),
//         22)),
//         Ok((
//             Tuple(vec![
//                 Literal(Token::Literal(crate::Literal::Integer(1))),
//                 Grouping(Box::new(Literal(Token::Literal(crate::Literal::Integer(2))))),
//                 Grouping(Box::new(Tuple(vec![Literal(Token::Literal(crate::Literal::Integer(2))), Literal(Token::Literal(crate::Literal::Integer(3)))]))),
//             ]),
//         15))
//     ];

//     let lex_res = lex::lex_content(expr).unwrap();
//     assert!(lex_res.len() == expected.len());
//     for ((r, _), exp) in lex_res.into_iter().zip(expected.into_iter()) {
//         let res = parse_expr(&r, 0);
//         assert_eq!(res, exp, "Grouped and nested");
//     }
// }

// #[test]
// fn should_parse_addition_lvl_grouped_ungrouped() {
//     let place = Place::new((0, 0));
//     let tokens = vec![
//         TokenInfo::new(Token::Literal(Literal::Float(1.0)), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Float(1.0)), place.clone()),
//     ];

//     let res = parse_expr(&tokens, 0);

//     assert_eq!(res, Ok((Expression::Binary(Box::new(Expression::Literal(Token::Literal(Literal::Float(1.0)))),
//         Token::Operator(Operator::Plus), Box::new(Expression::Literal(Token::Literal(Literal::Float(1.0))))), 3)), "Ungrouped");

//     let expr = "!(-1 * 2 - 3) / 3 + 1".to_owned();
//     let lex_res = lex::lex_content(expr).unwrap();
//     assert!(lex_res.len() == 1);
//     for (r, _) in lex_res {
//         let res = parse_expr(&r, 0);
//         use Operator::*;
//         assert_eq!(res, Ok((
//             Expression::Binary(
//                 Box::new(Expression::Binary(
//                     Box::new(Expression::Unary(
//                         Token::Operator(Not),
//                         Box::new(Expression::Grouping(Box::new(Expression::Binary(
//                             Box::new(Expression::Binary(
//                                 Box::new(Expression::Unary(Token::Operator(Minus), Box::new(Expression::Literal(Token::Literal(Literal::Integer(1)))))),
//                                 Token::Operator(Multip),
//                                 Box::new(Expression::Literal(Token::Literal(Literal::Integer(2)))))),
//                             Token::Operator(Minus),
//                             Box::new(Expression::Literal(Token::Literal(Literal::Integer(3))))
//                         ))))
//                     )),
//                     Token::Operator(Div),
//                     Box::new(Expression::Literal(Token::Literal(Literal::Integer(3))))
//                 )),
//                 Token::Operator(Plus),
//                 Box::new(Expression::Literal(Token::Literal(Literal::Integer(1))))
//             ),
//             13))
//         , "Grouped and nested")
//     }
// }

// #[test]
// fn should_parse_comparison_lvl_grouped_ungrouped() {
//     let place = Place::new((0, 0));
//     let tokens = vec![
//         TokenInfo::new(Token::Literal(Literal::Float(1.0)), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::GreaterEq), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Float(1.0)), place.clone()),
//     ];

//     let res = parse_expr(&tokens, 0);

//     assert_eq!(res, Ok((Expression::Binary(Box::new(Expression::Literal(Token::Literal(Literal::Float(1.0)))),
//         Token::Operator(Operator::GreaterEq), Box::new(Expression::Literal(Token::Literal(Literal::Float(1.0))))), 3)), "Ungrouped");

//     let expr = "!(-1 * 2 - 3) / 3 + 1 > (0 <= 3 < 1) ".to_owned();
//     let lex_res = lex::lex_content(expr).unwrap();
//     assert!(lex_res.len() == 1);
//     for (r, _) in lex_res {
//         let res = parse_expr(&r, 0);
//         use Operator::*;
//         assert_eq!(res, Ok((
//             Expression::Binary(
//                 Box::new(Expression::Binary(
//                     Box::new(Expression::Binary(
//                         Box::new(Expression::Unary(
//                             Token::Operator(Not),
//                             Box::new(Expression::Grouping(Box::new(Expression::Binary(
//                                 Box::new(Expression::Binary(
//                                     Box::new(Expression::Unary(Token::Operator(Minus), Box::new(Expression::Literal(Token::Literal(Literal::Integer(1)))))),
//                                     Token::Operator(Multip),
//                                     Box::new(Expression::Literal(Token::Literal(Literal::Integer(2)))))),
//                                 Token::Operator(Minus),
//                                 Box::new(Expression::Literal(Token::Literal(Literal::Integer(3))))
//                             ))))
//                         )),
//                         Token::Operator(Div),
//                         Box::new(Expression::Literal(Token::Literal(Literal::Integer(3))))
//                     )),
//                     Token::Operator(Plus),
//                     Box::new(Expression::Literal(Token::Literal(Literal::Integer(1))))
//                 )),
//                 Token::Operator(Greater),
//                 Box::new(Expression::Grouping(Box::new(Expression::Binary(
//                     Box::new(Expression::Binary(
//                         Box::new(Expression::Literal(Token::Literal(Literal::Integer(0)))),
//                         Token::Operator(LesserEq),
//                         Box::new(Expression::Literal(Token::Literal(Literal::Integer(3))))
//                     )),
//                     Token::Operator(Lesser),
//                     Box::new(Expression::Literal(Token::Literal(Literal::Integer(1))))
//                 ))))
//             ),
//             21))
//         , "Grouped and nested")
//     }
// }

// #[test]
// fn should_parse_eq_neq_lvl_grouped_ungrouped() {
//     let place = Place::new((0, 0));
//     let tokens = vec![
//         TokenInfo::new(Token::Literal(Literal::Float(1.0)), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::Neq), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Float(1.0)), place.clone()),
//     ];

//     let res = parse_expr(&tokens, 0);

//     assert_eq!(res, Ok((Expression::Binary(Box::new(Expression::Literal(Token::Literal(Literal::Float(1.0)))),
//         Token::Operator(Operator::Neq), Box::new(Expression::Literal(Token::Literal(Literal::Float(1.0))))), 3)), "Ungrouped");

//     let expr = "!(-1 * 2 - 3) / 3 + 1 == (0 <= 3 < 1 != 13 * 14) ".to_owned();
//     let lex_res = lex::lex_content(expr).unwrap();
//     assert!(lex_res.len() == 1);
//     for (r, _) in lex_res {
//         let res = parse_expr(&r, 0);
//         use Operator::*;
//         assert_eq!(res, Ok((
//             Expression::Binary(
//                 Box::new(Expression::Binary(
//                     Box::new(Expression::Binary(
//                         Box::new(Expression::Unary(
//                             Token::Operator(Not),
//                             Box::new(Expression::Grouping(Box::new(Expression::Binary(
//                                 Box::new(Expression::Binary(
//                                     Box::new(Expression::Unary(Token::Operator(Minus), Box::new(Expression::Literal(Token::Literal(Literal::Integer(1)))))),
//                                     Token::Operator(Multip),
//                                     Box::new(Expression::Literal(Token::Literal(Literal::Integer(2)))))),
//                                 Token::Operator(Minus),
//                                 Box::new(Expression::Literal(Token::Literal(Literal::Integer(3))))
//                             ))))
//                         )),
//                         Token::Operator(Div),
//                         Box::new(Expression::Literal(Token::Literal(Literal::Integer(3))))
//                     )),
//                     Token::Operator(Plus),
//                     Box::new(Expression::Literal(Token::Literal(Literal::Integer(1))))
//                 )),
//                 Token::Operator(Eq),
//                 Box::new(Expression::Grouping(
//                     Box::new(Expression::Binary(
//                         Box::new(Expression::Binary(
//                             Box::new(Expression::Binary(
//                                 Box::new(Expression::Literal(Token::Literal(Literal::Integer(0)))),
//                                 Token::Operator(LesserEq),
//                                 Box::new(Expression::Literal(Token::Literal(Literal::Integer(3))))
//                             )),
//                             Token::Operator(Lesser),
//                             Box::new(Expression::Literal(Token::Literal(Literal::Integer(1))))
//                         )),
//                         Token::Operator(Neq),
//                         Box::new(Expression::Binary(
//                             Box::new(Expression::Literal(Token::Literal(Literal::Integer(13)))),
//                             Token::Operator(Multip),
//                             Box::new(Expression::Literal(Token::Literal(Literal::Integer(14))))
//                         ))
//                     )
//                 )))
//             ),
//             25))
//         , "Grouped and nested")
//     }
// }

// #[test]
// fn should_parse_ors_ands_lvl_grouped_ungrouped() {
//     let place = Place::new((0, 0));
//     let tokens = vec![
//         TokenInfo::new(Token::Literal(Literal::Boolean(true)), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::And), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Boolean(true)), place.clone()),
//     ];

//     let res = parse_expr(&tokens, 0);

//     assert_eq!(res, Ok((Expression::Binary(Box::new(Expression::Literal(Token::Literal(Literal::Boolean(true)))),
//         Token::Operator(Operator::And), Box::new(Expression::Literal(Token::Literal(Literal::Boolean(true))))), 3)), "Ungrouped");

//     let expr = "!(true || false && true) || true && false ".to_owned();
//     let lex_res = lex::lex_content(expr).unwrap();
//     assert!(lex_res.len() == 1);
//     for (r, _) in lex_res {
//         let res = parse_expr(&r, 0);
//         use Expression::*;
//         use Token::*;
//         assert_eq!(res, Ok((Binary(
//             Box::new(Unary(
//                 Operator(lex::Operator::Not),
//                 Box::new(Grouping(Box::new(Binary(
//                     Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(true)))),
//                     Operator(lex::Operator::Or),
//                     Box::new(Binary(
//                         Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(false)))),
//                         Operator(lex::Operator::And),
//                         Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(true))))
//                     ))
//                 ))))
//             )),
//             Operator(lex::Operator::Or),
//             Box::new(Binary(
//                 Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(true)))),
//                 Operator(lex::Operator::And),
//                 Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(false))))
//             ))
//         ), 12)))
//     }
// }

// #[test]
// fn should_parse_conditional_expression_grouped_ungrouped() {
//     let place = Place::new((0, 0));
//     let tokens = vec![
//         TokenInfo::new(Token::Literal(crate::Literal::Boolean(true)), place.clone()),
//         TokenInfo::new(Token::Operator(lex::Operator::IfExpr), place.clone()),
//         TokenInfo::new(Token::Literal(crate::Literal::Boolean(false)), place.clone()),
//         TokenInfo::new(Token::Operator(lex::Operator::ElseExpr), place.clone()),
//         TokenInfo::new(Token::Literal(crate::Literal::Boolean(false)), place.clone()),
//     ];

//     let res = parse_expr(&tokens, 0);

//     use Expression::*;
//     use Token::*;
//     assert_eq!(res, Ok((Conditional(Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(true)))),
//         Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(false)))), Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(false))))), 5)), "Ungrouped");

//     let expr = "!(true || false && true) ? 1 + 2 * 3 : 1 < 2 == false".to_owned();
//     let lex_res = lex::lex_content(expr).unwrap();
//     assert!(lex_res.len() == 1);
//     for (r, _) in lex_res {
//         let res = parse_expr(&r, 0);
//         assert_eq!(res, Ok((
//             Conditional(
//                 Box::new(Unary(
//                     Operator(lex::Operator::Not),
//                     Box::new(Grouping(Box::new(Binary(
//                         Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(true)))),
//                         Operator(lex::Operator::Or),
//                         Box::new(Binary(
//                             Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(false)))),
//                             Operator(lex::Operator::And),
//                             Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(true))))
//                         ))
//                     ))))
//                 )),
//                 Box::new(Binary(
//                     Box::new(Expression::Literal(Token::Literal(crate::Literal::Integer(1)))),
//                     Operator(lex::Operator::Plus),
//                     Box::new(Binary(
//                         Box::new(Expression::Literal(Token::Literal(crate::Literal::Integer(2)))),
//                         Operator(lex::Operator::Multip),
//                         Box::new(Expression::Literal(Token::Literal(crate::Literal::Integer(3))))
//                     ))
//                 )),
//                 Box::new(Binary(
//                     Box::new(Binary(
//                         Box::new(Expression::Literal(Token::Literal(crate::Literal::Integer(1)))),
//                         Operator(lex::Operator::Lesser),
//                         Box::new(Expression::Literal(Token::Literal(crate::Literal::Integer(2))))
//                     )),
//                     Operator(lex::Operator::Eq),
//                     Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(false))))
//                 )),
//             )
//         , 20)))
//     }
// }

// #[test]
// fn should_parse_nested_conditional_expressions() {
//     let expr = "1 ? (true ? 0 : 2) : 1 == 2 ? 200 : 400".to_owned();
//     let lex_res = lex::lex_content(expr).unwrap();
//     assert!(lex_res.len() == 1);
//     use Expression::*;
//     use Token::*;
//     for (r, _) in lex_res {
//         let res = parse_expr(&r, 0);
//         assert_eq!(res, Ok((
//             Conditional(
//                 Box::new(Expression::Literal(Token::Literal(crate::Literal::Integer(1)))),
//                 Box::new(Grouping(Box::new(Conditional(
//                     Box::new(Expression::Literal(Token::Literal(crate::Literal::Boolean(true)))),
//                     Box::new(Expression::Literal(Token::Literal(crate::Literal::Integer(0)))),
//                     Box::new(Expression::Literal(Token::Literal(crate::Literal::Integer(2)))),
//                 )))),
//                 Box::new(Conditional(
//                     Box::new(Binary(Box::new(Expression::Literal(Token::Literal(crate::Literal::Integer(1)))), Operator(lex::Operator::Eq), Box::new(Expression::Literal(Token::Literal(crate::Literal::Integer(2)))))),
//                     Box::new(Expression::Literal(Token::Literal(crate::Literal::Integer(200)))),
//                     Box::new(Expression::Literal(Token::Literal(crate::Literal::Integer(400)))),
//                 )),
//             )
//         , 17)))
//     }
// }

// #[test]
// fn sohuld_find_next_expression_start() {
//     let place = Place::new((0, 0));
//     let tokens = vec![
//         TokenInfo::new(Token::Literal(Literal::Boolean(true)), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Boolean(true)), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Boolean(true)), place.clone()),
//         TokenInfo::new(Token::Keyword(Keyword::Let), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::Semicolon), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Boolean(true)), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Boolean(true)), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Boolean(true)), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Boolean(true)), place.clone()),
//         TokenInfo::new(Token::Eof, place.clone()),
//     ];

//     assert_eq!(4, find_next_start(&tokens, 0));
//     assert_eq!(5, find_next_start(&tokens, 4));
//     assert_eq!(None, tokens.get(find_next_start(&tokens, 9)));
// }

// #[test]
// fn sohuld_parse_expression_and_expect_panic() {
//     let place = Place::new((0, 0));
//     let tokens = vec![
//         // TokenInfo::new(Token::Literal(Literal::Integer(1)), place.clone()),
//         // TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
//         // TokenInfo::new(Token::Literal(Literal::Integer(2)), place.clone()),
//         // TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Integer(3)), place.clone()),
//         TokenInfo::new(Token::Keyword(Keyword::Let), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Integer(4)), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Integer(4)), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::Semicolon), place.clone()),
//         // line 1: 1 + 2 + 3 let + 4; 13 + 13
//         TokenInfo::new(Token::Literal(Literal::Integer(13)), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
//         TokenInfo::new(Token::Literal(Literal::Integer(13)), place.clone()),
//     ];

//     let (res_e, ind, s) = prase_expression(tokens);
//     dbg!(&res_e, ind, &s);
//     assert_eq!(
//         res_e,
//         vec![Expression::Binary(
//             Box::new(Expression::Literal(Token::Literal(Literal::Integer(13)))),
//             Token::Operator(Operator::Plus),
//             Box::new(Expression::Literal(Token::Literal(Literal::Integer(13))))
//         )]
//     );
//     assert_eq!(ind, 12);
//     assert_eq!(s.len(), 2);

// }


}
