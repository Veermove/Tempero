use crate::lex::{TokenInfo, Token, Operator, self};

pub fn prase_expressions(expression_tokens: Vec<TokenInfo>) -> (Vec<Expression>, usize, Vec<String>) {
    let mut exps = Vec::new();
    let mut errors = Vec::new();
    let mut st_index = 0;

    while st_index < expression_tokens.len() {
        match start_parse(&expression_tokens, st_index) {
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

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(Token),
    Grouping(Box<Expression>),
    Unary(Token, Box<Expression>),
    Binary(Box<Expression>, Token, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Tuple(Vec<Expression>)
}

fn start_parse(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    parse_conditional_exp(tokens, index)
}

fn parse_conditional_exp(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    let (condition, mut current_index) = parse_ors(tokens, index)?;

    'outer: loop {
        let current_token = tokens.get(current_index);

        match current_token.map(|t| &t.token) {
            Some(Token::Operator(Operator::IfExpr)) => {
                let (if_expr, ci) = start_parse(tokens, current_index + 1)?;
                current_index = ci;

                let current_new_token = tokens.get(current_index);
                match current_new_token.map(|t| &t.token) {
                    Some(Token::Operator(Operator::ElseExpr)) => {
                        let (else_expr, ci) = start_parse(tokens, current_index + 1)?;
                        current_index = ci;
                        return Ok((Expression::Conditional(Box::new(condition), Box::new(if_expr), Box::new(else_expr)), current_index));
                    },
                    _ => {
                        return Err(lex::create_error_msg("Missing token - malformed conditional expression: missing condition-false branch ".to_string(), tokens.get(current_index), None, None))
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

fn parse_ors(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    let (mut ret_left, mut current_index) = parse_ands(tokens, index)?;

    'outer: loop {
        let current_token = tokens.get(current_index);
        match current_token.map(|t| &t.token) {
            Some(Token::Operator(Operator::Or)) => {
                let operator = Token::Operator(Operator::Or);
                let parse_result = parse_ands(tokens, current_index + 1)?;
                let right = parse_result.0;
                current_index = parse_result.1;

                ret_left = Expression::Binary(Box::new(ret_left), operator, Box::new(right));
            },
            _ => { break 'outer; },

        }
    }

    return Ok((ret_left, current_index));
}

fn parse_ands(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    let (mut ret_left, mut current_index) = parse_eqs(tokens, index)?;

    'outer: loop {
        let current_token = tokens.get(current_index);
        match current_token.map(|t| &t.token) {
            Some(Token::Operator(Operator::And)) => {
                let operator = Token::Operator(Operator::And);
                let parse_result = parse_eqs(tokens, current_index + 1)?;
                let right = parse_result.0;
                current_index = parse_result.1;

                ret_left = Expression::Binary(Box::new(ret_left), operator, Box::new(right));
            },
            _ => { break 'outer; },
        }
    }

    return Ok((ret_left, current_index));
}

fn parse_eqs(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    let (mut ret_left, mut current_index) = prase_comparisons(tokens, index)?;

    'outer: loop {
        let current_token = tokens.get(current_index);
        if let Some(Token::Operator(Operator::Eq | Operator::Neq)) = current_token.map(|t| &t.token) {
            let operator = &current_token.expect("Unr!").token;
            let parse_result = prase_comparisons(tokens, current_index + 1)?;
            let right = parse_result.0;
            current_index = parse_result.1;

            ret_left = Expression::Binary(Box::new(ret_left), operator.clone(), Box::new(right));
        } else {
            break 'outer;
        }
    }
    return Ok((ret_left, current_index));
}

fn prase_comparisons(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    // scan tokens at lower level, RETURNING when seeing Eq or Neq operator
    let (mut ret_left, mut current_index) = prase_terms(tokens, index)?;

    // if seeing those operators
    use Operator::*;
    'outer: loop {
        let current_token = tokens.get(current_index);

        if let Some(Token::Operator(Greater | GreaterEq | Lesser | LesserEq))
            = current_token.map(|t| &t.token)
        {
            let operator = &current_token.expect("Unr!").token;
            // scan again at lower levels, RETURNING when seeing Ge, Geq, Le, Leq operator
            let parse_res = prase_terms(tokens, current_index + 1)?;
            let right = parse_res.0;
            current_index = parse_res.1;

            ret_left = Expression::Binary(Box::new(ret_left), operator.clone(), Box::new(right));
        } else {
            break 'outer;
        }
    }
    return Ok((ret_left, current_index));
}

fn prase_terms(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    let (mut ret_left, mut current_index) = parse_factors(tokens, index)?;

    use Operator::*;
    'outer: loop {
        let current_token = tokens.get(current_index);
        // if seeing those operators
        if let Some(Token::Operator(Plus | Minus)) = current_token.map(|t| &t.token) {
            let operator = &current_token.expect("Unr!").token;
            // scan again at lower levels, RETURNING when seeing Multip or Div operator
            let parse_res = parse_factors(tokens, current_index + 1)?;

            let right = parse_res.0;
            current_index = parse_res.1;

            ret_left = Expression::Binary(Box::new(ret_left), operator.clone(), Box::new(right));
        } else {
            break 'outer;
        }
    }
    return Ok((ret_left, current_index));
}

fn parse_factors(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    let (mut ret_left, mut current_index) = prase_unary(tokens, index)?;

    use Operator::*;
    'outer: loop {
        let current_token = tokens.get(current_index);
        // if seeing those operators
        if let Some(Token::Operator(Multip | Div | Shl | Shr)) = current_token.map(|t| &t.token) {
            let operator = &current_token.expect("Unr!").token;
            // scan again at lower levels, RETURNING when seeing Multip or Div operator
            let parse_res = prase_unary(tokens, current_index + 1)?;

            let right = parse_res.0;
            current_index = parse_res.1;

            ret_left = Expression::Binary(Box::new(ret_left), operator.clone(), Box::new(right));
        } else {
            break 'outer;
        }
    }
    return Ok((ret_left, current_index));
}

fn prase_unary(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    let current_token = tokens.get(index);
    // if seeing those operators
    use Operator::*;
    if let Some(Token::Operator(Not | Minus)) = current_token.map(|t| &t.token) {
        let operator = &current_token.expect("Unr!").token;
        // scan again at lower levels, RETURNING when seeing ! or - operator
        let parse_res = prase_unary(tokens, index + 1)?;

        let right = parse_res.0;
        let current_index = parse_res.1;

        return Ok((Expression::Unary(operator.clone(), Box::new(right)), current_index))
    } else {
        return prase_primary(tokens, index);
    }
}

fn prase_primary(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    let current_token = tokens.get(index);
    match current_token.map(|t| &t.token) {
        Some(Token::BooleanLiteral(_))
        | Some(Token::FloatLiteral(_))
        | Some(Token::IntegerLiteral(_))
        | Some(Token::StringLiteral(_)) => {
            let token = &current_token.expect("Unr!").token;
            return Ok((Expression::Literal(token.clone()), index + 1))
        },
        Some(Token::Operator(Operator::OpParenth)) => {
            let (expr, n_index) = start_parse(tokens, index + 1)?;

            let after_token = tokens.get(n_index);

            match after_token.map(|t| &t.token) {
                Some(Token::Operator(Operator::CloParenth)) => { // Grouping
                    return Ok((Expression::Grouping(Box::new(expr)), n_index + 1));
                },
                Some(Token::Operator(Operator::Separator)) => { // Tuples
                    let mut next_index = n_index + 1;
                    let mut tuple_vals = vec![expr];
                    return loop {
                        let (parsed_expr, ni) = start_parse(tokens, next_index)?;
                        tuple_vals.push(parsed_expr);
                        next_index = ni;

                        match tokens.get(next_index).map(|t| &t.token) {
                            Some(Token::Operator(Operator::Separator)) => {
                                next_index = next_index + 1
                            },
                            Some(Token::Operator(Operator::CloParenth)) => {
                                break Ok((Expression::Tuple(tuple_vals), next_index + 1));
                            },
                            _ => {
                                break Err(lex::create_error_msg(
                                    "Expected ')' that was never found. ".to_owned(),
                                    Some(current_token.unwrap()), None, None
                                ));
                            }
                        };
                    }
                },
                _ => {
                    return Err(lex::create_error_msg(
                        "Expected ')' that was never found. ".to_owned(),
                        Some(current_token.unwrap()), None, None
                    ));
                }
            }
        },
        None | Some(_) => return Err(lex::create_error_msg(
        "Unexpected token: Expected Exprssion.".to_owned(),
            current_token, None, None)),
    }
}

fn find_next_start(tokens: &Vec<TokenInfo>, index: usize) -> usize {
    tokens.as_slice()
        .iter()
        .enumerate()
        .skip(index)
        .find(|(_, token)| match token.token {
            Token::Eof | Token::Keyword(_) | Token::Operator(Operator::Semicolon) => true,
            _ => false
        })
        .map(|(i, _)| i + 1)
        .unwrap_or(tokens.len() + 1)
}

mod tests {
    #![allow(unused_imports)]
    use crate::{lex::{TokenInfo, Place, Token, Keyword, Operator, self}, Expression, parse::{start_parse, find_next_start}};



#[test]
fn should_parse_literals_lvl_grouped_ungrouped() {
    let place = Place::new((0, 0));
    let tokens = vec![
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
    ];

    let res = start_parse(&tokens, 0);

    assert_eq!(res, Ok((Expression::Literal(Token::BooleanLiteral(true)), 1)), "Ungrouped");

    let tokens = vec![
        TokenInfo::new(Token::Operator(Operator::OpParenth), place.clone()),
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
        TokenInfo::new(Token::Operator(Operator::CloParenth), place.clone()),
    ];

    let res = start_parse(&tokens, 0);

    assert_eq!(res, Ok((Expression::Grouping(Box::new(Expression::Literal(Token::BooleanLiteral(true)))), 3)), "Grouped");
}

#[test]
fn should_parse_unary_operator_lvl_grouped_ungrouped() {
    let place = Place::new((0, 0));
    let tokens = vec![
        TokenInfo::new(Token::Operator(Operator::Not), place.clone()),
        TokenInfo::new(Token::FloatLiteral(1.0), place.clone()),
    ];

    let res = start_parse(&tokens, 0);

    assert_eq!(res, Ok((Expression::Unary(
        Token::Operator(Operator::Not), Box::new(Expression::Literal(Token::FloatLiteral(1.0)))), 2)), "Ungrouped");

    let tokens = vec![
        TokenInfo::new(Token::Operator(Operator::Minus), place.clone()),
        TokenInfo::new(Token::Operator(Operator::OpParenth), place.clone()),
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
        TokenInfo::new(Token::Operator(Operator::CloParenth), place.clone()),
    ];

    let res = start_parse(&tokens, 0);

    assert_eq!(res, Ok(
        (Expression::Unary(
            Token::Operator(Operator::Minus),
            Box::new(Expression::Grouping(Box::new(Expression::Literal(Token::BooleanLiteral(true)))))),
        4)
        ), "Grouped");
}

#[test]
fn should_parse_factor_lvl_grouped_ungrouped() {
    let place = Place::new((0, 0));
    let tokens = vec![
        TokenInfo::new(Token::FloatLiteral(1.0), place.clone()),
        TokenInfo::new(Token::Operator(Operator::Multip), place.clone()),
        TokenInfo::new(Token::FloatLiteral(1.0), place.clone()),
    ];

    let res = start_parse(&tokens, 0);

    assert_eq!(res, Ok((Expression::Binary(Box::new(Expression::Literal(Token::FloatLiteral(1.0))),
        Token::Operator(Operator::Multip), Box::new(Expression::Literal(Token::FloatLiteral(1.0)))), 3)), "Ungrouped");

    let expr = "!(-1 * 2) / 3".to_owned();
    let lex_res = lex::lex_content(expr).unwrap();
    assert!(lex_res.len() == 1);
    for (r, _) in lex_res {
        let res = start_parse(&r, 0);
        use Operator::*;
        assert_eq!(res, Ok((
            Expression::Binary(
                Box::new(Expression::Unary(
                    Token::Operator(Not),
                    Box::new(Expression::Grouping(
                        Box::new(Expression::Binary(
                            Box::new(Expression::Unary(Token::Operator(Minus), Box::new(Expression::Literal(Token::IntegerLiteral(1))))),
                            Token::Operator(Multip),
                            Box::new(Expression::Literal(Token::IntegerLiteral(2)))))
                    ))
                )),
                Token::Operator(Div),
                Box::new(Expression::Literal(Token::IntegerLiteral(3)))
            ),
            9))
        , "Grouped and nested")
    }
    //

    // assert_eq!(res, Ok((Expression::Grouping(Box::new(Expression::Literal(Token::BooleanLiteral(true)))), 3)), "Grouped");
}

#[test]
fn should_parse_tuples() {

    let expr = "(1, true, !(false || true), (1, 2), 1 + 2)\n(1, (2), ((2, 3)))".to_owned();

    use Expression::*;
    let expected = vec![
        Ok((
            Tuple(vec![
                Literal(Token::IntegerLiteral(1)),
                Literal(Token::BooleanLiteral(true)),
                Unary(
                    Token::Operator(Operator::Not),
                    Box::new(Grouping(Box::new(Binary(
                        Box::new(Literal(Token::BooleanLiteral(false))),
                        Token::Operator(Operator::Or),
                        Box::new(Literal(Token::BooleanLiteral(true)))
                    ))))
                ),
                Tuple(vec![Literal(Token::IntegerLiteral(1)), Literal(Token::IntegerLiteral(2))]),
                Binary(
                    Box::new(Literal(Token::IntegerLiteral(1))),
                    Token::Operator(Operator::Plus),
                    Box::new(Literal(Token::IntegerLiteral(2)))
                )
            ]),
        22)),
        Ok((
            Tuple(vec![
                Literal(Token::IntegerLiteral(1)),
                Grouping(Box::new(Literal(Token::IntegerLiteral(2)))),
                Grouping(Box::new(Tuple(vec![Literal(Token::IntegerLiteral(2)), Literal(Token::IntegerLiteral(3))]))),
            ]),
        15))
    ];

    let lex_res = lex::lex_content(expr).unwrap();
    assert!(lex_res.len() == expected.len());
    for ((r, _), exp) in lex_res.into_iter().zip(expected.into_iter()) {
        let res = start_parse(&r, 0);
        assert_eq!(res, exp, "Grouped and nested");
    }
}

#[test]
fn should_parse_addition_lvl_grouped_ungrouped() {
    let place = Place::new((0, 0));
    let tokens = vec![
        TokenInfo::new(Token::FloatLiteral(1.0), place.clone()),
        TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
        TokenInfo::new(Token::FloatLiteral(1.0), place.clone()),
    ];

    let res = start_parse(&tokens, 0);

    assert_eq!(res, Ok((Expression::Binary(Box::new(Expression::Literal(Token::FloatLiteral(1.0))),
        Token::Operator(Operator::Plus), Box::new(Expression::Literal(Token::FloatLiteral(1.0)))), 3)), "Ungrouped");

    let expr = "!(-1 * 2 - 3) / 3 + 1".to_owned();
    let lex_res = lex::lex_content(expr).unwrap();
    assert!(lex_res.len() == 1);
    for (r, _) in lex_res {
        let res = start_parse(&r, 0);
        use Operator::*;
        assert_eq!(res, Ok((
            Expression::Binary(
                Box::new(Expression::Binary(
                    Box::new(Expression::Unary(
                        Token::Operator(Not),
                        Box::new(Expression::Grouping(Box::new(Expression::Binary(
                            Box::new(Expression::Binary(
                                Box::new(Expression::Unary(Token::Operator(Minus), Box::new(Expression::Literal(Token::IntegerLiteral(1))))),
                                Token::Operator(Multip),
                                Box::new(Expression::Literal(Token::IntegerLiteral(2))))),
                            Token::Operator(Minus),
                            Box::new(Expression::Literal(Token::IntegerLiteral(3)))
                        ))))
                    )),
                    Token::Operator(Div),
                    Box::new(Expression::Literal(Token::IntegerLiteral(3)))
                )),
                Token::Operator(Plus),
                Box::new(Expression::Literal(Token::IntegerLiteral(1)))
            ),
            13))
        , "Grouped and nested")
    }
}

#[test]
fn should_parse_comparison_lvl_grouped_ungrouped() {
    let place = Place::new((0, 0));
    let tokens = vec![
        TokenInfo::new(Token::FloatLiteral(1.0), place.clone()),
        TokenInfo::new(Token::Operator(Operator::GreaterEq), place.clone()),
        TokenInfo::new(Token::FloatLiteral(1.0), place.clone()),
    ];

    let res = start_parse(&tokens, 0);

    assert_eq!(res, Ok((Expression::Binary(Box::new(Expression::Literal(Token::FloatLiteral(1.0))),
        Token::Operator(Operator::GreaterEq), Box::new(Expression::Literal(Token::FloatLiteral(1.0)))), 3)), "Ungrouped");

    let expr = "!(-1 * 2 - 3) / 3 + 1 > (0 <= 3 < 1) ".to_owned();
    let lex_res = lex::lex_content(expr).unwrap();
    assert!(lex_res.len() == 1);
    for (r, _) in lex_res {
        let res = start_parse(&r, 0);
        use Operator::*;
        assert_eq!(res, Ok((
            Expression::Binary(
                Box::new(Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Unary(
                            Token::Operator(Not),
                            Box::new(Expression::Grouping(Box::new(Expression::Binary(
                                Box::new(Expression::Binary(
                                    Box::new(Expression::Unary(Token::Operator(Minus), Box::new(Expression::Literal(Token::IntegerLiteral(1))))),
                                    Token::Operator(Multip),
                                    Box::new(Expression::Literal(Token::IntegerLiteral(2))))),
                                Token::Operator(Minus),
                                Box::new(Expression::Literal(Token::IntegerLiteral(3)))
                            ))))
                        )),
                        Token::Operator(Div),
                        Box::new(Expression::Literal(Token::IntegerLiteral(3)))
                    )),
                    Token::Operator(Plus),
                    Box::new(Expression::Literal(Token::IntegerLiteral(1)))
                )),
                Token::Operator(Greater),
                Box::new(Expression::Grouping(Box::new(Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Literal(Token::IntegerLiteral(0))),
                        Token::Operator(LesserEq),
                        Box::new(Expression::Literal(Token::IntegerLiteral(3)))
                    )),
                    Token::Operator(Lesser),
                    Box::new(Expression::Literal(Token::IntegerLiteral(1)))
                ))))
            ),
            21))
        , "Grouped and nested")
    }
}

#[test]
fn should_parse_eq_neq_lvl_grouped_ungrouped() {
    let place = Place::new((0, 0));
    let tokens = vec![
        TokenInfo::new(Token::FloatLiteral(1.0), place.clone()),
        TokenInfo::new(Token::Operator(Operator::Neq), place.clone()),
        TokenInfo::new(Token::FloatLiteral(1.0), place.clone()),
    ];

    let res = start_parse(&tokens, 0);

    assert_eq!(res, Ok((Expression::Binary(Box::new(Expression::Literal(Token::FloatLiteral(1.0))),
        Token::Operator(Operator::Neq), Box::new(Expression::Literal(Token::FloatLiteral(1.0)))), 3)), "Ungrouped");

    let expr = "!(-1 * 2 - 3) / 3 + 1 == (0 <= 3 < 1 != 13 * 14) ".to_owned();
    let lex_res = lex::lex_content(expr).unwrap();
    assert!(lex_res.len() == 1);
    for (r, _) in lex_res {
        let res = start_parse(&r, 0);
        use Operator::*;
        assert_eq!(res, Ok((
            Expression::Binary(
                Box::new(Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Unary(
                            Token::Operator(Not),
                            Box::new(Expression::Grouping(Box::new(Expression::Binary(
                                Box::new(Expression::Binary(
                                    Box::new(Expression::Unary(Token::Operator(Minus), Box::new(Expression::Literal(Token::IntegerLiteral(1))))),
                                    Token::Operator(Multip),
                                    Box::new(Expression::Literal(Token::IntegerLiteral(2))))),
                                Token::Operator(Minus),
                                Box::new(Expression::Literal(Token::IntegerLiteral(3)))
                            ))))
                        )),
                        Token::Operator(Div),
                        Box::new(Expression::Literal(Token::IntegerLiteral(3)))
                    )),
                    Token::Operator(Plus),
                    Box::new(Expression::Literal(Token::IntegerLiteral(1)))
                )),
                Token::Operator(Eq),
                Box::new(Expression::Grouping(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Binary(
                            Box::new(Expression::Binary(
                                Box::new(Expression::Literal(Token::IntegerLiteral(0))),
                                Token::Operator(LesserEq),
                                Box::new(Expression::Literal(Token::IntegerLiteral(3)))
                            )),
                            Token::Operator(Lesser),
                            Box::new(Expression::Literal(Token::IntegerLiteral(1)))
                        )),
                        Token::Operator(Neq),
                        Box::new(Expression::Binary(
                            Box::new(Expression::Literal(Token::IntegerLiteral(13))),
                            Token::Operator(Multip),
                            Box::new(Expression::Literal(Token::IntegerLiteral(14)))
                        ))
                    )
                )))
            ),
            25))
        , "Grouped and nested")
    }
}

#[test]
fn should_parse_ors_ands_lvl_grouped_ungrouped() {
    let place = Place::new((0, 0));
    let tokens = vec![
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
        TokenInfo::new(Token::Operator(Operator::And), place.clone()),
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
    ];

    let res = start_parse(&tokens, 0);

    assert_eq!(res, Ok((Expression::Binary(Box::new(Expression::Literal(Token::BooleanLiteral(true))),
        Token::Operator(Operator::And), Box::new(Expression::Literal(Token::BooleanLiteral(true)))), 3)), "Ungrouped");

    let expr = "!(true || false && true) || true && false ".to_owned();
    let lex_res = lex::lex_content(expr).unwrap();
    assert!(lex_res.len() == 1);
    for (r, _) in lex_res {
        let res = start_parse(&r, 0);
        use Expression::*;
        use Token::*;
        assert_eq!(res, Ok((Binary(
            Box::new(Unary(
                Operator(lex::Operator::Not),
                Box::new(Grouping(Box::new(Binary(
                    Box::new(Literal(BooleanLiteral(true))),
                    Operator(lex::Operator::Or),
                    Box::new(Binary(
                        Box::new(Literal(BooleanLiteral(false))),
                        Operator(lex::Operator::And),
                        Box::new(Literal(BooleanLiteral(true)))
                    ))
                ))))
            )),
            Operator(lex::Operator::Or),
            Box::new(Binary(
                Box::new(Literal(BooleanLiteral(true))),
                Operator(lex::Operator::And),
                Box::new(Literal(BooleanLiteral(false)))
            ))
        ), 12)))
    }
}

#[test]
fn should_parse_conditional_expression_grouped_ungrouped() {
    let place = Place::new((0, 0));
    let tokens = vec![
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
        TokenInfo::new(Token::Operator(lex::Operator::IfExpr), place.clone()),
        TokenInfo::new(Token::BooleanLiteral(false), place.clone()),
        TokenInfo::new(Token::Operator(lex::Operator::ElseExpr), place.clone()),
        TokenInfo::new(Token::BooleanLiteral(false), place.clone()),
    ];

    let res = start_parse(&tokens, 0);

    use Expression::*;
    use Token::*;
    assert_eq!(res, Ok((Conditional(Box::new(Literal(BooleanLiteral(true))),
        Box::new(Literal(BooleanLiteral(false))), Box::new(Literal(BooleanLiteral(false)))), 5)), "Ungrouped");

    let expr = "!(true || false && true) ? 1 + 2 * 3 : 1 < 2 == false".to_owned();
    let lex_res = lex::lex_content(expr).unwrap();
    assert!(lex_res.len() == 1);
    for (r, _) in lex_res {
        let res = start_parse(&r, 0);
        assert_eq!(res, Ok((
            Conditional(
                Box::new(Unary(
                    Operator(lex::Operator::Not),
                    Box::new(Grouping(Box::new(Binary(
                        Box::new(Literal(BooleanLiteral(true))),
                        Operator(lex::Operator::Or),
                        Box::new(Binary(
                            Box::new(Literal(BooleanLiteral(false))),
                            Operator(lex::Operator::And),
                            Box::new(Literal(BooleanLiteral(true)))
                        ))
                    ))))
                )),
                Box::new(Binary(
                    Box::new(Literal(IntegerLiteral(1))),
                    Operator(lex::Operator::Plus),
                    Box::new(Binary(
                        Box::new(Literal(IntegerLiteral(2))),
                        Operator(lex::Operator::Multip),
                        Box::new(Literal(IntegerLiteral(3)))
                    ))
                )),
                Box::new(Binary(
                    Box::new(Binary(
                        Box::new(Literal(IntegerLiteral(1))),
                        Operator(lex::Operator::Lesser),
                        Box::new(Literal(IntegerLiteral(2)))
                    )),
                    Operator(lex::Operator::Eq),
                    Box::new(Literal(BooleanLiteral(false)))
                )),
            )
        , 20)))
    }
}

#[test]
fn should_parse_nested_conditional_expressions() {
    let expr = "1 ? (true ? 0 : 2) : 1 == 2 ? 200 : 400".to_owned();
    let lex_res = lex::lex_content(expr).unwrap();
    assert!(lex_res.len() == 1);
    use Expression::*;
    use Token::*;
    for (r, _) in lex_res {
        let res = start_parse(&r, 0);
        assert_eq!(res, Ok((
            Conditional(
                Box::new(Literal(IntegerLiteral(1))),
                Box::new(Grouping(Box::new(Conditional(
                    Box::new(Literal(BooleanLiteral(true))),
                    Box::new(Literal(IntegerLiteral(0))),
                    Box::new(Literal(IntegerLiteral(2))),
                )))),
                Box::new(Conditional(
                    Box::new(Binary(Box::new(Literal(IntegerLiteral(1))), Operator(lex::Operator::Eq), Box::new(Literal(IntegerLiteral(2))))),
                    Box::new(Literal(IntegerLiteral(200))),
                    Box::new(Literal(IntegerLiteral(400))),
                )),
            )
        , 17)))
    }
}

#[test]
fn sohuld_find_next_expression_start() {
    let place = Place::new((0, 0));
    let tokens = vec![
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
        TokenInfo::new(Token::Keyword(Keyword::Let), place.clone()),
        TokenInfo::new(Token::Operator(Operator::Semicolon), place.clone()),
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
        TokenInfo::new(Token::BooleanLiteral(true), place.clone()),
        TokenInfo::new(Token::Eof, place.clone()),
    ];

    assert_eq!(4, find_next_start(&tokens, 0));
    assert_eq!(5, find_next_start(&tokens, 4));
    assert_eq!(None, tokens.get(find_next_start(&tokens, 9)));
}

// #[test]
// fn sohuld_parse_expression_and_expect_panic() {
//     let place = Place::new((0, 0));
//     let tokens = vec![
//         // TokenInfo::new(Token::IntegerLiteral(1), place.clone()),
//         // TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
//         // TokenInfo::new(Token::IntegerLiteral(2), place.clone()),
//         // TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
//         TokenInfo::new(Token::IntegerLiteral(3), place.clone()),
//         TokenInfo::new(Token::Keyword(Keyword::Let), place.clone()),
//         TokenInfo::new(Token::IntegerLiteral(4), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
//         TokenInfo::new(Token::IntegerLiteral(4), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::Semicolon), place.clone()),
//         // line 1: 1 + 2 + 3 let + 4; 13 + 13
//         TokenInfo::new(Token::IntegerLiteral(13), place.clone()),
//         TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
//         TokenInfo::new(Token::IntegerLiteral(13), place.clone()),
//     ];

//     let (res_e, ind, s) = prase_expression(tokens);
//     dbg!(&res_e, ind, &s);
//     assert_eq!(
//         res_e,
//         vec![Expression::Binary(
//             Box::new(Expression::Literal(Token::IntegerLiteral(13))),
//             Token::Operator(Operator::Plus),
//             Box::new(Expression::Literal(Token::IntegerLiteral(13)))
//         )]
//     );
//     assert_eq!(ind, 12);
//     assert_eq!(s.len(), 2);

// }


}
