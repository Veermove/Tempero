mod lex;

use std::{fs::File, io::Read};

use lex::*;

fn main() {
    let place = Place::new((0, 0));
    let tokens = vec![
        TokenInfo::new(Token::IntegerLiteral(1), place.clone()),
        TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
        TokenInfo::new(Token::IntegerLiteral(2), place.clone()),
        TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
        TokenInfo::new(Token::IntegerLiteral(3), place.clone()),
        TokenInfo::new(Token::Keyword(Keyword::Let), place.clone()),
        // line 1: 1 + 2 + 3 let 13 + 13
        TokenInfo::new(Token::IntegerLiteral(13), place.clone()),
        TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
        TokenInfo::new(Token::IntegerLiteral(13), place.clone()),
    ];

    let a = prase_expression(tokens);

    dbg!(a);
}


fn main2() -> std::io::Result<()> {
    let mut file = File::open("./add_ex.tm")?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    // lex_file()
    // println!("{}", content);
    // let c = lex_content(content).expect("msg");
    // for (tok, s) in c.iter() {
    //     for z in tok.iter() {
    //         println!("{:?}", z);
    //     } 
    // }
    // Ok(())

    if let Ok(tokens) = lex::lex_content(content) {
        for (line_toks, line) in tokens {
            if let Ok((_, i)) = prase_expression(line_toks) {
                // dbg!("{}", exp);
                println!("{}", i)
            }
        }
    }
    Ok(())
}

fn prase_expression(expression_tokens: Vec<TokenInfo>) -> Result<(Vec<Expression>, usize), String> {
    let mut exps = Vec::new();
    let mut st_index = 0;
    while st_index <= expression_tokens.len() {
        let re = prase_eqs(&expression_tokens, st_index);
        if let Ok((exp, ind)) = re {
            if exp == Expression::Eoe {
                break;
            }
            exps.push(exp);
            // dbg!(&exps);
            st_index = ind;
        } else if let Err(msg) = re {
            println!("{}", msg);
            st_index = find_next_start(&expression_tokens, st_index);
        }
    }
    return Ok((exps, st_index));
}

fn prase_eqs(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    // scan tokens at lower level, RETURNING when seeing Eq or Neq operator
    let res = prase_comparisons(tokens, index);
        // .and(res)
        // .or(prase_comparisons(&tokens, find_next_start(&tokens, index)));
    
    if res.is_err() {
        return res; 
    }
    let (mut ret_left, mut current_index) = res.expect("Unr!");
    // if seeing those operators
    'outer: loop {
        let current_token = tokens.get(current_index);
        if let Some(Token::Operator(Operator::Eq | Operator::Neq)) = current_token.map(|t| &t.token) {
            
            let operator = &current_token.expect("Unr!").token;
            // scan again at lower levels, RETURNING when seeing Eq or Neq operator 
            
            let parse_result = prase_comparisons(tokens, current_index + 1);
            if let Ok(parse_res) =  parse_result {
                let right = parse_res.0;
                current_index = parse_res.1;

                ret_left = Expression::Binary(Box::new(ret_left), operator.clone(), Box::new(right));
            } else {
                // if result is Err, return
                return parse_result;
            }
        } else {
            break 'outer;
        }
    }
    return Ok((ret_left, current_index));
}

fn prase_comparisons(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    // scan tokens at lower level, RETURNING when seeing Eq or Neq operator
    let res = prase_terms(tokens, index);
    if res.is_err() {
        return res;
    }
    let (mut ret_left, mut current_index) = res.expect("Unr!");

    // if seeing those operators
    use Operator::*;
    'outer: loop {
        let current_token = tokens.get(current_index);

        if let Some(Token::Operator(Greater | GreaterEq | Lesser | LesserEq)) 
            = current_token.map(|t| &t.token) 
        {
            let operator = &current_token.expect("Unr!").token;
            // scan again at lower levels, RETURNING when seeing Ge, Geq, Le, Leq operator 
            let parse_res = prase_terms(tokens, current_index + 1);
            if let Ok(parse_res) = parse_res {
                let right = parse_res.0;
                current_index = parse_res.1;

                ret_left = Expression::Binary(Box::new(ret_left), operator.clone(), Box::new(right));
            }
        } else {
            break 'outer;
        }
    }
    return Ok((ret_left, current_index));
}

fn prase_terms(tokens: &Vec<TokenInfo>, index: usize) -> Result<(Expression, usize), String> {
    let res = parse_factors(tokens, index);
    if res.is_err() {
        return res;
    }

    let (mut ret_left, mut current_index) = res.expect("Unr!");

    use Operator::*;
    'outer: loop {
        let current_token = tokens.get(current_index);
        // if seeing those operators
        if let Some(Token::Operator(Plus | Minus)) = current_token.map(|t| &t.token) {
            let operator = &current_token.expect("Unr!").token;
            // scan again at lower levels, RETURNING when seeing Multip or Div operator 
            let parse_res = parse_factors(tokens, current_index + 1);
            if parse_res.is_err() {
                return parse_res;
            }
            let parse_res = parse_res.expect("Unr!");

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
    let res = prase_unary(tokens, index);
    if res.is_err() {
        return res;
    }

    let (mut ret_left, mut current_index) = res.expect("Unr!");

    use Operator::*;
    'outer: loop {
        let current_token = tokens.get(current_index);
        // if seeing those operators
        if let Some(Token::Operator(Multip | Div)) = current_token.map(|t| &t.token) {
            let operator = &current_token.expect("Unr!").token;
            // scan again at lower levels, RETURNING when seeing Multip or Div operator 
            let parse_res = prase_unary(tokens, current_index + 1)
                .unwrap();

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
        let parse_res = prase_unary(tokens, index + 1);
        if parse_res.is_err() {
            return parse_res;
        }
        let parse_res = parse_res.expect("Unr!");

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
        }
        Some(Token::Operator(Operator::OpParenth)) => {
            let res = prase_eqs(tokens, index + 1);
            if res.is_err() {
                return res;
            }
            let (expr, n_index) = res.expect("Unr!");

            let after_token = tokens.get(n_index);

            if let Some(Token::Operator(Operator::CloParenth)) = after_token.map(|t| &t.token) {
                return Ok((Expression::Grouping(Box::new(expr)), n_index + 1));
            } else {
                lex::report_error(
                    "Expected ')' that was never found. ".to_owned(), 
                    Some(current_token.unwrap()), None, None
                );
                return Err("Unfinished parentheses".to_owned());
            }
        }
        None => return Ok((Expression::Eoe, index)),
        _    => return Err("Expected expression".to_string()),
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

#[derive(Debug, PartialEq)]
enum Expression {
    Literal(Token),
    Grouping(Box<Expression>),
    Unary(Token, Box<Expression>),
    Binary(Box<Expression>, Token, Box<Expression>),
    Eoe,
}

mod tests {
    use crate::{lex::{TokenInfo, Place, Token, Keyword, Operator}, find_next_start, prase_expression, Expression};


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


#[test]
fn sohuld_parse_expression_and_expect_panic() {
    let place = Place::new((0, 0));
    let tokens = vec![
        TokenInfo::new(Token::IntegerLiteral(1), place.clone()),
        TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
        TokenInfo::new(Token::IntegerLiteral(2), place.clone()),
        TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
        TokenInfo::new(Token::IntegerLiteral(3), place.clone()),
        TokenInfo::new(Token::Keyword(Keyword::Let), place.clone()),
        TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
        TokenInfo::new(Token::IntegerLiteral(4), place.clone()),
        TokenInfo::new(Token::Operator(Operator::Semicolon), place.clone()),
        // line 1: 1 + 1 + 1 let + 1; 
        // line 2: 1 + 1;
        TokenInfo::new(Token::IntegerLiteral(13), place.clone()),
        TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
        TokenInfo::new(Token::IntegerLiteral(13), place.clone()),
    ];

    let (res_e, ind) = prase_expression(tokens).unwrap();
    // assert_eq!(
    //     res_e, 
    //     Expression::Binary(
    //         Box::new(Expression::Literal(Token::IntegerLiteral(13))), 
    //         Token::Operator(Operator::Plus), 
    //         Box::new(Expression::Literal(Token::IntegerLiteral(13)))
    //     )
    // );

    assert_eq!(ind, 11);
}


}
