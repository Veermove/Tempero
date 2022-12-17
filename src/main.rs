mod lex;
mod parse;
mod typedExpr;
mod eval;

use std::{fs::File, io::{self, Read}};



use lex::*;
use parse::*;
use typedExpr::*;

fn main() {
    let mut buffer = String::new();
    print!(">");
    while let Ok(_) = io::stdin().read_line(&mut buffer) {
        print!(">");
        let tokens = lex_content_simple(buffer.drain(0..).collect());
        if let Ok(cont) = tokens {
            let (asts, _, errors) = parse::prase_expressions(cont);
            for t in &asts {
                dbg!(&t);
            }
            assert!(errors.is_empty());
            let asts = asts.into_iter()
                .map(typedExpr::TExpression::new)
                .map(|r| r.unwrap())
                .collect::<Vec<TExpression>>();

            for t in asts {
                dbg!(&t);
            }
        }

        buffer.drain(0..);
    }
}

fn main1() {
    // let place = Place::new((0, 0));
    // let tokens = vec![
    //     TokenInfo::new(Token::IntegerLiteral(1), place.clone()),
    //     TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
    //     TokenInfo::new(Token::IntegerLiteral(2), place.clone()),
    //     TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
    //     TokenInfo::new(Token::IntegerLiteral(3), place.clone()),
    //     TokenInfo::new(Token::Keyword(Keyword::Let), place.clone()),
    //     TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
    //     TokenInfo::new(Token::IntegerLiteral(4), place.clone()),
    //     TokenInfo::new(Token::Operator(Operator::Semicolon), place.clone()),
    //     // line 1: 1 + 1 + 1 let + 1;
    //     // line 2: 1 + 1;
    //     TokenInfo::new(Token::IntegerLiteral(13), place.clone()),
    //     TokenInfo::new(Token::Operator(Operator::Plus), place.clone()),
    //     TokenInfo::new(Token::IntegerLiteral(13), place.clone()),
    // ];

    // // let (res_e, ind, s) = prase_expression(tokens);
    // dbg!(&res_e, ind, &s);
    // assert_eq!(
    //     res_e,
    //     vec![Expression::Binary(
    //         Box::new(Expression::Literal(Token::IntegerLiteral(13))),
    //         Token::Operator(Operator::Plus),
    //         Box::new(Expression::Literal(Token::IntegerLiteral(13)))
    //     )]
    // );
    // assert_eq!(ind, 12);
    // assert_eq!(s.len(), 2);
}


fn main2() -> std::io::Result<()> {
    // let mut file = File::open("./add_ex.tm")?;
    // let mut content = String::new();
    // file.read_to_string(&mut content)?;

    // let tokens = lex::lex_content(content);
    // let ast = parse::prase_expressions(expression_tokens)
    Ok(())
}
