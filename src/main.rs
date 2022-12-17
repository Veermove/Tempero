mod lex;
mod parse;
mod r#type;

use std::{fs::File, io::Read};

use lex::*;
use parse::*;
use r#type::*;

fn main() {
    loop {
        
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

