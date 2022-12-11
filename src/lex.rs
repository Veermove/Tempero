#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    FloatLiteral(f64),
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    StringLiteral(String),

    Identifier(String),
    Keyword(Keyword),
    Operator(Operator),

    Eof,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Eq,         // ==
    Neq,        // != 
    Greater,    // >
    GreaterEq,  // >=
    Lesser,     // <
    LesserEq,   // <=
    Plus,       // +
    Minus,      // -
    Multip,     // *
    Div,        // /
    Semicolon,  // ;
    Not,        // !

    Assign,     //  =
    AssingInc,  // +=
    AssignDec,  // -=
    AssignMul,  // *=
    AssignDiv,  // /=

                // condition
    IfExpr,     // ? <expr if true>
    ElseExpr,   // : <expr if false>

    OpBrace,    // {
    CloBrace,   // }
    OpParenth,  // (
    CloParenth, // )
    
    Separator   // ,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Let,
    For,
    If,
    Else,
    Func,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Place {
    column: usize,
    line: usize,
    full_line: Option<String>,
}

impl Place {
    pub fn new(line_column: (usize, usize)) -> Self {
        Place { line: line_column.0, column: line_column.1, full_line: None }
    }

    pub fn new_full(line_column: (usize, usize), full_line: String) -> Self {
        Place { line: line_column.0, column: line_column.1, full_line: Some(full_line) }
    }
}


#[derive(Debug, PartialEq)]
pub struct TokenInfo {
    pub token: Token,
    pub place: Place
}

impl TokenInfo {
    pub fn new(token: Token, place: Place) -> Self {
        TokenInfo { token, place }
    }
}

pub fn lex_content(content: String) -> Result<Vec<(Vec<TokenInfo>, String)>, ()> {
    let mut lines: Vec<(Vec<TokenInfo>, String)> = Vec::new();
    let mut line_counter: usize = 1;
    let mut ml_comment = false; 
    for line in content.lines() {

        let mut char_itr = line.char_indices().peekable();
        let mut tokens = Vec::new();

        'over_line_loop: while let Some((index, current_char)) = char_itr.next() {
            let place = Place::new_full((line_counter, index + 1), line.to_owned());

            if ml_comment && current_char == '*' && char_itr.peek().is_some() && char_itr.peek().expect("Unr!").1 == '/' {
                char_itr.next().expect("Unr!");
                ml_comment = false;
                continue 'over_line_loop;
            } else if ml_comment {
                continue 'over_line_loop;
            }
            
            match current_char {
                '"' => {
                    let mut literal_buffor = Vec::new();

                    'str_literal_loop: loop {
                        if let Some((_, c)) = char_itr.peek() {
                            if *c == '"' {
                                char_itr.next().expect("Unr");
                                break 'str_literal_loop;
                            } else {
                                literal_buffor.push(char_itr.next().expect("Unr").1);
                            }
                        } else {
                            report_error("Unfinished string literal".to_owned(), None, Some(&place), Some(line.to_owned()));
                            break 'str_literal_loop;
                        }
                    }
                    
                    tokens.push(TokenInfo::new(Token::StringLiteral(literal_buffor.iter().collect()), place.clone()));
                },
                '0'..='9' => {
                    let mut literal_buffor = Vec::new();
                    literal_buffor.push(current_char);

                    'digit_loop: while let Some((_, c)) = char_itr.peek() {
                        if c.is_digit(10) || *c == '.' {
                            literal_buffor.push(char_itr.next().expect("Unr!").1)
                        } else {
                            break 'digit_loop;
                        }
                    }
                    
                    let mut t_itr = literal_buffor.split(|c| *c == '.');
                    match (t_itr.next(), t_itr.next(), t_itr.next()) {
                        (Some(ints), Some(decims), None) => {
                            let f_literal = ints.into_iter().chain(vec![&'.'].into_iter()).chain(decims.into_iter())
                                .collect::<String>()
                                .parse::<f64>();
                            if f_literal.is_ok() {
                                tokens.push(TokenInfo::new(Token::FloatLiteral(f_literal.expect("Unr!")), place));
                            } else {
                                report_error("Failed to prase float number literal".to_owned(), None, Some(&place), Some(line.to_owned()))
                            }
                            
                        },
                        (Some(ints), None, None) => {
                            let f_literal = ints.into_iter()
                                .collect::<String>()
                                .parse::<i64>();
                            if f_literal.is_ok() {
                                tokens.push(TokenInfo::new(Token::IntegerLiteral(f_literal.expect("Unr!")), place));
                            } else {
                                report_error("Failed to prase float number literal".to_owned(), None, Some(&place), Some(line.to_owned()))
                            };
                        }
                        _ => {
                            report_error("Failed to prase float number literal".to_owned(), None, Some(&place), Some(line.to_owned()));
                        }
                    }
                }
                'a'..='z' | 'A'..='Z' => {
                    let mut identifier_buffor = Vec::new();
                    identifier_buffor.push(current_char);

                    'identifier_loop: while let Some((_, c)) = char_itr.peek() {
                        if c.is_alphanumeric() || *c == '_'{
                            identifier_buffor.push(char_itr.next().expect("Unr").1);
                        } else {
                            break 'identifier_loop;
                        }
                    }
                    let idenf: String = identifier_buffor.into_iter().collect();

                    let token = match idenf.as_str() {
                        "let"   => Token::Keyword(Keyword::Let), 
                        "for"   => Token::Keyword(Keyword::For), 
                        "if"    => Token::Keyword(Keyword::If), 
                        "else"  => Token::Keyword(Keyword::Else), 
                        "func"  => Token::Keyword(Keyword::Func),
                        "true"  => Token::BooleanLiteral(true),
                        "false" => Token::BooleanLiteral(false),
                        _       => Token::Identifier(idenf), 
                    };
                    tokens.push(TokenInfo::new(token, place));
                }
                '<' => {
                    tokens.push(match  char_itr.peek() {
                        Some((_, '=')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::LesserEq), place)
                        },
                        _ => TokenInfo::new(Token::Operator(Operator::Lesser), place),
                    });
                },
                '>' => {
                        tokens.push(match char_itr.peek() {
                            Some((_, '=')) => {
                                let _ = char_itr.next().expect("Unr!");
                                TokenInfo::new(Token::Operator(Operator::GreaterEq), place)
                            },
                            _ => TokenInfo::new(Token::Operator(Operator::Greater), place),
                        }
                    );
                },
                '+' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, '=')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::AssingInc), place)
                        }
                        _ => TokenInfo::new(Token::Operator(Operator::Plus), place),
                    })
                },
                '-' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, '=')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::AssignDec), place)
                        }
                        _ => TokenInfo::new(Token::Operator(Operator::Minus), place),
                    })
                },
                '*' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, '=')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::AssignMul), place)
                        }
                        _ => TokenInfo::new(Token::Operator(Operator::Multip), place),
                    })
                },
                '/' => {
                    let nx = char_itr.peek();
                    if let Some((_, '*')) = nx {
                        let _ = char_itr.next().expect("Unr!");
                        ml_comment = true;
                    } else {
                        tokens.push(match  nx {
                            Some((_, '=')) => {
                                let _ = char_itr.next().expect("Unr!");
                                TokenInfo::new(Token::Operator(Operator::AssignDiv), place)
                            },
                            Some((_, '/')) => { // comments
                                break 'over_line_loop;
                            },
                            _ => TokenInfo::new(Token::Operator(Operator::Div), place),
                        })
                    }
                },
                '!' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, '=')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::Neq), place)
                        }
                        _ => TokenInfo::new(Token::Operator(Operator::Not), place),
                    })
                },
                ';' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::Semicolon), place));
                },
                '=' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, '=')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::Eq), place)
                        },
                        _ => TokenInfo::new(Token::Operator(Operator::Assign), place),
                    });
                }
                '?' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::IfExpr), place));
                },
                ':' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::ElseExpr), place));
                },
                '{' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::OpBrace), place));
                },
                '}' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::CloBrace), place));
                },
                '(' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::OpParenth), place));
                },
                ')' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::CloParenth), place));
                },
                ',' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::Separator), place));
                }
                ' ' => { },
                _  => unreachable!(),
            }
        }

        lines.push((tokens.drain(0..).collect(), line.to_owned()));
        line_counter += 1;

    }; 

    return Ok(lines);
}

pub fn report_error(error_message: String, token: Option<&TokenInfo>, place: Option<&Place>, error_line: Option<String>) {
    print!("[] Err: {}\n", error_message);
    if let Some(tok) = token {
        print!("[] Err: Compilation failed on: {:?}, at (line, column): ({}, {})\n", tok.token, tok.place.line, tok.place.column);
        if let Some(err_l) = &tok.place.full_line {
            print!("[] Err: Compilation failed on line: {}\n", err_l);
        }
    }
    if let Some(pl) = place {
        print!("[] Err: Compilation failed at (line, column): ({}, {})\n", pl.line, pl.column);
    }
    if let Some(er_l) = error_line {
        print!("[] Error line: {}\n", er_l);
    }
    // print!("[] Err: {}\n", );
}


mod tests {
    use crate::{*, lex::Operator};

    use super::{lex_content, TokenInfo, Token, Place};

#[test]
fn should_lex_string_literal() {
    assert_eq!(lex_content("\"ESSA\"".to_owned()), 
        Ok(vec![
            (vec![
                (TokenInfo::new(Token::StringLiteral("ESSA".to_owned()), Place::new_full((1, 1), "\"ESSA\"".to_owned())))
            ], "\"ESSA\"".to_owned())
        ])
    )
}

#[test]
fn should_parse_identifier() {
    assert_eq!(lex_content("thisIsMy_d".to_owned()), 
        Ok(vec![
            (vec![
                (TokenInfo::new(Token::Identifier("thisIsMy_d".to_owned()), Place::new_full((1, 1), "thisIsMy_d".to_owned())))
            ], "thisIsMy_d".to_owned())
        ])
    )
}

#[test]
fn should_parse_number_literals() {
    assert_eq!(lex_content("420.69 17".to_owned()), 
        Ok(vec![
            (vec![
                TokenInfo::new(Token::FloatLiteral(420.69), Place::new_full((1, 1), "420.69 17".to_owned())),
                TokenInfo::new(Token::IntegerLiteral(17), Place::new_full((1, 8), "420.69 17".to_owned()))
            ], "420.69 17".to_owned())
        ])
    )
}


#[test]
fn should_parse_boolean_literals() {
    assert_eq!(lex_content("true false".to_owned()), 
        Ok(vec![
            (vec![
                TokenInfo::new(Token::BooleanLiteral(true), Place::new_full((1, 1), "true false".to_owned())),
                TokenInfo::new(Token::BooleanLiteral(false), Place::new_full((1, 6), "true false".to_owned()))
            ], "true false".to_owned())
        ])
    )
}

#[test]
fn should_parse_operators() {
    let expected = vec![
            (vec![  
                TokenInfo::new(Token::Operator(Operator::Eq), Place::new_full((1, 1), "==".to_owned())),
            ], "==".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::Neq), Place::new_full((2, 1), "!=".to_owned())),
            ], "!=".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::Greater), Place::new_full((3, 1), ">".to_owned())),
            ], ">".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::GreaterEq), Place::new_full((4, 1), ">=".to_owned())),
            ], ">=".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::Lesser), Place::new_full((5, 1), "<".to_owned())),
            ], "<".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::LesserEq), Place::new_full((6, 1), "<=".to_owned())),
            ], "<=".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::Plus), Place::new_full((7, 1), "+".to_owned())),
            ], "+".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::Minus), Place::new_full((8, 1), "-".to_owned())),
            ], "-".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::Multip), Place::new_full((9, 1), "*".to_owned())),
            ], "*".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::Div), Place::new_full((10, 1), "/".to_owned())),
            ], "/".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::Semicolon), Place::new_full((11, 1), ";".to_owned())),
            ], ";".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::Not), Place::new_full((12, 1), "!".to_owned())),
            ], "!".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::Assign), Place::new_full((13, 1), "=".to_owned())),
            ], "=".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::AssingInc), Place::new_full((14, 1), "+=".to_owned())),
            ], "+=".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::AssignDec), Place::new_full((15, 1), "-=".to_owned())),
            ], "-=".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::AssignMul), Place::new_full((16, 1), "*=".to_owned())),
            ], "*=".to_owned()),
            (vec![
                TokenInfo::new(Token::Operator(Operator::AssignDiv), Place::new_full((17, 1), "/=".to_owned())),
            ], "/=".to_owned()),
        ];
    let res = lex_content("==\n!=\n>\n>=\n<\n<=\n+\n-\n*\n/\n;\n!\n=\n+=\n-=\n*=\n/=".to_owned());
    assert!(res.is_ok());
    for ((res_tok, res_line), (ex_tok, ex_line)) in res.expect("Unr").iter().zip(expected.iter()) {
        assert_eq!(res_tok, ex_tok);
        assert_eq!(res_line, ex_line);
    }
}

}
