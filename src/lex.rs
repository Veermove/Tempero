use std::fmt::Display;

use crate::types::{
    LineTokens, 
    Token, TokenInfo, Literal, Keyword, Operator};


pub fn to_simple_format(tokens: Vec<LineTokens>) -> Vec<(Token, usize, usize)> {
    tokens.into_iter()
        .map(|lt| (lt.tokens, lt.number))
        .flat_map(|(tokens, line)| tokens.into_iter()
            .map(move |ti| (ti.token, ti.column, line))
        )
        .collect()
}

pub fn lex_content_simple(content: String) -> Result<Vec<LineTokens>, String> {
    let (res, errs) = lex_content(content);
    return if errs.is_empty() {
        Ok(res)
    } else {
        Err(errs.join("\n"))
    }
}

pub fn lex_content(content: String) -> (Vec<LineTokens>, Vec<String>) {
    let mut lines = Vec::new();
    let mut errors = Vec::new();
    let mut line_counter: usize = 1;
    let mut ml_comment = false;
    for line in content.lines() {

        let mut char_itr = line.char_indices().peekable();
        let mut tokens = Vec::new();

        'over_line_loop: while let Some((index, current_char)) = char_itr.next() {

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
                            errors.push(report_error("Unfinished string literal".to_owned(), index + 1, line_counter));
                            break 'str_literal_loop;
                        }
                    }

                    tokens.push(TokenInfo::new(Token::Literal(Literal::String(literal_buffor.iter().collect())), index + 1));
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
                                tokens.push(TokenInfo::new(Token::Literal(Literal::Float(f_literal.expect("Unr!"))), index + 1));
                            } else {
                                errors.push(report_error("Failed to prase float number literal".to_owned(), index + 1, line_counter));
                            }

                        },
                        (Some(ints), None, None) => {
                            let f_literal = ints.into_iter()
                                .collect::<String>()
                                .parse::<i64>();
                            if f_literal.is_ok() {
                                tokens.push(TokenInfo::new(Token::Literal(Literal::Integer(f_literal.expect("Unr!"))), index + 1));
                            } else {
                                errors.push(report_error("Failed to prase float number literal".to_owned(), index + 1, line_counter));
                            };
                        }
                        _ => {
                            errors.push(report_error("Failed to prase float number literal".to_owned(), index + 1, line_counter));
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
                        "true"  => Token::Literal(Literal::Boolean(true)),
                        "false" => Token::Literal(Literal::Boolean(false)),
                        _       => Token::Identifier(idenf),
                    };
                    tokens.push(TokenInfo::new(token, index + 1));
                }
                '<' => {
                    tokens.push(match  char_itr.peek() {
                        Some((_, '=')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::LesserEq), index + 1)
                        },
                        Some((_, '<')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::Shl), index + 1)
                        },
                        _ => TokenInfo::new(Token::Operator(Operator::Lesser), index + 1),
                    });
                },
                '>' => {
                        tokens.push(match char_itr.peek() {
                            Some((_, '=')) => {
                                let _ = char_itr.next().expect("Unr!");
                                TokenInfo::new(Token::Operator(Operator::GreaterEq), index + 1)
                            },
                            Some((_, '>')) => {
                                let _ = char_itr.next().expect("Unr!");
                                TokenInfo::new(Token::Operator(Operator::Shr), index + 1)
                            },
                            _ => TokenInfo::new(Token::Operator(Operator::Greater), index + 1),
                        }
                    );
                },
                '+' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, '=')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::AssingInc), index + 1)
                        }
                        _ => TokenInfo::new(Token::Operator(Operator::Plus), index + 1),
                    })
                },
                '-' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, '=')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::AssignDec), index + 1)
                        }
                        _ => TokenInfo::new(Token::Operator(Operator::Minus), index + 1),
                    })
                },
                '*' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, '=')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::AssignMul), index + 1)
                        }
                        _ => TokenInfo::new(Token::Operator(Operator::Multip), index + 1),
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
                                TokenInfo::new(Token::Operator(Operator::AssignDiv), index + 1)
                            },
                            Some((_, '/')) => { // comments
                                break 'over_line_loop;
                            },
                            _ => TokenInfo::new(Token::Operator(Operator::Div), index + 1),
                        })
                    }
                },
                '!' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, '=')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::Neq), index + 1)
                        }
                        _ => TokenInfo::new(Token::Operator(Operator::Not), index + 1),
                    })
                },
                ';' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::Semicolon), index + 1));
                },
                '=' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, '=')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::Eq), index + 1)
                        },
                        _ => TokenInfo::new(Token::Operator(Operator::Assign), index + 1),
                    });
                }
                '?' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::IfExpr), index + 1));
                },
                ':' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, ':')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::ModStream), index + 1)
                        },
                        _ => TokenInfo::new(Token::Operator(Operator::ElseExpr), index + 1),
                    });
                },
                '&' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, '&')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::And), index + 1)
                        },
                        _ => TokenInfo::new(Token::Operator(Operator::BitAnd), index + 1),
                    });
                },
                '|' => {
                    tokens.push(match char_itr.peek() {
                        Some((_, '|')) => {
                            let _ = char_itr.next().expect("Unr!");
                            TokenInfo::new(Token::Operator(Operator::Or), index + 1)
                        },
                        _ => TokenInfo::new(Token::Operator(Operator::BitOr), index + 1),
                    });
                }
                '%' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::Modulo), index + 1));
                },
                '{' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::OpBrace), index + 1));
                },
                '}' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::CloBrace), index + 1));
                },
                '(' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::OpParenth), index + 1));
                },
                ')' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::CloParenth), index + 1));
                },
                '[' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::OpArr), index + 1));
                },
                ']' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::CloArr), index + 1));
                },
                ',' => {
                    tokens.push(TokenInfo::new(Token::Operator(Operator::Separator), index + 1));
                }
                ' ' => { },
                _  => unreachable!(),
            }
        }

        lines.push(LineTokens {
            number: line_counter,
            value: line.to_owned(),
            tokens: tokens.drain(0..).collect(),
        });
        line_counter += 1;

    };

    return (lines, errors);
}

pub fn report_error(error_message: String, column: usize, line: usize) -> String {
    return format!("Error: {} - at line: {}, col: {}.", error_message, line, column);
}


mod tests {
    #![allow(unused_imports)]
    use crate::{*, lex::Operator};
    use crate::lex::LineTokens;
    use super::{lex_content, TokenInfo, Token};

#[test]
fn should_parse_identifier() {
    assert_eq!(lex_content("thisIsMy_d ESSA\n120 5412\n+-/*&".to_owned()),
        (vec![], vec![])
    )
}

}
