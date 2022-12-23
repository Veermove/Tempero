mod lex;
mod parse;
mod typecheck;
mod eval;

use std::{fs::File, io::{self, Read}};



use lex::*;
use parse::*;
use typecheck::*;

use crate::eval::eval_exp;

fn main() {
    let mut buffer = String::new();
    'simul: while let Ok(_) = io::stdin().read_line(&mut buffer) {
        print!(">");
        let tokens = {
            let t = lex_content_simple(buffer.drain(0..).collect());
            if t.is_err() {
                continue 'simul;
            }
            t.unwrap()
        };

            let (asts, _, errors) = parse::prase_expressions(tokens);
            if !errors.is_empty() {
                for er in errors {
                    println!("{}", er);
                    continue 'simul;
                }
            }

            for exp in asts {
                let typed_expr = {
                    let t = typecheck::TExpression::new(exp);
                    if let Err(v) = &t {
                        println!("{}", v);
                        continue 'simul;
                    }
                    t.unwrap()
                };

                let evaluated = {
                    let t = eval_exp(&typed_expr.expression);
                    if let Err(v) = &t {
                        println!("{}", v);
                        continue 'simul;
                    }
                    t.unwrap()
                };

                println!("{}", &evaluated);
            }
    }
}

fn main2() -> std::io::Result<()> {
    // let mut file = File::open("./add_ex.tm")?;
    // let mut content = String::new();
    // file.read_to_string(&mut content)?;

    // let tokens = lex::lex_content(content);
    // let ast = parse::prase_expressions(expression_tokens)
    Ok(())
}
