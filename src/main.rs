mod lex;
mod exprparse;
mod typecheck;
mod eval;
mod stmtparse;
mod types;

use std::{fs::File, io::{self, Read}, collections::HashMap};



use eval::Runtime;
use lex::*;
use exprparse::*;
use stmtparse::*;
use typecheck::*;
use types::*;

use crate::eval::eval_exp;

fn main() {
    read_comp();
}

fn interactive() {
    todo!()
    // let mut buffer = String::new();
    // let mut state: BindingRequests = HashMap::new();
    // 'simul: while let Ok(_) = io::stdin().read_line(&mut buffer) {
    //     print!(">");
    //     let tokens = {
    //         let t = lex_content_simple(buffer.drain(0..).collect());
    //         if t.is_err() {
    //             continue 'simul;
    //         }
    //         lex::to_simple_format(t.unwrap())
    //     };

    //         let (asts, _, errors) = exprparse::prase_expressions(tokens);
    //         if !errors.is_empty() {
    //             for er in errors {
    //                 println!("{}", er);
    //                 continue 'simul;
    //             }
    //         }

    //         for exp in asts {
    //             let typed_expr = {
    //                 let t = types::TExpression::new(exp, &state);
    //                 if let Err(v) = &t {
    //                     println!("{}", v);
    //                     continue 'simul;
    //                 }
    //                 t.unwrap()
    //             };

    //             // let evaluated = {
    //             //     let t = eval_exp(&typed_expr.expression, &state);
    //             //     if let Err(v) = &t {
    //             //         println!("{}", v);
    //             //         continue 'simul;
    //             //     }
    //             //     t.unwrap()
    //             // };

    //             // println!("{}", &evaluated);
    //         }
    // }
}

fn read_comp() -> std::io::Result<()> {
    let mut file = File::open("./add_ex.tm")?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;



    let (tokens, errors) = lex::lex_content(content);
    if !errors.is_empty() {
        println!("{}", errors.join("\n"));
    }

    let s_tokens = lex::to_simple_format(tokens);
    let program = stmtparse::parse_program(s_tokens);
    let rt = Runtime::new(HashMap::new(), program.binds);
    for p_stms in program.instrucrions {
        if let Ok(prog) = p_stms {
            dbg!(&prog);

            // if let Statement::Assignment(id, v) = prog {
            //     let value = eval_exp(&v.expression, &rt);
            //     if let Some(v) = program.binds.get(id.as_str()) {
            //         // let res = eval_exp(&v.value.expression, &rt);
            //         // if let Ok(l) = res {
            //         //     println!("{}", l);
            //         // }
            //     }

            // }

        } else if let Err(err) = p_stms {
            println!("{}", err);
        };
    }
    // let ast = parse::prase_expressions(expression_tokens)
    Ok(())
}
