use std::fmt::Display;

use crate::{exprparse::{self, EnumeratedTokens}, lex::{Operator, Token}, typecheck::TExpression};


#[derive(Debug, Clone)]
pub enum Statement {
    Expr(TExpression),
    Print(TExpression),
}

pub fn parse_program(tokens: EnumeratedTokens) -> Vec<Result<Statement, String>> {
    let mut stmts = Vec::new();
    let mut index = 0;

    loop {
        if index >= tokens.len() {
            break;
        }
        let res = parse_stmt(&tokens, index);
        if let Ok((stmt, ni)) = res {
            index = ni;
            stmts.push(Ok(stmt));
        } else if let Err(msg) = res {
            index = exprparse::find_next_start(&tokens, index);
            stmts.push(Err(msg));
        }
    }
    return stmts;
}

fn parse_stmt(tokens: &EnumeratedTokens, o_index: usize) -> Result<(Statement, usize), String> {
    prase_print_stmt(tokens, o_index)
}


fn prase_print_stmt(tokens: &EnumeratedTokens, o_index: usize) -> Result<(Statement, usize), String> {
    let p_print = tokens.get(o_index);
    if !matches!(p_print, Some((Token::Identifier(id), _, _)) if id.eq("print")) {
        return parse_expr_stmt(tokens, o_index);
    }

    let (p_expr_to_print, index) = exprparse::parse_expr(&tokens, o_index + 1)?;
    let p_semicol = tokens.get(index);

    if !matches!(p_semicol, Some((Token::Operator(Operator::Semicolon), _, _))) {
        return Err("Error: print statement requires ';' at the end.".to_owned());
    }

    let type_checked_exp = TExpression::new(p_expr_to_print)?;
    return Ok((Statement::Print(type_checked_exp), index + 1));
}

fn parse_expr_stmt(tokens: &EnumeratedTokens, o_index: usize) -> Result<(Statement, usize), String> {
    let (p_expr_to_print, index) = exprparse::parse_expr(&tokens, o_index)?;
    let p_semicol = tokens.get(index);

    match (p_expr_to_print, p_semicol) {
        (ex, Some((Token::Operator(Operator::Semicolon), _, _))) => {
            let type_checked_exp = TExpression::new(ex)?;
            Ok((Statement::Expr(type_checked_exp), index + 1))
        }
        _ => Err("Error: expression statement requires ';' at the end.".to_owned())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expr(a) => write!(f, "[{:?}] {} ;", a.exprssion_type, a.expression),
            Statement::Print(a) => write!(f, "[{:?}] print {} ;", a.exprssion_type, a.expression),
        }
    }
}
