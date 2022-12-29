use std::{fmt::Display, collections::HashMap};

use crate::{exprparse::{self, EnumeratedTokens, Expression}, lex::{Operator, Token, Literal}, typecheck::{TExpression}};


#[derive(Debug, Clone)]
pub enum Statement {
    Expr(TExpression),
    Print(TExpression),
}


#[derive(Debug, Clone)]
pub enum TyplessStatement {
    Expr(Expression),
    Print(Expression),
}

pub type State<'a> = HashMap<&'a str, Variable>;

pub struct Variable {
    pub value: Literal,
}

pub fn parse_program(tokens: EnumeratedTokens) -> Vec<Result<Statement, String>> {
    let mut stmts = Vec::new();
    let mut index = 0;
    let mut state: HashMap<&str, Variable> = HashMap::new();

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

    return stmts
        .into_iter()
        .map(|r| r.and_then(|r| to_typed_stmt(r, &state)))
        .collect();
}

fn to_typed_stmt(stmt: TyplessStatement, state: &State) -> Result<Statement, String> {
    match stmt {
        TyplessStatement::Expr(v) => Ok(Statement::Expr(TExpression::new(v, state)?)),
        TyplessStatement::Print(v) => Ok(Statement::Print(TExpression::new(v, state)?)),
    }
}

fn parse_stmt(tokens: &EnumeratedTokens, o_index: usize) -> Result<(TyplessStatement, usize), String> {
    prase_print_stmt(tokens, o_index)
}


fn prase_print_stmt(tokens: &EnumeratedTokens, o_index: usize) -> Result<(TyplessStatement, usize), String> {
    let p_print = tokens.get(o_index);
    if !matches!(p_print, Some((Token::Identifier(id), _, _)) if id.eq("print")) {
        return parse_expr_stmt(tokens, o_index);
    }

    let (p_expr_to_print, index) = exprparse::parse_expr(&tokens, o_index + 1)?;
    let p_semicol = tokens.get(index);

    if !matches!(p_semicol, Some((Token::Operator(Operator::Semicolon), _, _))) {
        return Err("Error: print statement requires ';' at the end.".to_owned());
    }

    // let type_checked_exp = TExpression::new(p_expr_to_print)?;
    return Ok((TyplessStatement::Print(p_expr_to_print), index + 1));
}

fn parse_expr_stmt(tokens: &EnumeratedTokens, o_index: usize) -> Result<(TyplessStatement, usize), String> {
    let (p_expr_to_print, index) = exprparse::parse_expr(&tokens, o_index)?;
    let p_semicol = tokens.get(index);

    match (p_expr_to_print, p_semicol) {
        (ex, Some((Token::Operator(Operator::Semicolon), _, _))) => {
            // let type_checked_exp = TExpression::new(ex)?;
            Ok((TyplessStatement::Expr(ex), index + 1))
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
