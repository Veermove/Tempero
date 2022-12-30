use std::{fmt::Display, collections::HashMap};

use crate::{
    exprparse::{self, EnumeratedTokens, Expression},
    lex::{Operator, Token, Literal, Keyword},
    typecheck::{TExpression}
};


#[derive(Debug, Clone)]
pub enum Statement {
    Expr(TExpression),
    Print(TExpression),
    Assignment(String)
}


#[derive(Debug, Clone)]
pub enum TyplessStatement {
    Expr(Expression),
    Print(Expression),
    Assignment(String, Expression),
}

pub type State = HashMap<String, Box<Variable>>;

pub struct Variable {
    pub value: TExpression,
}

pub fn parse_program(tokens: EnumeratedTokens) -> (Vec<Result<Statement, String>>, State) {
    let mut stmts = Vec::new();
    let mut index = 0;
    let mut state: HashMap<String, Box<Variable>> = HashMap::new();

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

    return (
        stmts
            .into_iter()
            .map(|r| r.and_then(|r| to_typed_stmt(r, &mut state)))
            .collect(),
        state
        );
}

fn to_typed_stmt(stmt: TyplessStatement, state: &mut State) -> Result<Statement, String> {
    match stmt {
        TyplessStatement::Expr(v) => Ok(Statement::Expr(TExpression::new(v, state)?)),
        TyplessStatement::Print(v) => Ok(Statement::Print(TExpression::new(v, state)?)),
        TyplessStatement::Assignment(v, expr) => {
            let t_expr = Box::new(Variable { value: TExpression::new(expr, state)?});

            state.insert(v.clone(), t_expr );
            Ok(Statement::Assignment(v))
        },
    }
}

fn parse_stmt(tokens: &EnumeratedTokens, o_index: usize) -> Result<(TyplessStatement, usize), String> {
    parse_assign(tokens, o_index)
}

fn parse_assign(tokens: &EnumeratedTokens, o_index: usize) -> Result<(TyplessStatement, usize), String> {
    let initial = tokens.get(o_index);
    if !matches!(initial, Some((Token::Keyword(Keyword::Let), _, _))) {
        return parse_expr_stmt(tokens, o_index);
    }
    let (o_index, identfier) = (o_index + 1, tokens.get(o_index + 1));

    if !matches!(identfier, Some((Token::Identifier(_), _, _))) {
        if let Some((t, l, r)) = identfier {
            return Err(format!("Expected identifier after keyowrd 'let', but got: {}, at line: {}, col: {}", t, l, r));
        } else {
            return Err(format!("Expected identifier after keyword 'let'."))
        }
    };

    let (assignee,_, _) = identfier.unwrap();

    let (o_index, operator) = (o_index + 1, tokens.get(o_index + 1));

    if !matches!(operator, Some((Token::Operator(Operator::Assign), _, _))) {
        if let Some((t, l, r)) = identfier {
            return Err(format!("Expected operator after 'let <identifier>', but got: {}, at line: {}, col: {}", t, l, r));
        } else {
            return Err(format!("Expected operator after 'let <identifier>'."));
        }
    };

    // dbg!(o_index, &tokens);
    let (expr, o_index) = exprparse::parse_expr(&tokens, o_index + 1)?;

    if !matches!(tokens.get(o_index), Some((Token::Operator(Operator::Semicolon), _, _))) {
        if let Some((t, l, r)) = identfier {
            return Err(format!("Expected semicolon operator after 'let <identifier> <op> <expr>', but got: {}, at line: {}, col: {}", t, l, r));
        } else {
            return Err(format!("Expected semicolon
            operator after 'let <identifier> <op> <expr>'."));
        }
    };

    if let Token::Identifier(id) = assignee {
        return Ok((TyplessStatement::Assignment(id.to_owned(), expr), o_index + 1));
    }
    unreachable!()

}



fn parse_print_stmt(tokens: &EnumeratedTokens, o_index: usize) -> Result<(TyplessStatement, usize), String> {
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
            Statement::Assignment(v) => write!(f, "{}", v)
        }
    }
}
