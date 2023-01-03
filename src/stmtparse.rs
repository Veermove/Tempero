use std::{collections::HashMap};

use crate::{
    types::{
        EnumeratedTokens, BindingRequests,
        Token, Keyword, Operator, Type, Statement,
        Program, TExpression
    },
    exprparse,
};

pub fn parse_program(tokens: EnumeratedTokens) -> Program {
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

    let (stmts, binds) = extract_bindings(stmts, &HashMap::new());

    return Program::new(
        stmts.into_iter()
            .map(|r|
                r.and_then(|typless_stmt| to_typed_stmt(typless_stmt, &binds))
            ).collect(),
        binds
    );
}

fn extract_bindings(stmts: Vec<Result<Statement, String>>, higer_scoped_binds: &BindingRequests) -> (Vec<Result<Statement, String>>, BindingRequests) {
    let mut binds = HashMap::new();


    let stmts = stmts.into_iter()
        .map(|parsing_res| {
            let (p, cb) = extract_binding(parsing_res, &(binds.clone().into_iter().chain(higer_scoped_binds.clone().into_iter()).collect()));
            binds.extend(cb.into_iter());
            p
        })
        .collect();
    (stmts, binds)
}

fn extract_binding(stmt: Result<Statement, String>, higer_scoped_binds: &BindingRequests) -> (Result<Statement, String>, BindingRequests) {
    let mut binds = HashMap::new();
    let r = match stmt {
        Ok(Statement::Assignment(name, mut expression)) => {
            let type_res = (
                expression.get_type(&(binds.clone().into_iter().chain(higer_scoped_binds.clone().into_iter()).collect())),
                name
            );

            match type_res {
                (Ok(bind_req), name)
                    => {
                        binds.insert(name.clone(), bind_req);
                        Ok(Statement::Assignment(name, expression))
                    }
                (Err(msg), _)
                    => {
                        Err(format!("Unsuccessfull assigment reuqest: {}", msg))
                    }
            }
        }
        Ok(Statement::Block(inner_stmts, empty_binds)) => {
            assert!(empty_binds.is_empty());
            let (n_stmts, block_binds) = extract_bindings(
                inner_stmts.to_vec(),
                &(binds.clone().into_iter().chain(higer_scoped_binds.clone().into_iter()).collect())
            );

            Ok(Statement::Block(n_stmts, block_binds))
        }
        Ok(Statement::If(condition, truthy_stmts, falsy_stmts)) => {
            let tv = truthy_stmts
                .and_then(|s| {
                    let (res, _) = extract_binding(Ok(*s), &(binds.clone().into_iter().chain(higer_scoped_binds.clone().into_iter()).collect()));
                    res.map(Box::new)
                });

            let fv = falsy_stmts.map(|ffv| ffv
                .and_then(|s| {
                    let (res, _) = extract_binding(Ok(*s), &(binds.clone().into_iter().chain(higer_scoped_binds.clone().into_iter()).collect()));
                    res.map(Box::new)
                })
            );

            Ok(Statement::If(condition, tv, fv))
        }
        _ => stmt
    };

    return (r, binds);
}


fn to_typed_stmt(stmt: Statement, state: &BindingRequests) -> Result<Statement, String> {
    match stmt {
        Statement::Expr(mut v) => v.get_type(state)
            .and(Ok(Statement::Expr(v))),
        Statement::Print(mut v) => v.get_type(state)
            .and(Ok(Statement::Print(v))),
        Statement::Assignment(v, mut expr) => expr.get_type(state)
            .and(Ok(Statement::Assignment(v, expr))),
        Statement::Block(inner_stmts, inner_types) => {
            let n_stmts = inner_stmts.into_iter()
                .map(|d| d
                    .and_then(|s|
                        to_typed_stmt(s, &(inner_types.clone().into_iter().chain(state.clone().into_iter()).collect()))
                    )
                )
                .collect();
            return Ok(Statement::Block(n_stmts, inner_types));
        }
        Statement::If(mut c , tv, fv) => {
            if c.get_type(state)? != Type::Bool {
                return Err(format!("Condition in if statement must be of type Bool but was {}", c.get_type(state)?));
            }

            let v_if_true = tv.and_then(|s| to_typed_stmt(*s, state))
                .map(Box::new);
            let v_if_false = fv.map(|falsy_v|
                falsy_v.and_then(|s| to_typed_stmt(*s, state))
                    .map(Box::new)
            );

            return Ok(Statement::If(c, v_if_true, v_if_false));
        },
    }
}

fn parse_stmt(tokens: &EnumeratedTokens, o_index: usize) -> Result<(Statement, usize), String> {
    parse_if(tokens, o_index)
}

fn prase_l_while(tokens: &EnumeratedTokens, o_index: usize) -> Result<(Statement, usize), String> {
    todo!()
}

fn parse_if(tokens: &EnumeratedTokens, o_index: usize) -> Result<(Statement, usize), String> {
    let initial = tokens.get(o_index);
    if !matches!(initial, Some((Token::Keyword(Keyword::If), _, _))) {
        return parse_assign(tokens, o_index);
    }

    let (condition, n_index) = exprparse::parse_expr(tokens, o_index + 1)
        .map_err(|e|
            format!("Error while prasing condition in 'if' statement. Required: if <bool-expr> <stmt>. Error: {}", e)
        )?;

    let stmt_true = parse_stmt(tokens, n_index);
    let mut index_after = stmt_true.as_ref()
        .map(|(_, i)| *i)
        .unwrap_or(exprparse::find_next_start(tokens, n_index));

    if !matches!(tokens.get(index_after), Some((Token::Keyword(Keyword::Else), _, _))) {
        return Ok((
            Statement::If(
                TExpression::new(condition),
                stmt_true.map(|(s, _)| Box::new(s)),
                None
            ),
            index_after,
        ));
    }

    let stmt_false = parse_stmt(tokens, index_after + 1);
    index_after = stmt_true.as_ref()
        .map(|(_, i)| *i + 1)
        .unwrap_or(exprparse::find_next_start(tokens, n_index));

    return Ok((
            Statement::If(
                TExpression::new(condition),
                stmt_true.map(|(s, _)| Box::new(s)),
                Some(stmt_false.map(|(s, _)| Box::new(s)))
            ),
            index_after
        ));
}

fn parse_assign(tokens: &EnumeratedTokens, o_index: usize) -> Result<(Statement, usize), String> {
    let initial = tokens.get(o_index);
    if !matches!(initial, Some((Token::Keyword(Keyword::Let), _, _))) {
        return parse_block(tokens, o_index);
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
        return Ok((Statement::Assignment(id.to_owned(), TExpression::new(expr)), o_index + 1));
    }
    unreachable!()
}

fn parse_block(tokens: &EnumeratedTokens, o_index: usize) -> Result<(Statement, usize), String> {
    let open = tokens.get(o_index);
    if !matches!(open, Some((Token::Operator(Operator::OpBrace), _, _))) {
        return parse_expr_stmt(tokens, o_index);
    }

    let mut index = o_index + 1;
    let mut block_stmts = Vec::new();
    loop {
        let stmt = parse_stmt(tokens, index);
        index = stmt.as_ref()
            .map(|(_, i)| *i)
            .unwrap_or(exprparse::find_next_start(tokens, index));

        block_stmts.push(stmt.map(|(s, _)| s));

        if index >= tokens.len() {
            return Err("Unfinished block statement.".to_owned());
        } else if matches!(tokens.get(index), Some((Token::Operator(Operator::CloBrace), _, _))) {
            return Ok((Statement::Block(block_stmts, HashMap::new()), index + 1));
        }
    }
}

fn parse_expr_stmt(tokens: &EnumeratedTokens, o_index: usize) -> Result<(Statement, usize), String> {
    let (p_expr_to_print, index) = exprparse::parse_expr(&tokens, o_index)?;
    let p_semicol = tokens.get(index);

    match (p_expr_to_print, p_semicol) {
        (ex, Some((Token::Operator(Operator::Semicolon), _, _))) => {
            Ok((Statement::Expr(TExpression::new(ex)), index + 1))
        }
        _ => parse_print_stmt(tokens, o_index),
    }
}

fn parse_print_stmt(tokens: &EnumeratedTokens, o_index: usize) -> Result<(Statement, usize), String> {
    let p_print = tokens.get(o_index);
    if !matches!(p_print, Some((Token::Identifier(id), _, _)) if id.eq("print")) {
        return Err("Error: expected statement 'print <expr> ;'.".to_owned());
    }

    let (p_expr_to_print, index) = exprparse::parse_expr(&tokens, o_index + 1)?;
    let p_semicol = tokens.get(index);

    if !matches!(p_semicol, Some((Token::Operator(Operator::Semicolon), _, _))) {
        return Err("Error: print statement requires ';' at the end.".to_owned());
    }

    return Ok((Statement::Print(TExpression::new(p_expr_to_print)), index + 1));
}
