use super::{Lexer, Token, ParamName, ParamExprToken, ParamExprKind, name, word, tokenize_with_op, is_param_op};

named_args!(pub param<'a>(lexer: &mut Lexer)<&'a [u8], Token<'a>>,
    do_parse!(
        res: call!(param_expr, lexer) >>
        tag!("}") >>
        (Token::Parameter(Box::new(res)))
    )
);

// TODO: define all of these
// FIXME: these names suck
named!(pub param_name<&[u8], ParamName>,
    alt!(
        name => { |name| ParamName::Var(name) } |
        tag!("*") => { |_| ParamName::Star } |
        tag!("?") => { |_| ParamName::Question } |
        tag!("@") => { |_| ParamName::At }
    )
);

named_args!(param_expr<'a>(lexer: &mut Lexer)<&'a [u8], ParamExprToken<'a>>,
    alt!(
        do_parse!(
            tag!("#") >>
            name: map!(opt!(param_name), |name| {
                // NOTE: this is to be compatible with dash (the standard doesn't explicitly say
                //       what to do in this case)
                name.unwrap_or_else(|| ParamName::Var(&[]))
            }) >>
            (ParamExprToken::new(name, ParamExprKind::Length, Vec::with_capacity(0)))
        ) |
        do_parse!(
            name: param_name >>
            kind: param_subst >>
            // FIXME: if there is space at the beginning of rhs this fails
            rhs: opt!(call!(tokenize_with_op, lexer, &is_param_op)) >>
            (ParamExprToken::new(name, kind, rhs.map(|v| v).unwrap_or_else(|| Vec::with_capacity(0))))
        ) |
        map!(param_name, |name| ParamExprToken::new(name, ParamExprKind::Value, Vec::with_capacity(0)))
    )
);

named!(param_subst<&[u8], ParamExprKind>,
    switch!(take!(1),
        b":" => switch!(take!(1),
            b"=" => value!(ParamExprKind::Assign) |
            b"-" => value!(ParamExprKind::Use) |
            b"?" => value!(ParamExprKind::Error) |
            b"+" => value!(ParamExprKind::AlternateNull)
        ) |
        b"=" => value!(ParamExprKind::AssignNull) |
        b"-" => value!(ParamExprKind::UseNull) |
        b"?" => value!(ParamExprKind::ErrorNull) |
        b"+" => value!(ParamExprKind::Alternate) |
        b"%" => alt_complete!(
            tag!("%") => { |_| ParamExprKind::LargeSuffix } |
            value!(ParamExprKind::SmallSuffix)
        ) |
        // FIXME: probably should integrate this with the Length stuff
        b"#" => alt_complete!(
            tag!("#") => { |_| ParamExprKind::LargePrefix } |
            value!(ParamExprKind::SmallPrefix)
        )
    )
);
