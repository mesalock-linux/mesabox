use either::Either;
use nom::{alphanumeric1, space0, newline};

use std::ffi::OsString;
use std::os::unix::ffi::{OsStringExt, OsStrExt};
use std::os::unix::io::RawFd;

use super::ast::*;

/*
named!(script<&[u8], Script>,
    do_parse!(
        separated_nonempty_list_complete!()
        eof!() >>
    )
);*/

named!(pub complete_command<&[u8], CompleteCommand>,
    do_parse!(
        ignore >>
        res: separated_nonempty_list_complete!(separator, list) >>
        //eof!() >>
        (CompleteCommand::new(res))
    )
);

named!(list<&[u8], Vec<Vec<AndOr>>>,
    separated_nonempty_list_complete!(separator_op, and_or)
);

named!(and_or<&[u8], Vec<AndOr>>,
    do_parse!(
        first: pipeline >>
        res: fold_many0!(
            and_or_inner,
            vec![AndOr::new(first, SepKind::First)],
            |mut acc: Vec<_>, item| { acc.push(item); acc }
        ) >>
        (res)
    )
);

named!(and_or_inner<&[u8], AndOr>,
    do_parse!(
        sep: and_or_sep >>
        pipe: pipeline >>
        (AndOr::new(pipe, sep))
    )
);

named!(and_or_sep<&[u8], SepKind>,
    do_parse!(
        res: alt!(
            tag!("&&") => { |_| SepKind::And } |
            tag!("||") => { |_| SepKind::Or }
        ) >>
        ignore >>
        linebreak >>
        (res)
    )
);

named!(var_name<&[u8], OsString>,
    do_parse!(
        name: alphanumeric1 >>
        ignore >>
        (OsString::from_vec(name.to_owned()))
    )
);

named!(var_assign<&[u8], VarAssign>,
    do_parse!(
        name: var_name >>
        tag!("=") >>
        expr: value!(()) >>
        ignore >>
        (VarAssign { varname: name, value: expr })
    )
);

named!(pipe_seq<&[u8], Pipeline>,
    map!(separated_nonempty_list_complete!(pipe_sep, command), |vec| {
        vec.into_iter().collect()
    })
);

named!(pipe_sep,
    recognize!(tuple!(tag!("|"), ignore, linebreak))
);

named!(pipeline<&[u8], Pipeline>,
    do_parse!(
        bang: opt!(tag!("!")) >>
        seq: pipe_seq >>
        ({ let mut seq = seq; seq.bang = bang.is_some(); seq })
    )
);

named!(command<&[u8], Command>,
    alt!(
        function_def => { |def: FunctionDef| def.into() } |
        do_parse!(
            cmd: compound_command >>
            redir: opt!(redirect_list) >>
            ({ let mut cmd = cmd; cmd.redirect_list = redir; cmd })
        ) => { |cmd| cmd } |
        simple_command => { |cmd: SimpleCommand| cmd.into() }
    )
);

named!(compound_command<&[u8], Command>,
    alt!(
        brace_group |
        subshell |
        for_clause => { |clause: ForClause| clause.into() } |
        case_clause => { |clause: CaseClause| clause.into() } |
        if_clause => { |clause: IfClause| clause.into() } |
        while_clause => { |clause: WhileClause| clause.into() } |
        until_clause => { |clause: WhileClause| clause.into() }
    )
);

named!(subshell<&[u8], Command>,
    delimited!(
        pair!(tag!("("), ignore),
        compound_list,
        pair!(tag!(")"), ignore)
    )
);

named!(compound_list<&[u8], Command>,
    do_parse!(
        // XXX: this is technically an optional newline_list in the spec
        linebreak >>
        res: map!(term, |and_ors| Command::with_inner(CommandInner::AndOr(and_ors))) >>
        opt!(separator) >>
        (res)
    )
);

// FIXME: check if this sketchy parsing is still needed
named!(term<&[u8], Vec<Vec<AndOr>>>,
    //terminated!(separated_nonempty_list_complete!(separator, and_or), opt!(separator))
    do_parse!(
        first: and_or >>
        res: many_till!(
            complete!(term_inner),
            pair!(opt!(separator), not!(and_or))
        ) >>
        //    vec![first],
        //    |mut acc: Vec<_>, item| { acc.push(item); acc }
        //) >>
        ({ let mut res = res.0; res.insert(0, first); res })
    )
);

named!(term_inner<&[u8], Vec<AndOr>>,
    do_parse!(
        separator >>
        and_or: and_or >>
        (and_or)
    )
);

named!(for_clause<&[u8], ForClause>,
    dbg_dmp!(do_parse!(
        tag!("for") >>
        ignore >>
        name: name >>
        linebreak >>
        words: opt!(
            do_parse!(
                in_tok >>
                words: many0!(word) >>
                sequential_sep >>
                (words)
            )
        ) >>
        do_stmt: do_group >>
        (ForClause::new(name, words, do_stmt))
    ))
);

named!(name<&[u8], Name>,
    do_parse!(
        val: map!(alphanumeric1, |res| OsString::from_vec(res.to_owned())) >>
        ignore >>
        (val)
    )
);

named!(in_tok,
    // TODO: parse in (apply rule 6)
    recognize!(pair!(tag!("in"), ignore))
);

named!(case_clause<&[u8], CaseClause>,
    do_parse!(
        tag!("case") >>
        ignore >>
        word: word >>
        linebreak >>
        in_tok >>
        linebreak >>
        case_list: many0!(
            alt!(case_item | case_item_ns)
        ) >>
        tag!("esac") >>
        ignore >>
        (CaseClause::new(word, CaseList::new(case_list)))
    )
);

named!(case_item_ns<&[u8], CaseItem>,
    do_parse!(
        opt!(pair!(tag!("("), ignore)) >>
        pattern: pattern >>
        tag!(")") >>
        ignore >>
        list: opt!(compound_list) >>
        linebreak >>
        (CaseItem::new(pattern, list))
    )
);

named!(case_item<&[u8], CaseItem>,
    do_parse!(
        item: case_item_ns >>
        tag!(";;") >>
        ignore >>
        linebreak >>
        (item)
    )
);

// TODO: clearly
named!(pattern<&[u8], Pattern>,
    value!(())
);

named!(if_clause<&[u8], IfClause>,
    do_parse!(
        tag!("if") >>
        ignore >>
        cond: compound_list >>
        tag!("then") >>
        ignore >>
        body: compound_list >>
        else_stmt: opt!(else_part) >>
        tag!("fi") >>
        ignore >>
        (IfClause::new(cond, body, else_stmt))
    )
);

named!(else_part<&[u8], ElseClause>,
    alt!(
        do_parse!(
            tag!("elif") >>
            ignore >>
            cond: compound_list >>
            tag!("then") >>
            ignore >>
            body: compound_list >>
            else_stmt: else_part >>
            (ElseClause::new(Some(cond), body, Some(Box::new(else_stmt))))
        ) |
        do_parse!(
            tag!("else") >>
            ignore >>
            body: compound_list >>
            (ElseClause::new(None, body, None))
        )
    )
);

named!(while_clause<&[u8], WhileClause>,
    // FIXME: maybe these clauses should use space1?
    do_parse!(
        tag!("while") >>
        ignore >>
        cond: compound_list >>
        do_stmt: do_group >>
        (WhileClause::new(cond, true, do_stmt))
    )
);

named!(until_clause<&[u8], WhileClause>,
    do_parse!(
        tag!("until") >>
        ignore >>
        cond: compound_list >>
        do_stmt: do_group >>
        (WhileClause::new(cond, false, do_stmt))
    )
);

named!(function_def<&[u8], FunctionDef>,
    do_parse!(
        name: fname >>
        tag!("()") >>
        ignore >>
        linebreak >>
        body: function_body >>
        (FunctionDef::new(name, body))
    )
);

named!(function_body<&[u8], FunctionBody>,
    do_parse!(
        // TODO: apply rule 9
        cmd: compound_command >>
        redir: opt!(redirect_list) >>
        (FunctionBody::new(cmd, redir))
    )
);

named!(fname<&[u8], Name>,
    // FIXME: this do_parse should not be necessary
    do_parse!(n: name >> (n))
);

named!(brace_group<&[u8], Command>,
    delimited!(
        pair!(tag!("{"), ignore),
        compound_list,
        pair!(tag!("}"), ignore)
    )
);

named!(do_group<&[u8], Command>,
    do_parse!(
        tag!("do") >>
        ignore >>
        lst: compound_list >>
        tag!("done") >>
        ignore >>
        (lst)
    )
);

named!(simple_command<&[u8], SimpleCommand>,
    alt!(
        do_parse!(
            prefix: cmd_prefix >>
            others: map!(opt!(pair!(cmd_word, opt!(cmd_suffix))), |others| {
                others.map(|(word, suffix)| (Some(word), suffix)).unwrap_or((None, None))
            }) >>
            (SimpleCommand::new(others.0, Some(prefix), others.1))
        ) |
        do_parse!(
            name: cmd_name >>
            suffix: opt!(cmd_suffix) >>
            (SimpleCommand::new(Some(name), None, suffix))
        )
    )
);

named!(cmd_name<&[u8], CommandName>,
    // TODO: apply rule 7a
    map_opt!(word, |word: OsString| {
        // TODO: this prob needs to check all keywords (ensure that e.g. in and then are actually not allowed)
        match word.as_os_str().as_bytes() {
            b"done" | b"for" | b"done" | b"if" | b"fi" | b"while" | b"until" | b"case" | b"esac" | b"in" | b"then" => None,
            _ => Some(word)
        }
    })
);

named!(cmd_word<&[u8], CommandName>,
    // TODO: apply rule 7b
    // FIXME: this do_parse should not be necessary
    do_parse!(w: word >> (w))
);

named!(cmd_prefix<&[u8], Vec<PreAction>>,
    many1!(
        alt!(
            io_redirect => { |redir| Either::Left(redir) } |
            var_assign => { |assign| Either::Right(assign) }
        )
    )
);

named!(cmd_suffix<&[u8], Vec<PostAction>>,
    many1!(
        alt!(
            io_redirect => { |redir| Either::Left(redir) } |
            word => { |word| Either::Right(word) }
        )
    )
);

named!(redirect_list<&[u8], Vec<IoRedirect>>,
    many1!(io_redirect)
);

named!(io_redirect<&[u8], IoRedirect>,
    alt!(
        do_parse!(
            num: opt!(io_number) >>
            file: io_file >>
            (IoRedirect::File(num, file))
        ) |
        do_parse!(
            num: opt!(io_number) >>
            here: io_here >>
            (IoRedirect::Heredoc(num, here))
        )
    )
);

named!(io_file<&[u8], IoRedirectFile>,
    do_parse!(
        kind: alt!(
            tag!("<&") => { |_| IoRedirectKind::DupInput } |
            tag!("<>") => { |_| IoRedirectKind::ReadWrite } |
            tag!("<") => { |_| IoRedirectKind::Input } |
            tag!(">&") => { |_| IoRedirectKind::DupOutput } |
            tag!(">>") => { |_| IoRedirectKind::Append } |
            tag!(">|") => { |_| IoRedirectKind::Clobber } |
            tag!(">") => { |_| IoRedirectKind::Output }
        ) >>
        ignore >>
        name: filename >>
        (IoRedirectFile::new(kind, name))
    )
);

named!(filename<&[u8], Name>,
    // TODO: apply rule 2
    // FIXME: this do_parse should not be necessary
    do_parse!(w: word >> (w))
);

named!(io_here<&[u8], HereDoc>,
    do_parse!(
        tag!("<<") >>
        dash: opt!(tag!("-")) >>
        ignore >>
        delim: here_end >>
        // TODO: figure out how to parse heredoc
        (unimplemented!())
    )
);

named!(here_end<&[u8], Word>,
    // TODO: apply rule 3
    // FIXME: this do_parse should not be necessary
    do_parse!(w: word >> (w))
);

named!(linebreak<&[u8], Option<&[u8]>>,
    opt!(newline_list)
    /*alt!(
        newline_list | space1
    )*/
);

// XXX: not sure if this is gonna make a Vec (which we don't want as we don't use the result)
named!(newline_list,
    // XXX: spec doesn't actually seem to give value of newline (am guessing for portability)
    //recognize!(pair!(take_while1!(|byte| byte == b'\n'), ignore))
    recognize!(many1!(pair!(newline, ignore)))
);

// XXX: is & just run in background for this?
named!(separator_op,
    recognize!(pair!(alt!(tag!("&") | tag!(";")), ignore))
);

named!(separator,
    alt!(recognize!(pair!(separator_op, linebreak)) | newline_list)
);

named!(sequential_sep,
    alt!(recognize!(tuple!(tag!(";"), ignore, linebreak)) | newline_list)
);

named!(word<&[u8], Word>,
    // TODO: this might need to be able to parse quoted strings/words following different rules
    do_parse!(
        val: map!(alphanumeric1, |res| OsString::from_vec(res.to_owned())) >>
        ignore >>
        (val)
    )
);

named!(io_number<&[u8], RawFd>,
    // XXX: not sure if right
    /*many1(digit()).then(|num_iter: Vec<_>| {
        let res = String::from_utf8(num_iter).map_err(|_| ()).and_then(|s| RawFd::from_str(&s).map_err(|_| ()));
        if let Ok(num) = res {
            value(num).left()
        } else {
            /* FIXME: unexpected(num_iter) */
            unexpected("IO number").map(|_| 0).message("not a valid file descriptor").right()
        }
    })*/
    value!(1)
);

named!(ignore,
    recognize!(
        pair!(
            space0,
            opt!(pair!(
                tag!("#"),
                // FIXME: we use newline() else where
                take_till!(|byte| byte == b'\n')
            ))
        )
    )
);
