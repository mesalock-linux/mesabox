use either::Either;
use nom::{alphanumeric1, space0, newline};

use std::cell::RefCell;
use std::ffi::OsString;
use std::os::unix::ffi::{OsStringExt, OsStrExt};
use std::os::unix::io::RawFd;
use std::rc::Rc;

use super::ast::*;

struct HereDocMarker {
    marker: OsString,
    strip_tabs: bool,
    heredoc: Rc<RefCell<HereDoc>>,
}

pub struct Parser {
    heredoc_markers: Vec<HereDocMarker>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            heredoc_markers: vec![],
        }
    }

    /*
    named!(script<&[u8], Script>,
        do_parse!(
            separated_nonempty_list_complete!()
            eof!() >>
        )
    );*/

    method!(pub complete_command<Parser, &[u8], CompleteCommand>, mut self,
        do_parse!(
            ignore >>
            call!(linebreak, &mut self) >>
            res: separated_nonempty_list_complete!(call!(separator, &mut self), call!(list, &mut self)) >>
            //eof!() >>
            (CompleteCommand::new(res))
        )
    );
}

named_args!(list<'a>(parser: &mut Parser)<&'a [u8], Vec<Vec<AndOr>>>,
    separated_nonempty_list_complete!(call!(separator_op, parser), call!(and_or, parser))
);

named_args!(and_or<'a>(parser: &mut Parser)<&'a [u8], Vec<AndOr>>,
    do_parse!(
        first: call!(pipeline, parser) >>
        res: fold_many0!(
            call!(and_or_inner, parser),
            vec![AndOr::new(first, SepKind::First)],
            |mut acc: Vec<_>, item| { acc.push(item); acc }
        ) >>
        (res)
    )
);

named_args!(and_or_inner<'a>(parser: &mut Parser)<&'a [u8], AndOr>,
    do_parse!(
        sep: call!(and_or_sep, parser) >>
        pipe: call!(pipeline, parser) >>
        (AndOr::new(pipe, sep))
    )
);

named_args!(and_or_sep<'a>(parser: &mut Parser)<&'a [u8], SepKind>,
    do_parse!(
        res: alt!(
            tag!("&&") => { |_| SepKind::And } |
            tag!("||") => { |_| SepKind::Or }
        ) >>
        ignore >>
        call!(linebreak, parser) >>
        (res)
    )
);

named_args!(var_name<'a>(parser: &mut Parser)<&'a [u8], OsString>,
    do_parse!(
        name: alphanumeric1 >>
        ignore >>
        (OsString::from_vec(name.to_owned()))
    )
);

named_args!(var_assign<'a>(parser: &mut Parser)<&'a [u8], VarAssign>,
    do_parse!(
        name: call!(var_name, parser) >>
        tag!("=") >>
        expr: value!(()) >>
        ignore >>
        (VarAssign { varname: name, value: expr })
    )
);

named_args!(pipe_seq<'a>(parser: &mut Parser)<&'a [u8], Pipeline>,
    map!(separated_nonempty_list_complete!(call!(pipe_sep, parser), call!(command, parser)), |vec| {
        vec.into_iter().collect()
    })
);

named_args!(pipe_sep<'a>(parser: &mut Parser)<&'a [u8], &'a [u8]>,
    recognize!(tuple!(tag!("|"), ignore, call!(linebreak, parser)))
);

named_args!(pipeline<'a>(parser: &mut Parser)<&'a [u8], Pipeline>,
    do_parse!(
        bang: opt!(tag!("!")) >>
        seq: call!(pipe_seq, parser) >>
        ({ let mut seq = seq; seq.bang = bang.is_some(); seq })
    )
);

named_args!(command<'a>(parser: &mut Parser)<&'a [u8], Command>,
    alt!(
        call!(function_def, parser) => { |def: FunctionDef| def.into() } |
        do_parse!(
            cmd: call!(compound_command, parser) >>
            redir: opt!(call!(redirect_list, parser)) >>
            ({ let mut cmd = cmd; cmd.redirect_list = redir; cmd })
        ) => { |cmd| cmd } |
        call!(simple_command, parser) => { |cmd: SimpleCommand| cmd.into() }
    )
);

named_args!(compound_command<'a>(parser: &mut Parser)<&'a [u8], Command>,
    alt!(
        call!(brace_group, parser) |
        call!(subshell, parser) |
        call!(for_clause, parser) => { |clause: ForClause| clause.into() } |
        call!(case_clause, parser) => { |clause: CaseClause| clause.into() } |
        call!(if_clause, parser) => { |clause: IfClause| clause.into() } |
        call!(while_clause, parser) => { |clause: WhileClause| clause.into() } |
        call!(until_clause, parser) => { |clause: WhileClause| clause.into() }
    )
);

named_args!(subshell<'a>(parser: &mut Parser)<&'a [u8], Command>,
    /*delimited!(
        pair!(tag!("("), ignore),
        call!(compound_list, parser),
        pair!(tag!(")"), ignore)
    )*/
    do_parse!(
        tag!("(") >>
        ignore >>
        res: call!(compound_list, parser) >>
        tag!(")") >>
        ignore >>
        (res)
    )
);

named_args!(compound_list<'a>(parser: &mut Parser)<&'a [u8], Command>,
    do_parse!(
        // XXX: this is technically an optional newline_list in the spec
        call!(linebreak, parser) >>
        res: map!(call!(term, parser), |and_ors| Command::with_inner(CommandInner::AndOr(and_ors))) >>
        opt!(call!(separator, parser)) >>
        (res)
    )
);

// FIXME: check if this sketchy parsing is still needed
named_args!(term<'a>(parser: &mut Parser)<&'a [u8], Vec<Vec<AndOr>>>,
    //terminated!(separated_nonempty_list_complete!(separator, and_or), opt!(separator))
    do_parse!(
        first: call!(and_or, parser) >>
        res: many_till!(
            complete!(call!(term_inner, parser)),
            pair!(opt!(call!(separator, parser)), not!(call!(and_or, parser)))
        ) >>
        //    vec![first],
        //    |mut acc: Vec<_>, item| { acc.push(item); acc }
        //) >>
        ({ let mut res = res.0; res.insert(0, first); res })
    )
);

named_args!(term_inner<'a>(parser: &mut Parser)<&'a [u8], Vec<AndOr>>,
    do_parse!(
        call!(separator, parser) >>
        and_or: call!(and_or, parser) >>
        (and_or)
    )
);

named_args!(for_clause<'a>(parser: &mut Parser)<&'a [u8], ForClause>,
    do_parse!(
        tag!("for") >>
        ignore >>
        name: call!(name, parser) >>
        call!(linebreak, parser) >>
        words: opt!(
            do_parse!(
                call!(in_tok, parser) >>
                words: many0!(word) >>
                call!(sequential_sep, parser) >>
                (words)
            )
        ) >>
        do_stmt: call!(do_group, parser) >>
        (ForClause::new(name, words, do_stmt))
    )
);

named_args!(name<'a>(parser: &mut Parser)<&'a [u8], Name>,
    do_parse!(
        val: map!(alphanumeric1, |res| OsString::from_vec(res.to_owned())) >>
        ignore >>
        (val)
    )
);

named_args!(in_tok<'a>(parser: &mut Parser)<&'a [u8], &'a [u8]>,
    // TODO: parse in (apply rule 6)
    recognize!(pair!(tag!("in"), ignore))
);

named_args!(case_clause<'a>(parser: &mut Parser)<&'a [u8], CaseClause>,
    do_parse!(
        tag!("case") >>
        ignore >>
        word: word >>
        call!(linebreak, parser) >>
        call!(in_tok, parser) >>
        call!(linebreak, parser) >>
        case_list: many0!(
            alt!(call!(case_item, parser) | call!(case_item_ns, parser))
        ) >>
        tag!("esac") >>
        ignore >>
        (CaseClause::new(word, CaseList::new(case_list)))
    )
);

named_args!(case_item_ns<'a>(parser: &mut Parser)<&'a [u8], CaseItem>,
    do_parse!(
        opt!(pair!(tag!("("), ignore)) >>
        pattern: call!(pattern, parser) >>
        tag!(")") >>
        ignore >>
        list: opt!(call!(compound_list, parser)) >>
        call!(linebreak, parser) >>
        (CaseItem::new(pattern, list))
    )
);

named_args!(case_item<'a>(parser: &mut Parser)<&'a [u8], CaseItem>,
    do_parse!(
        item: call!(case_item_ns, parser) >>
        tag!(";;") >>
        ignore >>
        call!(linebreak, parser) >>
        (item)
    )
);

// TODO: clearly
named_args!(pattern<'a>(parser: &mut Parser)<&'a [u8], Pattern>,
    value!(())
);

named_args!(if_clause<'a>(parser: &mut Parser)<&'a [u8], IfClause>,
    do_parse!(
        tag!("if") >>
        ignore >>
        cond: call!(compound_list, parser) >>
        tag!("then") >>
        ignore >>
        body: call!(compound_list, parser) >>
        else_stmt: opt!(call!(else_part, parser)) >>
        tag!("fi") >>
        ignore >>
        (IfClause::new(cond, body, else_stmt))
    )
);

named_args!(else_part<'a>(parser: &mut Parser)<&'a [u8], ElseClause>,
    alt!(
        do_parse!(
            tag!("elif") >>
            ignore >>
            cond: call!(compound_list, parser) >>
            tag!("then") >>
            ignore >>
            body: call!(compound_list, parser) >>
            else_stmt: call!(else_part, parser) >>
            (ElseClause::new(Some(cond), body, Some(Box::new(else_stmt))))
        ) |
        do_parse!(
            tag!("else") >>
            ignore >>
            body: call!(compound_list, parser) >>
            (ElseClause::new(None, body, None))
        )
    )
);

named_args!(while_clause<'a>(parser: &mut Parser)<&'a [u8], WhileClause>,
    // FIXME: maybe these clauses should use space1? (they definitely need to, otherwise stuff like whilex is parsed as while x)
    do_parse!(
        tag!("while") >>
        ignore >>
        cond: call!(compound_list, parser) >>
        do_stmt: call!(do_group, parser) >>
        (WhileClause::new(cond, true, do_stmt))
    )
);

named_args!(until_clause<'a>(parser: &mut Parser)<&'a [u8], WhileClause>,
    do_parse!(
        tag!("until") >>
        ignore >>
        cond: call!(compound_list, parser) >>
        do_stmt: call!(do_group, parser) >>
        (WhileClause::new(cond, false, do_stmt))
    )
);

named_args!(function_def<'a>(parser: &mut Parser)<&'a [u8], FunctionDef>,
    do_parse!(
        name: call!(fname, parser) >>
        tag!("()") >>
        ignore >>
        call!(linebreak, parser) >>
        body: call!(function_body, parser) >>
        (FunctionDef::new(name, body))
    )
);

named_args!(function_body<'a>(parser: &mut Parser)<&'a [u8], FunctionBody>,
    do_parse!(
        // TODO: apply rule 9
        cmd: call!(compound_command, parser) >>
        redir: opt!(call!(redirect_list, parser)) >>
        (FunctionBody::new(cmd, redir))
    )
);

named_args!(fname<'a>(parser: &mut Parser)<&'a [u8], Name>,
    // FIXME: this do_parse should not be necessary
    call!(name, parser)
);

named_args!(brace_group<'a>(parser: &mut Parser)<&'a [u8], Command>,
    delimited!(
        pair!(tag!("{"), ignore),
        call!(compound_list, parser),
        pair!(tag!("}"), ignore)
    )
);

named_args!(do_group<'a>(parser: &mut Parser)<&'a [u8], Command>,
    do_parse!(
        tag!("do") >>
        ignore >>
        lst: call!(compound_list, parser) >>
        tag!("done") >>
        ignore >>
        (lst)
    )
);

named_args!(simple_command<'a>(parser: &mut Parser)<&'a [u8], SimpleCommand>,
    alt!(
        do_parse!(
            prefix: call!(cmd_prefix, parser) >>
            others: map!(opt!(pair!(call!(cmd_word, parser), opt!(call!(cmd_suffix, parser)))), |others| {
                others.map(|(word, suffix)| (Some(word), suffix)).unwrap_or((None, None))
            }) >>
            (SimpleCommand::new(others.0, Some(prefix), others.1))
        ) |
        do_parse!(
            name: call!(cmd_name, parser) >>
            suffix: opt!(call!(cmd_suffix, parser)) >>
            (SimpleCommand::new(Some(name), None, suffix))
        )
    )
);

named_args!(cmd_name<'a>(parser: &mut Parser)<&'a [u8], CommandName>,
    // TODO: apply rule 7a
    map_opt!(word, |word: OsString| {
        // TODO: this prob needs to check all keywords (ensure that e.g. in and then are actually not allowed)
        match word.as_os_str().as_bytes() {
            b"done" | b"for" | b"done" | b"if" | b"fi" | b"while" | b"until" | b"case" | b"esac" | b"in" | b"then" => None,
            _ => Some(word)
        }
    })
);

named_args!(cmd_word<'a>(parser: &mut Parser)<&'a [u8], CommandName>,
    // TODO: apply rule 7b
    // FIXME: this do_parse should not be necessary
    do_parse!(w: word >> (w))
);

named_args!(cmd_prefix<'a>(parser: &mut Parser)<&'a [u8], Vec<PreAction>>,
    many1!(
        alt!(
            call!(io_redirect, parser) => { |redir| Either::Left(redir) } |
            call!(var_assign, parser) => { |assign| Either::Right(assign) }
        )
    )
);

named_args!(cmd_suffix<'a>(parser: &mut Parser)<&'a [u8], Vec<PostAction>>,
    many1!(
        alt!(
            call!(io_redirect, parser) => { |redir| Either::Left(redir) } |
            word => { |word| Either::Right(word) }
        )
    )
);

named_args!(redirect_list<'a>(parser: &mut Parser)<&'a [u8], Vec<IoRedirect>>,
    many1!(call!(io_redirect, parser))
);

named_args!(io_redirect<'a>(parser: &mut Parser)<&'a [u8], IoRedirect>,
    alt!(
        do_parse!(
            num: opt!(io_number) >>
            file: io_file >>
            (IoRedirect::File(num, file))
        ) |
        do_parse!(
            num: opt!(io_number) >>
            here: call!(io_here, parser) >>
            (IoRedirect::Heredoc(num, here))
        )
    )
);

named_args!(io_here<'a>(parser: &mut Parser)<&'a [u8], Rc<RefCell<HereDoc>>>,
    do_parse!(
        tag!("<<") >>
        dash: opt!(tag!("-")) >>
        ignore >>
        delim: call!(here_end, parser) >>
        // XXX: can there be more than one heredoc at a time? (the answer is yes, for different commands on the same line)
        ({
            // NOTE: not really happy about using an Rc here, so see if there's an easy way to do without it
            let heredoc = Rc::new(RefCell::new(HereDoc::default()));
            parser.heredoc_markers.push(HereDocMarker {
                marker: delim,
                strip_tabs: dash.is_some(),
                heredoc: heredoc.clone()
            });
            heredoc
        })
    )
);

named_args!(here_end<'a>(parser: &mut Parser)<&'a [u8], Word>,
    // TODO: apply rule 3
    // FIXME: this do_parse should not be necessary
    do_parse!(w: word >> (w))
);

// this needs to read a heredoc until it encounters "\nEND MARKER\n"
// XXX: this could probably use some improvement
fn parse_heredoc<'a>(mut input: &'a [u8], parser: &mut Parser) -> ::nom::IResult<&'a [u8], ()> {
    for marker in &mut parser.heredoc_markers {
        let mut heredoc = marker.heredoc.borrow_mut();
        let res = do_parse!(input,
            res: fold_many1!(
                do_parse!(
                    not!(tuple!(newline, tag!(marker.marker.as_bytes()), newline)) >>
                    val: take!(1) >>
                    (val)
                ),
                vec![],
                |mut acc: Vec<_>, item: &[u8]| {
                    acc.extend(item.iter());
                    acc
                }
            ) >>
            nl: newline >>
            tag!(marker.marker.as_bytes()) >>
            newline >>
            ({ let mut res = res; res.push(nl as _); res })
         )?;
        input = res.0;
        heredoc.data = res.1.to_owned();
    }
    parser.heredoc_markers.clear();

    Ok((input, ()))
}

named_args!(linebreak<'a>(parser: &mut Parser)<&'a [u8], Option<&'a [u8]>>,
    opt!(call!(newline_list, parser))
);

// XXX: not sure if this is gonna make a Vec (which we don't want as we don't use the result)
named_args!(newline_list<'a>(parser: &mut Parser)<&'a [u8], &'a [u8]>,
    // XXX: spec doesn't actually seem to give value of newline (am guessing for portability)
    //recognize!(pair!(take_while1!(|byte| byte == b'\n'), ignore))
    recognize!(many1!(tuple!(newline, cond!(!parser.heredoc_markers.is_empty(), call!(parse_heredoc, parser)), ignore)))
);

// XXX: is & just run in background for this?
named_args!(separator_op<'a>(parser: &mut Parser)<&'a [u8], &'a [u8]>,
    recognize!(pair!(alt!(tag!("&") | tag!(";")), ignore))
);

named_args!(separator<'a>(parser: &mut Parser)<&'a [u8], &'a [u8]>,
    alt!(recognize!(pair!(call!(separator_op, parser), call!(linebreak, parser))) | call!(newline_list, parser))
);

named_args!(sequential_sep<'a>(parser: &mut Parser)<&'a [u8], &'a [u8]>,
    alt!(recognize!(tuple!(tag!(";"), ignore, call!(linebreak, parser))) | call!(newline_list, parser))
);

named!(word<&[u8], Word>,
    // TODO: this might need to be able to parse quoted strings/words following different rules
    do_parse!(
        val: map!(alphanumeric1, |res| OsString::from_vec(res.to_owned())) >>
        ignore >>
        (val)
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
