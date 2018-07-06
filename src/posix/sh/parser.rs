use either::Either;
use nom;

use std::cell::RefCell;
use std::ffi::OsString;
use std::iter;
use std::os::unix::ffi::{OsStringExt, OsStrExt};
use std::os::unix::io::RawFd;
use std::rc::Rc;

use super::ast::*;
use super::lexer::{DoubleQuotePart, ParamExprToken, Token, TokenIter, name};

type IResult<'a, 'b, O> = nom::IResult<TokenIter<'a, 'b>, O>;

fn tag_token<'a, 'b>(input: TokenIter<'a, 'b>, tag: Token) -> IResult<'a, 'b, &'a Token<'b>> {
    map_opt!(input, take_one, |token| {
        if token == &tag {
            Some(token)
        } else {
            None
        }
    })
}

macro_rules! tag_token (
    ($i: expr, $tag: expr) => ({
        call!($i, tag_token, $tag)
    });
);

macro_rules! tag_keyword (
    ($i:expr, $tag:expr) => ({
        verify!($i, take_one, |token| {
            if let &Token::Word(word) = token {
                if word == $tag.as_bytes() {
                    return true;
                }
            }
            false
        })
    })
);

fn take_one<'a, 'b>(input: TokenIter<'a, 'b>) -> IResult<'a, 'b, &'a Token<'b>> {
    map!(input, take!(1), |iter| iter.get(0))
}

pub struct Parser {
    linenum: usize,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            linenum: 1,
        }
    }

    pub fn parse<'a, 'b>(&mut self, input: &'a [Token<'b>]) -> IResult<'a, 'b, CompleteCommand> {
        let iter = TokenIter::new(input);
        self.complete_command(iter)
    }

    /*
    named!(script<&[u8], Script>,
        do_parse!(
            separated_nonempty_list_complete!()
            eof!() >>
        )
    );*/

    pub fn complete_command<'a, 'b>(&mut self, input: TokenIter<'a, 'b>) -> IResult<'a, 'b, CompleteCommand> {
        do_parse!(input,
            call!(linebreak, self) >>
            res: separated_nonempty_list_complete!(call!(separator, self), call!(list, self)) >>
            //eof!() >>
            (CompleteCommand::new(res))
        )
    }
}

fn list<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Vec<Vec<AndOr>>> {
    separated_nonempty_list_complete!(input, call!(separator_op, parser), call!(and_or, parser))
}

fn and_or<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Vec<AndOr>> {
    do_parse!(input,
        first: call!(pipeline, parser) >>
        res: fold_many0!(
            call!(and_or_inner, parser),
            vec![AndOr::new(first, SepKind::First)],
            |mut acc: Vec<_>, item| { acc.push(item); acc }
        ) >>
        (res)
    )
}

fn and_or_inner<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, AndOr> {
    do_parse!(input,
        sep: call!(and_or_sep, parser) >>
        pipe: call!(pipeline, parser) >>
        (AndOr::new(pipe, sep))
    )
}

fn and_or_sep<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, SepKind> {
    do_parse!(input,
        res: alt!(
            tag_token!(Token::And) => { |_| SepKind::And } |
            tag_token!(Token::Or) => { |_| SepKind::Or }
        ) >>
        call!(linebreak, parser) >>
        (res)
    )
}

fn var_name(name: &[u8]) -> Option<OsString> {
    let input = [Token::Word(name)];
    owned_name(TokenIter::new(&input)).ok().map(|(_, v)| v)
}

fn var_assign<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, VarAssign> {
    map_opt!(input, take_one, |token| {
        match token {
            &Token::Word(word) => {
                let mut iter = word.splitn(2, |&byte| byte == b'=');
                let left = var_name(iter.next().unwrap())?;
                match iter.next() {
                    Some(right) if left.len() > 0 => {
                        // there is an = that is not at the beginning of the word
                        let value = if right.is_empty() {
                            None
                        } else {
                            Some(Word::Simple(OsString::from_vec(right.to_owned())))
                        };
                        Some(VarAssign { varname: left, value: value })
                    }
                    _ => None,
                }
            }
            &Token::ComplexWord(ref parts) if parts.len() > 0 => {
                let mut part_iter = parts.iter();
                match part_iter.next() {
                    Some(Token::Word(word)) => {
                        let mut iter = word.splitn(2, |&byte| byte == b'=');
                        let left = var_name(iter.next().unwrap())?;
                        match iter.next() {
                            Some(right) if left.len() > 0 => {
                                // there is an = that is not at the beginning of the word
                                let value = if right.is_empty() {
                                    complex_word(part_iter)
                                } else {
                                    complex_word(iter::once(&Token::Word(right)).chain(part_iter))
                                }?;
                                Some(VarAssign { varname: left, value: Some(value) })
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    })
}

fn pipe_seq<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Pipeline> {
    map!(input, separated_nonempty_list_complete!(call!(pipe_sep, parser), call!(command, parser)), |vec| {
        vec.into_iter().collect()
    })
}

fn pipe_sep<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, &'a Token<'b>> {
    terminated!(input, tag_token!(Token::Pipe), call!(linebreak, parser))
}

fn pipeline<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Pipeline> {
    do_parse!(input,
        bang: opt!(tag_keyword!("!")) >>
        seq: call!(pipe_seq, parser) >>
        ({ let mut seq = seq; seq.bang = bang.is_some(); seq })
    )
}

fn command<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Command> {
    alt!(input,
        call!(function_def, parser) => { |def: FunctionDef| def.into() } |
        do_parse!(
            cmd: call!(compound_command, parser) >>
            redir: opt!(call!(redirect_list, parser)) >>
            ({ let mut cmd = cmd; cmd.redirect_list = redir; cmd })
        ) => { |cmd| cmd } |
        call!(simple_command, parser) => { |cmd: SimpleCommand| cmd.into() }
    )
}

fn compound_command<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Command> {
    alt!(input,
        call!(brace_group, parser) |
        call!(subshell, parser) |
        call!(for_clause, parser) => { |clause: ForClause| clause.into() } |
        call!(case_clause, parser) => { |clause: CaseClause| clause.into() } |
        call!(if_clause, parser) => { |clause: IfClause| clause.into() } |
        call!(while_clause, parser) => { |clause: WhileClause| clause.into() } |
        call!(until_clause, parser) => { |clause: WhileClause| clause.into() }
    )
}

fn subshell<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Command> {
    delimited!(input,
        tag_token!(Token::LeftParen),
        call!(compound_list, parser),
        tag_token!(Token::RightParen)
    )
}

fn compound_list<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Command> {
    do_parse!(input,
        // XXX: this is technically an optional newline_list in the spec
        call!(linebreak, parser) >>
        res: map!(call!(term, parser), |and_ors| Command::with_inner(CommandInner::AndOr(and_ors))) >>
        opt!(call!(separator, parser)) >>
        (res)
    )
}

// FIXME: check if this sketchy parsing is still needed
fn term<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Vec<Vec<AndOr>>> {
    //terminated!(separated_nonempty_list_complete!(separator, and_or), opt!(separator))
    do_parse!(input,
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
}

fn term_inner<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Vec<AndOr>> {
    do_parse!(input,
        call!(separator, parser) >>
        and_or: call!(and_or, parser) >>
        (and_or)
    )
}

fn for_clause<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, ForClause> {
    do_parse!(input,
        tag_keyword!("for") >>
        name: owned_name >>
        call!(linebreak, parser) >>
        words: opt!(
            do_parse!(
                tag_keyword!("in") >>
                words: many0!(word) >>
                call!(sequential_sep, parser) >>
                (words)
            )
        ) >>
        do_stmt: call!(do_group, parser) >>
        (ForClause::new(name, words, do_stmt))
    )
}

fn case_clause<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, CaseClause> {
    do_parse!(input,
        tag_keyword!("case") >>
        word: word >>
        call!(linebreak, parser) >>
        tag_keyword!("in") >>
        call!(linebreak, parser) >>
        case_list: many0!(
            alt!(call!(case_item, parser) | call!(case_item_ns, parser))
        ) >>
        tag_keyword!("esac") >>
        (CaseClause::new(word, CaseList::new(case_list)))
    )
}

fn case_item_ns<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, CaseItem> {
    do_parse!(input,
        tag_token!(Token::LeftParen) >>
        pattern: call!(pattern, parser) >>
        tag_token!(Token::RightParen) >>
        list: opt!(call!(compound_list, parser)) >>
        call!(linebreak, parser) >>
        (CaseItem::new(pattern, list))
    )
}

fn case_item<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, CaseItem> {
    do_parse!(input,
        item: call!(case_item_ns, parser) >>
        tag_token!(Token::DoubleSemi) >>
        call!(linebreak, parser) >>
        (item)
    )
}

// TODO: this needs to actually match "patterns" (i believe they are satisfied by globset, but need to figure out how to get nom to read them
//       so they can be given to globset)
fn pattern<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Pattern> {
    do_parse!(input,
        res: map!(
            separated_nonempty_list_complete!(tag_token!(Token::Pipe), word),
            |values| Pattern::new(values)
        ) >>
        (res)
    )
}

fn if_clause<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, IfClause> {
    do_parse!(input,
        tag_keyword!("if") >>
        cond: call!(compound_list, parser) >>
        tag_keyword!("then") >>
        body: call!(compound_list, parser) >>
        else_stmt: opt!(call!(else_part, parser)) >>
        tag_keyword!("fi") >>
        (IfClause::new(cond, body, else_stmt))
    )
}

fn else_part<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, ElseClause> {
    alt!(input,
        do_parse!(
            tag_keyword!("elif") >>
            cond: call!(compound_list, parser) >>
            tag_keyword!("then") >>
            body: call!(compound_list, parser) >>
            else_stmt: call!(else_part, parser) >>
            (ElseClause::new(Some(cond), body, Some(Box::new(else_stmt))))
        ) |
        do_parse!(
            tag_keyword!("else") >>
            body: call!(compound_list, parser) >>
            (ElseClause::new(None, body, None))
        )
    )
}

fn while_clause<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, WhileClause> {
    // FIXME: maybe these clauses should use space1? (they definitely need to, otherwise stuff like whilex is parsed as while x)
    do_parse!(input,
        tag_keyword!("while") >>
        cond: call!(compound_list, parser) >>
        do_stmt: call!(do_group, parser) >>
        (WhileClause::new(cond, true, do_stmt))
    )
}

fn until_clause<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, WhileClause> {
    do_parse!(input,
        tag_keyword!("until") >>
        cond: call!(compound_list, parser) >>
        do_stmt: call!(do_group, parser) >>
        (WhileClause::new(cond, false, do_stmt))
    )
}

fn function_def<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, FunctionDef> {
    do_parse!(input,
        name: fname >>
        tag_token!(Token::LeftParen) >>
        tag_token!(Token::RightParen) >>
        call!(linebreak, parser) >>
        body: call!(function_body, parser) >>
        (FunctionDef::new(name, Rc::new(body)))
    )
}

fn function_body<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, FunctionBody> {
    do_parse!(input,
        // TODO: apply rule 9
        cmd: call!(compound_command, parser) >>
        redir: opt!(call!(redirect_list, parser)) >>
        (FunctionBody::new(cmd, redir))
    )
}

fn fname<'a, 'b>(input: TokenIter<'a, 'b>) -> IResult<'a, 'b, Name> {
    owned_name(input)
}

fn owned_name<'a, 'b>(input: TokenIter<'a, 'b>) -> IResult<'a, 'b, Name> {
    map_opt!(input, take_one, |token| {
        match token {
            &Token::Word(word) => {
                name(word).ok().and_then(|(inp, v)| {
                    if inp.len() > 0 {
                        None
                    } else {
                        Some(OsString::from_vec(v.to_owned()))
                    }
                })
            }
            _ => None,
        }
    })
}

fn brace_group<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Command> {
    delimited!(input,
        tag_keyword!("{"),
        call!(compound_list, parser),
        tag_keyword!("}")
    )
}

fn do_group<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Command> {
    do_parse!(input,
        tag_keyword!("do") >>
        lst: call!(compound_list, parser) >>
        tag_keyword!("done") >>
        (lst)
    )
}

fn simple_command<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, SimpleCommand> {
    alt!(input,
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
}

fn cmd_name<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, CommandName> {
    // TODO: apply rule 7a
    map_opt!(input, word, |word: Word| {
        // TODO: this prob needs to check all keywords (ensure that e.g. in and then are actually not allowed)
        // FIXME: not sure if this should behave differently if the command is surrounded with single quotes
        //        dash gives a syntax error without quotes and a command not found with quotes
        if let Word::Simple(ref text) = word {
            match text.as_os_str().as_bytes() {
                b"if" | b"then" | b"else" | b"elif" | b"fi" | b"do" | b"done" | b"case" | b"esac" | b"while" | b"until" | b"for" | b"{" | b"}" | b"!" | b"in" => return None,
                _ => {}
            }
        }
        Some(word)
    })
}

fn cmd_word<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, CommandName> {
    // TODO: apply rule 7b
    // FIXME: this do_parse should not be necessary
    do_parse!(input, w: word >> (w))
}

fn cmd_prefix<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Vec<PreAction>> {
    many1!(input,
        alt!(
            call!(io_redirect, parser) => { |redir| Either::Left(redir) } |
            call!(var_assign, parser) => { |assign| Either::Right(assign) }
        )
    )
}

fn cmd_suffix<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Vec<PostAction>> {
    many1!(input,
        alt!(
            call!(io_redirect, parser) => { |redir| Either::Left(redir) } |
            word => { |word| Either::Right(word) }
        )
    )
}

fn redirect_list<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Vec<IoRedirect>> {
    many1!(input, call!(io_redirect, parser))
}

fn io_redirect<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, IoRedirect> {
    do_parse!(input,
        num: opt!(io_number) >>
        res: alt!(
            io_file => { |file| IoRedirect::File(num, file) } |
            io_here => { |here| IoRedirect::Heredoc(num, here) }
        ) >>
        (res)
    )
}

fn io_here<'a, 'b>(input: TokenIter<'a, 'b>) -> IResult<'a, 'b, Rc<RefCell<HereDoc>>> {
    map_opt!(input, take_one, |token| {
        match token {
            &Token::HereDoc(ref heredoc) => Some(heredoc.clone()),
            _ => None,
        }
    })
}

fn command_subst<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, CommandSubst> {
    map_opt!(input, take_one, |token| {
        match token {
            &Token::CommandSubst(ref tokens) => {
                let iter = TokenIter::new(&tokens);
                 complete!(iter, call!(command, parser)).ok().map(|(_, val)| CommandSubst::new(Box::new(val)))
            }
            _ => None
        }
    })
}

// FIXME: just realized there is an issue with the parser state (if something in an alt!() fails
//        that increments linenum, it will stay incremented).  this may be unlikely however as i
//        think it would be uncommon for something to match a newline but then fail later
fn linebreak<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, Option<&'a Token<'b>>> {
    opt!(input, call!(newline_list, parser))
}

// FIXME: should just iterate
fn newline_list<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, &'a Token<'b>> {
    do_parse!(input,
        res: map!(tag_token!(Token::Newline), |res| {
            parser.linenum += 1;
            res
        }) >>
        opt!(call!(newline_list, parser)) >>
        (res)
    )
}

// XXX: is & just run in background for this? (answer is yes, so need to keep track of which one was given)
fn separator_op<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, &'a Token<'b>> {
    alt!(input, tag_token!(Token::Background) | tag_token!(Token::CommandEnd))
}

fn separator<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, &'a Token<'b>> {
    alt!(input,
        do_parse!(
            sep: call!(separator_op, parser) >>
            call!(linebreak, parser) >>
            (sep)
        ) |
        call!(newline_list, parser)
    )
}

// FIXME: pretty sure &'a [u8] is wrong
fn sequential_sep<'a, 'b>(input: TokenIter<'a, 'b>, parser: &mut Parser) -> IResult<'a, 'b, &'a Token<'b>> {
    alt!(input, terminated!(tag_token!(Token::CommandEnd), call!(linebreak, parser)) | call!(newline_list, parser))
}

// XXX: list of things that use word and accept patterns: wordlist (used by for), case_item/case_item_ns, cmd_name, cmd_word, cmd_suffix, cmd_prefix (the right side)
//      things that do not accept patterns (afaict): case WORD in, name (used by e.g. for, fname), here_end, filename
//      Pattern/Glob and Text should probably be separate parts of WordPart enum because of this (or we could just store in Text and then always run a glob for things that accept globs)
//      we are going with the second option as it's easier to parse
fn word<'a, 'b>(input: TokenIter<'a, 'b>) -> IResult<'a, 'b, Word> {
    map_opt!(input, take_one, |token| word_handler(token))
}

fn word_handler<'a>(token: &Token<'a>) -> Option<Word> {
    match token {
        &Token::Parameter(ref expr) => param_expr_handler(expr),
        &Token::CommandSubst(ref subst) => command_subst_handler(subst),
        &Token::SingleQuote(quote) => {
            Some(Word::SingleQuote(OsString::from_vec(quote.to_owned())))
        }
        &Token::DoubleQuote(ref quote) => {
            double_quote_handler(quote)
        }
        &Token::Word(word) => {
            Some(Word::Simple(OsString::from_vec(word.to_owned())))
        }
        &Token::ComplexWord(ref word) => {
            complex_word(word.iter())
        }
        _ => None,
    }
}

fn param_expr_handler<'a>(expr: &ParamExprToken<'a>) -> Option<Word> {
    use super::lexer::ParamExprKind::*;

    match expr.kind {
        Value if expr.rhs.is_empty() => {
            Some(Word::Parameter(ParamExpr::new(expr.param.to_param(), ParamExprKind::Value)))
        }
        _ => unimplemented!(),
    }
}

fn command_subst_handler<'a>(tokens: &[Token<'a>]) -> Option<Word> {
    unimplemented!()
}

fn double_quote_handler<'a>(parts: &[DoubleQuotePart<'a>]) -> Option<Word> {
    use self::DoubleQuotePart::*;

    let mut words = vec![];

    for part in parts {
        words.push(match part {
            &Parameter(ref expr) => param_expr_handler(expr)?,
            &CommandSubst(ref subst) => command_subst_handler(subst)?,
            ArithExpr => {
                unimplemented!()
            }
            Escape(ch) => {
                unimplemented!()
            }
            Text(text) => Word::SingleQuote(OsString::from_vec(text.to_vec())),
        });
    }

    Some(Word::DoubleQuote(DoubleQuote::new(words)))
}

fn complex_word<'a: 'b, 'b, I: Iterator<Item = &'b Token<'a>>>(tokens: I) -> Option<Word> {
    let mut parts = vec![];
    for token in tokens {
        parts.push(word_handler(token)?);
    }
    Some(Word::Complex(parts))
}

fn io_file<'a, 'b>(input: TokenIter<'a, 'b>) -> IResult<'a, 'b, IoRedirectFile> {
    do_parse!(input,
        kind: map_opt!(take_one, |token| match token {
            &Token::IoRedirect(kind) => Some(kind),
            _ => None,
        }) >>
        name: filename >>
        (IoRedirectFile::new(kind, name))
    )
}

fn filename<'a, 'b>(input: TokenIter<'a, 'b>) -> IResult<'a, 'b, Word> {
    // TODO: apply rule 2
    // FIXME: this do_parse should not be necessary
    word(input)
}

// NOTE: we only accept fds 0-9 as that is the minimum required (and seems to be the number
//       supported several shells).  if this ever changes, FD_COUNT in option.rs must be changed as
//       well
fn io_number<'a, 'b>(input: TokenIter<'a, 'b>) -> IResult<'a, 'b, RawFd> {
    map_opt!(input, take_one, |token| {
        match token {
            &Token::Word(num) if num.len() == 1 && nom::is_digit(num[0]) => {
                Some((num[0] as char).to_digit(10).unwrap() as RawFd)
            }
            _ => None
        }
    })
}
