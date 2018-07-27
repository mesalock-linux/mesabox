use nom::{self, alpha1, alphanumeric1, digit1, space0, space1, newline};

use std::cell::RefCell;
use std::ffi::{OsString, OsStr};
use std::fmt;
use std::os::unix::ffi::{OsStringExt, OsStrExt};
use std::os::unix::io::RawFd;
use std::rc::Rc;

use super::ast::*;

macro_rules! tag_token {
    ($i:expr, $tag:expr) => { call!($i, tag_token, $tag) }
}

macro_rules! tag_keyword {
    ($i:expr, $tag:expr) => { call!($i, tag_keyword, $tag) }
}

macro_rules! clean_tag {
    ($i:expr, $tag:expr) => { fix_error!($i, ParserError, tag!($tag)) }
}

// XXX: we need to redefine the following two macros as they hardcode the error to u32 normally
macro_rules! complete (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        {
            use ::std::result::Result::*;
            use $crate::nom::{Err, ErrorKind};

            let i_ = $i.clone();
            match $submac!(i_, $($args)*) {
                Err(Err::Incomplete(_)) =>  {
                    Err(Err::Error(error_position!($i, ErrorKind::Complete::<ParserError>)))
                },
                rest => rest
            }
        }
    );
    ($i:expr, $f:expr) => (
        complete!($i, call!($f));
    );
);

macro_rules! separated_nonempty_list_complete {
    ($i:expr, $sep:ident!( $($args:tt)* ), $submac:ident!( $($args2:tt)* )) => ({
        separated_nonempty_list!($i, complete!($sep!($($args)*)), complete!($submac!($($args2)*)))
    });

    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => (
        separated_nonempty_list_complete!($i, $submac!($($args)*), call!($g));
    );
    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => (
        separated_nonempty_list_complete!($i, call!($f), $submac!($($args)*));
    );
    ($i:expr, $f:expr, $g:expr) => (
        separated_nonempty_list_complete!($i, call!($f), call!($g));
    );
}

// XXX: redefining this as it fails with mutable arguments
macro_rules! return_error (
    ($i:expr, $code:expr, $submac:ident!( $($args:tt)* )) => (
        {
            use ::std::result::Result::*;
            use $crate::nom::{Context,Err,ErrorKind};

            let i_ = $i.clone();
            let mut cl = || {
                $submac!(i_, $($args)*)
            };

            fn unify_types<I,E>(_: &Context<I,E>, _: &Context<I,E>) {}

            match cl() {
                Err(Err::Incomplete(x)) => Err(Err::Incomplete(x)),
                Ok((i, o))              => Ok((i, o)),
                Err(Err::Error(e)) | Err(Err::Failure(e)) => {
                unify_types(&e, &Context::Code($i, $code));
                return Err(Err::Failure(error_node_position!($i, $code, e)))
                }
            }
        }
    );
    ($i:expr, $code:expr, $f:expr) => (
        return_error!($i, $code, call!($f));
    );
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        {
            use ::std::result::Result::*;
            use $crate::nom::{Context,Err,ErrorKind};

            let i_ = $i.clone();
            let cl = || {
                $submac!(i_, $($args)*)
            };

            fn unify_types<I,E>(_: &Context<I,E>, _: &Context<I,E>) {}

            match cl() {
                Err(Err::Incomplete(x)) => Err(Err::Incomplete(x)),
                Ok((i, o))              => Ok((i, o)),
                Err(Err::Error(e)) | Err(Err::Failure(e)) => {
                return Err(Err::Failure(e))
                }
            }
        }
    );
    ($i:expr, $f:expr) => (
        return_error!($i, call!($f));
    );
);

type IResult<'a, O> = nom::IResult<&'a [u8], O, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    If,
    Case,
    For,
    While,
    Until,
    BraceGroup,
    SubShell,
    FunctionDef,
    ParamExpand,
    CommandSubst,
    DoubleQuote,
    SingleQuote,
    Token(&'static str),
    Keyword(&'static str),

    // XXX: this should never happen but is needed to make nom happy
    Other(u32)
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ParserError::*;

        let args = match self {
            If => "in if",
            Case => "in case",
            For => "in for",
            While => "in while",
            Until => "in until",
            BraceGroup => "in brace group ({{ ... }})",
            SubShell => "in subshell",
            FunctionDef => "in function definition",
            ParamExpand => "in parameter expansion",
            CommandSubst => "in command substitution",
            DoubleQuote => "in double quote",
            SingleQuote => "in single quote",
            Token(tok) => return write!(f, "expected token {}", tok),
            Keyword(keyword) => return write!(f, "expected keyword {}", keyword),

            // XXX: if this is reachable the parser is broken
            Other(_) => unreachable!()
        };
        write!(f, "{}", args)
    }
}

// XXX: exists to make nom happy
impl From<u32> for ParserError {
    fn from(num: u32) -> Self {
        ParserError::Other(num)
    }
}

struct HereDocMarker {
    marker: HereDocWord,
    strip_tabs: bool,
    heredoc: Rc<RefCell<HereDoc>>,
}

// NOTE: for better or worse, the heredoc ending word seems to be pretty much literal (it removes quotes but does not expand anything)
//       we thus need to process it specially
pub type HereDocWord = OsString;

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

    // XXX: rather than collect all available commands, it may make more sense for this to parse
    //      one line (or, if the command is split over multiple, however many lines are needed to
    //      parse one command) and then make script parse everything BUT execute stuff line-by-line
    //      (or more accurately command-by-command)
    // NOTE: afaict, the shell should execute each full line/command *until* it hits a line that
    //       can't be parsed or we reach the end of the file (so we can't do what we do now which
    //       is fail without doing anything if any line is wrong, although maybe this could be an
    //       shopt extension?)
    pub fn complete_command<'a>(mut self, input: &'a [u8]) -> (Self, IResult<'a, CompleteCommand>) {
        let res = do_parse!(input,
            ignore >>
            call!(linebreak, &mut self) >>
            res: separated_nonempty_list_complete!(call!(separator, &mut self), call!(list, &mut self)) >>
            //eof!() >>
            (CompleteCommand::new(res))
        );
        (self, res)
    }
}

fn tag_token<'a>(input: &'a [u8], token: &'static str) -> IResult<'a, &'a [u8]> {
    add_return_error!(input, ErrorKind::Custom(ParserError::Token(token)), terminated!(clean_tag!(token), ignore))
}

fn tag_keyword<'a>(input: &'a [u8], keyword: &'static str) -> IResult<'a, &'a [u8]> {
    add_return_error!(input, ErrorKind::Custom(ParserError::Keyword(keyword)), terminated!(clean_tag!(keyword), delimiter))
}

fn list<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Vec<Vec<AndOr>>> {
    separated_nonempty_list_complete!(input, separator_op, call!(and_or, parser))
}

fn and_or<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Vec<AndOr>> {
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

fn and_or_inner<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, AndOr> {
    do_parse!(input,
        sep: call!(and_or_sep, parser) >>
        pipe: call!(pipeline, parser) >>
        (AndOr::new(pipe, sep))
    )
}

fn and_or_sep<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, SepKind> {
    do_parse!(input,
        res: alt!(
            tag_token!("&&") => { |_| SepKind::And } |
            tag_token!("||") => { |_| SepKind::Or }
        ) >>
        call!(linebreak, parser) >>
        (res)
    )
}

named!(var_name<&[u8], OsString, ParserError>,
    call!(name)
);

fn var_assign<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, VarAssign> {
    do_parse!(input,
        name: var_name >>
        clean_tag!("=") >>
        expr: opt!(call!(word, parser)) >>
        cond!(expr.is_none(), ignore) >>
        (VarAssign { varname: name, value: expr })
    )
}

fn parameter<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, ParamExpr> {
    do_parse!(input,
        clean_tag!("$") >>
        res: alt!(
            preceded!(
                clean_tag!("{"),
                return_error!(ErrorKind::Custom(ParserError::ParamExpand), terminated!(call!(param_expr, parser), clean_tag!("}")))
            ) |
            map!(param_name, |name| ParamExpr::new(name, ParamExprKind::Value))
        ) >>
        (res)
    )
}

fn param_expr<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, ParamExpr> {
    alt!(input,
        do_parse!(
            clean_tag!("#") >>
            name: map!(opt!(param_name), |name| {
                // NOTE: this is to be compatible with dash (the standard doesn't explicitly say
                //       what to do in this case)
                name.unwrap_or_else(|| Param::Var(OsString::new()))
            }) >>
            (ParamExpr::new(name, ParamExprKind::Length))
        ) |
        do_parse!(
            name: param_name >>
            res: call!(param_subst, parser, name) >>
            (res)
        ) |
        map!(param_name, |name| ParamExpr::new(name, ParamExprKind::Value))
    )
}

fn param_subst<'a>(input: &'a [u8], parser: &mut Parser, name: Param) -> IResult<'a, ParamExpr> {
    map!(input, alt!(
        preceded!(
            clean_tag!(":"),
            map!(call!(param_subst_value, parser), |kind| {
                match kind {
                    ParamExprKind::AssignNull(value) => ParamExprKind::Assign(value),
                    ParamExprKind::UseNull(value) => ParamExprKind::Use(value),
                    ParamExprKind::ErrorNull(value) => ParamExprKind::Error(value),
                    ParamExprKind::Alternate(value) => ParamExprKind::AlternateNull(value),
                    _ => unreachable!()
                }
            })
        ) |
        preceded!(
            clean_tag!("%"),
            map!(call!(param_subst_suffix, parser), |kind| {
                if let ParamExprKind::SmallSuffix(value) = kind {
                    ParamExprKind::LargeSuffix(value)
                } else {
                    unreachable!()
                }
            })
        ) |
        preceded!(
            clean_tag!("#"),
            map!(call!(param_subst_prefix, parser), |kind| {
                if let ParamExprKind::SmallPrefix(value) = kind {
                    ParamExprKind::LargePrefix(value)
                } else {
                    unreachable!()
                }
            })
        ) |
        call!(param_subst_value, parser) |
        call!(param_subst_suffix, parser) |
        call!(param_subst_prefix, parser)
    ), |kind| {
        ParamExpr::new(name, kind)
    })
}

// FIXME: word may be incorrect as dash accepts ${var:=hello there} (i.e. two words)
//        maybe just take text until the brace and interpret after reading?
fn param_subst_value<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, ParamExprKind> {
    alt!(input,
        preceded!(
            clean_tag!("-"),
            map!(call!(param_value, parser), |val| ParamExprKind::UseNull(val))
        ) |
        preceded!(
            clean_tag!("="),
            map!(call!(param_value, parser), |val| ParamExprKind::AssignNull(val))
        ) |
        preceded!(
            clean_tag!("?"),
            map!(opt!(call!(param_value, parser)), |val| ParamExprKind::ErrorNull(val))
        ) |
        preceded!(
            clean_tag!("+"),
            map!(call!(param_value, parser), |val| ParamExprKind::Alternate(val))
        )
    )
}

fn param_subst_prefix<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, ParamExprKind> {
    preceded!(input,
        clean_tag!("#"),
        map!(call!(param_value, parser), |val| ParamExprKind::SmallPrefix(val))
    )
}

fn param_subst_suffix<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, ParamExprKind> {
    preceded!(input,
        clean_tag!("%"),
        map!(call!(param_value, parser), |val| ParamExprKind::SmallSuffix(val))
    )
}

// FIXME: the names for *, ?, and @ suck
named!(param_name<&[u8], Param, ParserError>,
    alt!(
        var_name => { |name| Param::Var(name) } |
        clean_tag!("*") => { |_| Param::Star } |
        clean_tag!("?") => { |_| Param::Question } |
        clean_tag!("@") => { |_| Param::At } |
        clean_tag!("#") => { |_| Param::NumParams } |
        clean_tag!("$") => { |_| Param::ShellPid } |
        clean_tag!("!") => { |_| Param::BackgroundPid } |
        number => { |number| Param::Positional(number) }
    )
);

fn param_value<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Box<Word>> {
    map!(input, call!(word, parser), |val| Box::new(val))
}

named!(number<&[u8], usize, ParserError>,
    map_opt!(
        terminated!(fix_error!(ParserError, digit1), ignore),
        |res| {
            use std::str;

            str::from_utf8(res).ok().and_then(|s| s.parse().ok())
        }
    )
);

fn pipe_seq<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Pipeline> {
    map!(input, separated_nonempty_list_complete!(call!(pipe_sep, parser), call!(command, parser)), |vec| {
        vec.into_iter().collect()
    })
}

fn pipe_sep<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, &'a [u8]> {
    terminated!(input, tag_token!("|"), call!(linebreak, parser))
}

fn pipeline<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Pipeline> {
    do_parse!(input,
        bang: opt!(tag_keyword!("!")) >>
        seq: call!(pipe_seq, parser) >>
        ({ let mut seq = seq; seq.bang = bang.is_some(); seq })
    )
}

fn command<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Command> {
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

fn compound_command<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Command> {
    alt!(input,
        preceded!(
            tag_keyword!("{"),
            return_error!(ErrorKind::Custom(ParserError::BraceGroup), call!(brace_group, parser))
        ) |
        preceded!(
            tag_token!("("),
            return_error!(ErrorKind::Custom(ParserError::SubShell), call!(subshell, parser))
        ) |
        preceded!(
            tag_keyword!("for"),
            return_error!(ErrorKind::Custom(ParserError::For), call!(for_clause, parser))
        ) => { |clause: ForClause| clause.into() } |
        preceded!(
            tag_keyword!("case"),
            return_error!(ErrorKind::Custom(ParserError::Case), call!(case_clause, parser))
        ) => { |clause: CaseClause| clause.into() } |
        preceded!(
            tag_keyword!("if"),
            return_error!(ErrorKind::Custom(ParserError::If), call!(if_clause, parser))
        ) => { |clause: IfClause| clause.into() } |
        preceded!(
            tag_keyword!("while"),
            return_error!(ErrorKind::Custom(ParserError::While), call!(while_clause, parser, true))
        ) => { |clause: WhileClause| clause.into() } |
        preceded!(
            tag_keyword!("until"),
            return_error!(ErrorKind::Custom(ParserError::Until), call!(while_clause, parser, false))
        ) => { |clause: WhileClause| clause.into() }
    )
}

fn subshell<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Command> {
    map!(input, terminated!(
        call!(compound_list_inner, parser),
        tag_token!(")")
    ), |inner| {
        if let CommandInner::AndOr(and_ors) = inner {
            Command::with_inner(CommandInner::SubShell(and_ors))
        } else {
            unreachable!()
        }
    })
}

fn compound_list<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Command> {
    map!(input, call!(compound_list_inner, parser), |inner| Command::with_inner(inner))
}

fn compound_list_inner<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, CommandInner> {
    do_parse!(input,
        opt!(call!(newline_list, parser)) >>
        res: map!(call!(term, parser), |and_ors| CommandInner::AndOr(and_ors)) >>
        opt!(preceded!(not!(term_separator), call!(separator, parser))) >>
        (res)
    )
}

// XXX: verify this works (needed some sketchy parsing before)
fn term<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Vec<Vec<AndOr>>> {
    separated_nonempty_list!(input, preceded!(not!(term_separator), call!(separator, parser)), call!(and_or, parser))
    // NOTE: if the above doesn't work, the below should
    /*let (mut input, first) = try_parse!(input, call!(and_or, parser));
    let mut and_ors = vec![first];
    loop {
        if let Ok((new_input, _)) = preceded!(input, not!(term_separator), call!(separator, parser)) {
            if let Ok((new_input, value)) = call!(new_input, and_or, parser) {
                input = new_input;
                and_ors.push(value);
                continue;
            }
        }
        break;
    }
    Ok((input, and_ors))*/
}

// FIXME: the only reason this needs to be done is because we don't tokenize the input
fn term_separator<'a>(input: &'a [u8]) -> IResult<'a, &'a [u8]> {
    // thus far the only item that seems to confuse the parser is DSEMI
    clean_tag!(input, ";;")
}

fn for_clause<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, ForClause> {
    do_parse!(input,
        name: name >>
        call!(linebreak, parser) >>
        words: opt!(
            do_parse!(
                tag_keyword!("in") >>
                words: many0!(call!(word, parser)) >>
                call!(sequential_sep, parser) >>
                (words)
            )
        ) >>
        do_stmt: call!(do_group, parser) >>
        (ForClause::new(name, words, do_stmt))
    )
}

// FIXME: i think this will split like name @ on name@ (dunno if that's wrong though?)
named!(name<&[u8], Name, ParserError>,
    do_parse!(
        val: fix_error!(ParserError, map!(
            recognize!(
                pair!(
                    alt!(alpha1 | tag!("_")),
                    many0!(alt!(alphanumeric1 | tag!("_")))
                )
            ),
            |res| OsString::from_vec(res.to_owned())
        )) >>
        ignore >>
        (val)
    )
);

fn case_clause<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, CaseClause> {
    do_parse!(input,
        word: call!(word, parser) >>
        call!(linebreak, parser) >>
        tag_keyword!("in") >>
        ignore >>
        call!(linebreak, parser) >>
        // XXX: we can add return_error!() here
        case_list: alt!(
            do_parse!(
                list: many1!(call!(case_item, parser)) >>
                last_case: opt!(call!(case_item_ns, parser)) >>
                ({
                    let mut list = list;
                    if let Some(case) = last_case {
                        list.push(case);
                    }
                    list
                })
            ) |
            map!(call!(case_item_ns, parser), |case| vec![case])
        ) >>
        tag_keyword!("esac") >>
        ignore >>
        (CaseClause::new(word, CaseList::new(case_list)))
    )
}

fn case_item_common<'a, F>(input: &'a [u8], parser: &mut Parser, func: F) -> IResult<'a, CaseItem>
where
    F: Fn(&'a [u8], &mut Parser) -> IResult<'a, Option<Command>>,
{
    do_parse!(input,
        alt!(
            tag_token!("(") => { |_| () } |
            not!(tag_keyword!("esac"))
        ) >>
        pattern: call!(pattern, parser) >>
        tag_token!(")") >>
        list: call!(func, parser) >>
        call!(linebreak, parser) >>
        (CaseItem::new(pattern, list))
    )
}

fn case_item_ns<'a>(inp: &'a [u8], p: &mut Parser) -> IResult<'a, CaseItem> {
    case_item_common(inp, p, |input, parser| {
        opt!(input, call!(compound_list, parser))
    })
}

fn case_item<'a>(inp: &'a [u8], p: &mut Parser) -> IResult<'a, CaseItem> {
    case_item_common(inp, p, |input, parser| {
        terminated!(input,
            alt!(
                call!(compound_list, parser) => { |res| Some(res) } |
                call!(linebreak, parser) => { |_| None }
            ),
            tag_token!(";;")
        )
    })
}

// TODO: this needs to actually match "patterns" rather than words
fn pattern<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Pattern> {
    map!(input,
        separated_nonempty_list_complete!(tag_token!("|"), call!(word, parser)),
        |values| Pattern::new(values)
    )
}

fn if_clause<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, IfClause> {
    do_parse!(input,
        cond: call!(compound_list, parser) >>
        tag_keyword!("then") >>
        body: call!(compound_list, parser) >>
        else_stmt: opt!(call!(else_part, parser)) >>
        tag_keyword!("fi") >>
        (IfClause::new(cond, body, else_stmt))
    )
}

fn else_part<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, ElseClause> {
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

fn while_clause<'a>(input: &'a [u8], parser: &mut Parser, check: bool) -> IResult<'a, WhileClause> {
    do_parse!(input,
        cond: call!(compound_list, parser) >>
        do_stmt: call!(do_group, parser) >>
        (WhileClause::new(cond, check, do_stmt))
    )
}

fn function_def<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, FunctionDef> {
    do_parse!(input,
        name: fname >>
        tag_token!("()") >>
        call!(linebreak, parser) >>
        body: return_error!(ErrorKind::Custom(ParserError::FunctionDef), call!(function_body, parser)) >>
        (FunctionDef::new(name, Rc::new(body)))
    )
}

fn function_body<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, FunctionBody> {
    do_parse!(input,
        // TODO: apply rule 9
        cmd: call!(compound_command, parser) >>
        redir: opt!(call!(redirect_list, parser)) >>
        (FunctionBody::new(cmd, redir))
    )
}

named!(fname<&[u8], Name, ParserError>,
    call!(name)
);

fn brace_group<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Command> {
    terminated!(input,
        call!(compound_list, parser),
        tag_keyword!("}")
    )
}

fn do_group<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Command> {
    do_parse!(input,
        tag_keyword!("do") >>
        lst: call!(compound_list, parser) >>
        tag_keyword!("done") >>
        (lst)
    )
}

fn simple_command<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, SimpleCommand> {
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

fn cmd_name<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, CommandName> {
    // TODO: apply rule 7a
    map_opt!(input, call!(word, parser), |word: Word| {
        // XXX: need to ensure all reserved words are being checked (a command name is not allowed
        //      to be exactly a reserved word unless it is quoted)
        if let Word::Simple(ref text) = word {
            match text.as_os_str().as_bytes() {
                b"do" | b"done" | b"for" | b"if" | b"elif" | b"else" | b"fi" | b"while" | b"until" | b"case" | b"esac" | b"in" | b"then" | b"{" | b"}" | b"!" => return None,
                _ => {}
            }
        }
        Some(word)
    })
}

fn cmd_word<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, CommandName> {
    // TODO: apply rule 7b
    word(input, parser)
}

fn cmd_prefix<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Vec<PreAction>> {
    many1!(input,
        alt!(
            call!(io_redirect, parser) => { |redir| PreAction::IoRedirect(redir) } |
            call!(var_assign, parser) => { |assign| PreAction::VarAssign(assign) }
        )
    )
}

fn cmd_suffix<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Vec<PostAction>> {
    many1!(input,
        alt!(
            call!(io_redirect, parser) => { |redir| PostAction::IoRedirect(redir) } |
            call!(word, parser) => { |word| PostAction::Word(word) }
        )
    )
}

fn redirect_list<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Vec<IoRedirect>> {
    many1!(input, call!(io_redirect, parser))
}

fn io_redirect<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, IoRedirect> {
    alt!(input,
        do_parse!(
            num: opt!(io_number) >>
            file: call!(io_file, parser) >>
            (IoRedirect::File(num, file))
        ) |
        do_parse!(
            num: opt!(io_number) >>
            here: call!(io_here, parser) >>
            (IoRedirect::Heredoc(num, here))
        )
    )
}

fn io_here<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Rc<RefCell<HereDoc>>> {
    do_parse!(input,
        clean_tag!("<<") >>
        dash: opt!(clean_tag!("-")) >>
        ignore >>
        delim: here_end >>
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
}

named!(here_end<&[u8], HereDocWord, ParserError>,
    // TODO: apply rule 3
    // FIXME: this is wrong (need to do quote removal and such)
    map!(fix_error!(ParserError, alphanumeric1), |res| OsString::from_vec(res.to_owned()))
);

// this needs to read a heredoc until it encounters "\nEND MARKER\n"
// XXX: this could probably use some improvement
fn parse_heredoc<'a>(mut input: &'a [u8], parser: &mut Parser) -> IResult<'a, ()> {
    for marker in &mut parser.heredoc_markers {
        let mut heredoc = marker.heredoc.borrow_mut();
        let res = do_parse!(input,
            res: fold_many1!(
                do_parse!(
                    not!(tuple!(clean_newline, clean_tag!(marker.marker.as_bytes()), clean_newline)) >>
                    val: fix_error!(ParserError, take!(1)) >>
                    (val)
                ),
                vec![],
                |mut acc: Vec<_>, item: &[u8]| {
                    acc.extend(item.iter());
                    acc
                }
            ) >>
            nl: clean_newline >>
            clean_tag!(marker.marker.as_bytes()) >>
            clean_newline >>
            ({ let mut res = res; res.push(nl as _); res })
         )?;
        input = res.0;
        heredoc.data = res.1.to_owned();
    }
    parser.heredoc_markers.clear();

    Ok((input, ()))
}

fn command_subst<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, CommandSubst> {
    alt!(input, call!(command_subst_dollar, parser) | call!(command_subst_backtick, parser))
}

// FIXME: it seems stuff like $(NEWLINE cmd NEWLINE) should be supported, so ensure it is
fn command_subst_dollar<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, CommandSubst> {
    do_parse!(input,
        fix_error!(ParserError, tag!("$(")) >>
        fix_error!(ParserError, not!(tag!("("))) >>        // avoid ambiguity between subshell and arithmetic expression
        ignore >>
        cmd: return_error!(ErrorKind::Custom(ParserError::CommandSubst), terminated!(call!(compound_list, parser), tag_token!(")"))) >>
        (CommandSubst::new(Box::new(cmd)))
    )
}

// XXX: strategy should probably be collect input until first unescaped backtick (handling escaped dollar signs and such by perhaps adding single quotes around them and then feeding them to command for processing)
fn command_subst_backtick<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, CommandSubst> {
    do_parse!(input,
        tag_token!("`") >>
        // TODO
        tag_token!("`") >>
        (unimplemented!())
    )
}
/*
named_args!(arith_expr<'a>(parser: &mut Parser)<&'a [u8], ArithExpr>,
    do_parse!(
        tag!("$((") >>
        ignore >>
        
        tag!("))") >>
        ignore
        ()
    )
);
*/
named!(single_quote<&[u8], Word, ParserError>,
    delimited!(
        clean_tag!("'"),
        map!(
            fix_error!(ParserError, take_until!("'")),
            |res| Word::SingleQuote(OsString::from_vec(res.to_owned()))
        ),
        clean_tag!("'")
    )
);

fn double_quote<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, DoubleQuote> {
    delimited!(input,
        clean_tag!("\""),
        return_error!(
            ErrorKind::Custom(ParserError::DoubleQuote),
            map!(
                many0!(
                    alt!(
                        map!(call!(parameter, parser), |res| Word::Parameter(res)) |
                        // XXX: i think there will be special rules for the backtick version when
                        //      in quotes
                        map!(call!(command_subst, parser), |res| Word::CommandSubst(res)) |
                        text_or_escape_seq => { |text| Word::SingleQuote(text) }
                        // TODO: arith expr
                    )
                ), |res| DoubleQuote::new(res)
            )
                ),
                clean_tag!("\"")
            )
}

named!(text_or_escape_seq<&[u8], OsString, ParserError>,
    fold_many1!(
        fix_error!(ParserError, alt!(
            preceded!(
                peek!(tag!(r"\")),
                alt!(
                    preceded!(
                        take!(1),
                alt!(
                    recognize!(one_of!("$`\"\\")) |
                    map!(newline, |_| &b""[..])
                )
            ) |
                    take!(2)
                )
            ) |
            is_not!("\"$`\\")
        )),
        OsString::new(),
        |mut acc: OsString, text| {
            acc.push(OsStr::from_bytes(text));
            acc
        }
    )
);

fn linebreak<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Option<&'a [u8]>> {
    opt!(input, call!(newline_list, parser))
}

// XXX: not sure if this is gonna make a Vec (which we don't want as we don't use the result)
fn newline_list<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, &'a [u8]> {
    // XXX: spec doesn't actually seem to give value of newline (am guessing for portability)
    recognize!(input, many1!(tuple!(clean_newline, cond!(!parser.heredoc_markers.is_empty(), call!(parse_heredoc, parser)), ignore)))
}

// XXX: is & just run in background for this? (answer is yes, so need to keep track of which one was given)
named!(separator_op<&[u8], &[u8], ParserError>,
    alt!(tag_token!("&") | tag_token!(";"))
);

fn separator<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, &'a [u8]> {
    alt!(input, recognize!(pair!(separator_op, call!(linebreak, parser))) | call!(newline_list, parser))
}

fn sequential_sep<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, &'a [u8]> {
    alt!(input, recognize!(pair!(tag_token!(";"), call!(linebreak, parser))) | call!(newline_list, parser))
}

// XXX: list of things that use word and accept patterns: wordlist (used by for), case_item/case_item_ns, cmd_name, cmd_word, cmd_suffix, cmd_prefix (the right side)
//      things that do not accept patterns (afaict): case WORD in, name (used by e.g. for, fname), here_end, filename
fn word<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Word> {
    // TODO: this might need to be able to parse quoted strings/words following different rules
    do_parse!(input,
        val: fold_many1!(
            alt!(
                single_quote |
                map!(call!(double_quote, parser), |quote| Word::DoubleQuote(quote)) |
                map!(call!(parameter, parser), |param| Word::Parameter(param)) |
                map!(call!(command_subst, parser), |subst| Word::CommandSubst(subst)) |
                // XXX: there may be more that need to be checked for
                // FIXME: this should not ignore "}" (this is why i likely need to tokenize first)
                // FIXME: if there is a word like \\LINE CONTINUATIONhi, the result will be two
                //        words rather than one (i.e. \ and hi instead of \hi), which is wrong
                preceded!(not!(line_continuation), alt!(
                    map!(preceded!(clean_tag!(r"\"), fix_error!(ParserError, take!(1))), |res| Word::Simple(OsString::from_vec(res.to_owned()))) |
                    map!(many1!(preceded!(not!(delimiter), fix_error!(ParserError, map!(take!(1), |arr| arr[0])))), |res| {
                        Word::Simple(OsString::from_vec(res))
                    })
                ))
            ),
            vec![],
            |mut acc: Vec<_>, item| {
                // this placates the borrow checker
                loop {
                    if let Word::Simple(text) = item {
                        if let Some(Word::Simple(ref mut prev_text)) = acc.last_mut() {
                            prev_text.push(&text);
                            break;
                        }
                        acc.push(Word::Simple(text));
                    } else {
                        acc.push(item);
                    }
                    break;
                }
                acc
            }
        ) >>
        ignore >>
        ({
            if val.len() == 1 {
                val.into_iter().next().unwrap()
            } else {
                Word::Complex(val)
            }
        })
    )
}

fn io_file<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, IoRedirectFile> {
    do_parse!(input,
        kind: alt!(
            tag_token!("<&") => { |_| IoRedirectKind::DupInput } |
            tag_token!("<>") => { |_| IoRedirectKind::ReadWrite } |
            tag_token!("<") => { |_| IoRedirectKind::Input } |
            tag_token!(">&") => { |_| IoRedirectKind::DupOutput } |
            tag_token!(">>") => { |_| IoRedirectKind::Append } |
            tag_token!(">|") => { |_| IoRedirectKind::Clobber } |
            tag_token!(">") => { |_| IoRedirectKind::Output }
        ) >>
        name: call!(filename, parser) >>
        (IoRedirectFile::new(kind, name))
    )
}

fn filename<'a>(input: &'a [u8], parser: &mut Parser) -> IResult<'a, Word> {
    // TODO: apply rule 2
    word(input, parser)
}

// NOTE: we only accept fds 0-9 as that is the minimum required (and seems to be the number
//       supported several shells).  if this ever changes, FD_COUNT in option.rs must be changed as
//       well
named!(io_number<&[u8], RawFd, ParserError>,
    map!(verify!(map!(fix_error!(ParserError, take!(1)), |res: &[u8]| res[0]), |byte| {
        nom::is_digit(byte)
    }), |byte| {
        (byte as char).to_digit(10).unwrap() as RawFd
    })
);

named!(delimiter<&[u8], &[u8], ParserError>,
    alt!(terminated!(delimiter_no_op, opt!(op)) | op)
);

named!(delimiter_no_op<&[u8], &[u8], ParserError>,
    recognize!(
        pair!(
            alt!(fix_error!(ParserError, space1) | line_continuation | comment | fix_error!(ParserError, eof!())),
            opt!(delimiter_no_op)
        )
    )
);

named!(line_continuation<&[u8], &[u8], ParserError>,
    recognize!(
        pair!(
            clean_tag!(r"\"),
            clean_newline
        )
    )
);

named!(comment<&[u8], &[u8], ParserError>,
    recognize!(
        pair!(
            clean_tag!("#"),
            // FIXME: we use newline() elsewhere
            fix_error!(ParserError, take_till!(|byte| byte == b'\n'))
        )
    )
);

named!(op<&[u8], &[u8], ParserError>,
    recognize!(peek!(one_of!("|&;<>()$`\\\"'\n")))
);

// TODO: eliminate from as many parsers as possible
named!(ignore<&[u8], &[u8], ParserError>,
    recognize!(
        tuple!(
            fix_error!(ParserError, space0),
            opt!(
                alt!(
                    recognize!(pair!(line_continuation, ignore)) |
                    recognize!(pair!(
                        clean_tag!("#"),
                        // FIXME: we use newline() elsewhere
                        fix_error!(ParserError, take_till!(|byte| byte == b'\n'))
                    ))
                )
            )
        )
    )
);

named!(clean_newline<&[u8], char, ParserError>,
    fix_error!(ParserError, newline)
);
