use std::cell::RefCell;
use std::ffi::{OsString, OsStr};
use std::fmt;
use std::os::unix::ffi::{OsStringExt, OsStrExt};
use std::os::unix::io::RawFd;
use std::rc::Rc;

use super::ast::*;

#[cfg(windows)]
type ParseInput<'a> = ::std::os::windows::ffi::EncodeWide<'a>;
#[cfg(unix)]
type ParseInput<'a> = ::std::slice::Iter<'a, u8>;

type ParseResult<'a, O> = Result<(ParseInput<'a>, O), ParserError>;

#[derive(Debug, Fail)]
pub struct ParserError {
    linenum: usize,
    errors: Vec<ParserErrorKind>,
}

impl ParserError {
    pub fn new(linenum: usize) -> Self {
        Self {
            linenum: linenum,
            errors: vec![],
        }
    }

    pub fn with_kind(kind: ParserErrorKind) -> Self {
        Self {
            //linenum: linenum,
            linenum: 0,
            errors: vec![kind],
        }
    }

    pub fn incomplete(&self) -> bool {
        self.last() == Some(&ParserErrorKind::Incomplete)
    }

    pub fn last(&self) -> Option<&ParserErrorKind> {
        self.errors.last()
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for err in &self.errors {
            writeln!(f, "{}", err)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum ParserErrorKind {
    If,
    Case,
    For,
    While,
    Until,
    BraceGroup,
    SubShell,
    FunctionDef,
    SimpleCommand,
    ParamExpand,
    CommandSubst,
    DoubleQuote,
    SingleQuote,
    Word,
    Name,
    IoNumber,
    Number,
    AndOrSep,
    Not,
    TakeUntil,
    OneOf(&'static [&'static str]),
    CmdNameKeyword(&'static str),
    CompoundCommandKeyword,
    Token(&'static str),
    Keyword(&'static str),
    Item(&'static str),

    // this should occur when there is a newline but nothing else in the input (in a location where
    // a newline is expected AND where we are still in the middle of parsing something) or when
    // there is a line continuation and nothing else in the input (unlike `nom`, which gives this
    // value back very easily)
    Incomplete,
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ParserErrorKind::*;

        // TODO: add line numbers (probably set ParserError up so that it's a struct with `linenum`
        //       and `kind` fields
        let args = match self {
            If => "\tin if",
            Case => "\tin case",
            For => "\tin for",
            While => "\tin while",
            Until => "\tin until",
            BraceGroup => "\tin brace group ({ ... })",
            SubShell => "\tin subshell",
            FunctionDef => "\tin function definition",
            SimpleCommand => "\tin simple command",
            ParamExpand => "\tin parameter expansion",
            CommandSubst => "\tin command substitution",
            DoubleQuote => "\tin double quote",
            SingleQuote => "\tin single quote",
            Word => "\tin word",
            Name => "expected function or command name",
            IoNumber => "expected file descriptor number",
            Number => "expected numeral",
            AndOrSep => "expected && or ||",
            Not => "unexpected value (not)",
            TakeUntil => "unexpected value (take until)",
            OneOf(values) => return write!(f, "expected one of {:?}", values),
            CmdNameKeyword(keyword) => return write!(f, "expected command name but found keyword {}", keyword),
            CompoundCommandKeyword => "expected one of {, for, if, case, while, until",
            Token(tok) => return write!(f, "expected token {}", tok),
            Keyword(keyword) => return write!(f, "expected keyword {}", keyword),
            Item(item) => return write!(f, "expected {}", item),

            Incomplete => "expected more input",
        };
        write!(f, "{}", args)
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
    linenum: usize,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            heredoc_markers: vec![],
            linenum: 1,
        }
    }

    // NOTE: afaict, the shell should execute each full line/command *until* it hits a line that
    //       can't be parsed or we reach the end of the file (so we can't do what we do now which
    //       is fail without doing anything if any line is wrong, although maybe this could be an
    //       shopt extension?)
    pub fn complete_command<'a>(&mut self, mut input: ParseInput<'a>) -> ParseResult<'a, CompleteCommand> {
        debug!("complete_command");

        let (mut input, _) = ignore(input)?;
        while let Ok((inp, _)) = newline_list(input.clone(), self).and_then(|(input, _)| ignore(input)) {
            input = inp;
        }

        let (input, lists) = if input.clone().next().is_none() {
            // the line is blank
            (input, Vec::with_capacity(0))
        } else {
            let (input, first) = list(input, self)?;
            let mut result = vec![first];

            // FIXME: this seems like a hacky solution, some rule in the parser is too lenient
            let (input, _) = ignore(input)?;
            let input = match newline_list(input.clone(), self) {
                Ok((input, _)) => input,
                Err(f) => {
                    println!("{:?}", input);
                    if input.clone().next().is_some() {
                        return Err(f);
                    } else {
                        input
                    }
                }
            };

            /*
            let mut input = skip_whitespace0(input);

            loop {
                match separator(input.clone(), self) {
                    Ok((inp, val)) => {
                        let (inp, val) = list(inp, self)?;
                        result.push(val);
                        input = skip_whitespace0(input);
                    }
                    Err(f) => {
                        if input.clone().next().is_some() {
                            return Err(f);
                        } else {
                            break;
                        }
                    }
                }
            }*/

            (input, result)
        };

        Ok((input, CompleteCommand::new(lists)))
    }

    pub fn convert_input<'a>(&self, input: &'a OsStr) -> ParseInput<'a> {
        osstr_to_parse_input(input)
    }
}

#[cfg(windows)]
fn osstr_to_parse_input(input: &OsStr) -> ParseInput {
    use std::os::windows::ffi::OsStrExt;

    input.encode_wide()
}

#[cfg(unix)]
fn osstr_to_parse_input(input: &OsStr) -> ParseInput {
    use std::os::unix::ffi::OsStrExt;

    input.as_bytes().iter()
}

fn token_to_input(token: &OsStr) -> ParseInput {
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStrExt;

        token.as_bytes().iter()
    }

    #[cfg(windows)]
    {
        use std::os::windows::ffi::OsStrExt;

        token.encode_wide()
    }
}

#[cfg(windows)]
fn unit_to_osstring(unit: u16) -> OsString {
    use std::os::windows::ffi::OsStringExt;

    OsString::from_wide(&[unit])
}

#[cfg(unix)]
fn unit_to_osstring(unit: u8) -> OsString {
    use std::os::unix::ffi::OsStrExt;

    OsStr::from_bytes(&[unit]).to_owned()
}

#[cfg(windows)]
fn vec_to_osstring(data: Vec<u16>) -> OsString {
    use std::os::windows::ffi::OsStringExt;

    OsString::from_wide(&data)
}

#[cfg(unix)]
fn vec_to_osstring(data: Vec<u8>) -> OsString {
    use std::os::unix::ffi::OsStringExt;

    OsString::from_vec(data)
}

fn is_token<'a>(input: ParseInput<'a>, token: &'static str) -> ParseResult<'a, ()> {
    debug!("is_token");

    is_next(input, token)
        .and_then(|(input, res)|
            ignore(input).map(|(input, _)| (input, res))
        )
        .map_err(|mut e| {
            e.errors.clear();
            e.errors.push(ParserErrorKind::Token(token));
            e
        })
}

fn is_keyword<'a>(input: ParseInput<'a>, keyword: &'static str) -> ParseResult<'a, ()> {
    debug!("is_keyword");

    is_next(input, keyword)
        .and_then(|(input, res)|
            delimiter(input).map(|(input, _)| (input, res))
        )
        .map_err(|mut e| {
            e.errors.clear();
            e.errors.push(ParserErrorKind::Keyword(keyword));
            e
        })
}

fn is_next<'a>(mut input: ParseInput<'a>, value: &'static str) -> ParseResult<'a, ()> {
    debug!("is_next");

    for ch in token_to_input(OsStr::new(value)) {
        match input.next() {
            Some(val) if val == ch => {}
            _ => {
                return Err(ParserError::with_kind(ParserErrorKind::Item(value)));
            }
        }
    }

    debug!("success: {}", value);

    Ok((input, ()))
}

fn is_one_of<'a>(mut input: ParseInput<'a>, values: &'static [&'static str]) -> ParseResult<'a, usize> {
    for (idx, val) in values.iter().enumerate() {
        if let Ok((input, _)) = is_next(input.clone(), val) {
            return Ok((input, idx));
        }
    }

    Err(ParserError::with_kind(ParserErrorKind::OneOf(values)))
}

fn is_heredoc_marker<'a>(mut input: ParseInput<'a>, value: &OsStr) -> ParseResult<'a, ()> {
    // TODO
    unimplemented!()
}

/// Skip a value that may or may not exist in the input.
fn skip_value<'a>(mut input: ParseInput<'a>, value: &'static str) -> ParseInput<'a> {
    is_next(input.clone(), value).map(|(input, _)| input).unwrap_or(input)
}

/// Skip a value multiple times.  The value may or may not exist in the input.
fn skip_value_many<'a>(mut input: ParseInput<'a>, value: &'static str) -> ParseInput<'a> {
    while let Ok((inp, _)) = is_next(input.clone(), value) {
        input = inp;
    }
    input
}

/// Skip values multiple times.  The values may or may not exist in the input.
fn skip_values_many<'a>(mut input: ParseInput<'a>, values: &'static [&'static str]) -> ParseInput<'a> {
    while let Ok((inp, _)) = is_one_of(input.clone(), values) {
        input = inp;
    }
    input
}

fn take_while_matches1<'a, F, R>(mut input: ParseInput<'a>, mut func: F) -> ParseResult<'a, OsString>
where
    F: FnMut(ParseInput<'a>) -> ParseResult<'a, R>,
{
    func(input.clone())?;
    let mut buffer = vec![*input.next().unwrap()];
    loop {
        if let Err(_) = func(input.clone()) {
            break;
        }
        if let Some(&unit) = input.next() {
            buffer.push(unit);
        }
    }
    Ok((input, vec_to_osstring(buffer)))
}

fn take_until_consuming_matches1<'a, F, R>(mut input: ParseInput<'a>, mut func: F) -> ParseResult<'a, OsString>
where
    F: FnMut(ParseInput<'a>) -> ParseResult<'a, R>,
{
    if let Ok(_) = func(input.clone()) {
        // TODO: give more info
        return Err(ParserError::with_kind(ParserErrorKind::TakeUntil));
    }

    let mut buffer = match input.next() {
        Some(&unit) => vec![unit],
        None => return Err(ParserError::with_kind(ParserErrorKind::TakeUntil)),
    };

    let mut not_eof = true;
    while not_eof {
        if let Ok(_) = func(input.clone()) {
            // consume the match
            let (inp, _) = func(input).unwrap();
            input = inp;
            break;
        }
        if let Some(&unit) = input.next() {
            buffer.push(unit);
        } else {
            not_eof = false;
        }
    }

    Ok((input, vec_to_osstring(buffer)))
}

fn take_until_value0<'a>(mut input: ParseInput<'a>, value: &'static str) -> ParseResult<'a, OsString> {
    let mut not_eof = true;

    let mut buffer = vec![];
    while not_eof {
        if let Ok(_) = is_next(input.clone(), value) {
            break;
        }
        if let Some(&unit) = input.next() {
            buffer.push(unit);
        } else {
            not_eof = false;
        }
    }

    Ok((input, vec_to_osstring(buffer)))
}

fn skip_until_matches<'a, F, R>(mut input: ParseInput<'a>, mut func: F) -> ParseInput<'a>
where
    F: FnMut(ParseInput<'a>) -> ParseResult<'a, R>,
{
    let mut not_eof = true;

    while not_eof {
        if let Ok(_) = func(input.clone()) {
            return input;
        }
        not_eof = input.next().is_some();
    }

    input
}

fn many_matches1<'a, F, R>(input: ParseInput<'a>, mut func: F) -> ParseResult<'a, Vec<R>>
where
    F: FnMut(ParseInput<'a>) -> ParseResult<'a, R>,
{
    let (mut input, first) = func(input)?;
    let mut result = vec![first];

    while let Ok((inp, value)) = func(input.clone()) {
        result.push(value);
        input = inp;
    }

    Ok((input, result))
}

fn many_matches0<'a, F, R>(mut input: ParseInput<'a>, mut func: F) -> ParseResult<'a, Vec<R>>
where
    F: FnMut(ParseInput<'a>) -> ParseResult<'a, R>,
{
    let mut result = vec![];

    while let Ok((inp, value)) = func(input.clone()) {
        result.push(value);
        input = inp;
    }

    Ok((input, result))
}

fn is_not<'a, F, R>(input: ParseInput<'a>, mut func: F) -> ParseResult<'a, ()>
where
    F: FnMut(ParseInput<'a>) -> ParseResult<'a, R>,
{
    match func(input.clone()) {
        // FIXME: should return something about the value that was unexpected
        Ok(_) => Err(ParserError::with_kind(ParserErrorKind::Not)),
        Err(_) => Ok((input, ()))
    }
}

fn list<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Vec<Vec<AndOr>>> {
    debug!("list");

    let (mut input, value) = and_or(input, parser)?;
    let mut result = vec![value];

    while let Ok((inp, op)) = separator_op(input.clone()) {
        let (inp, value) = and_or(inp, parser)?;
        input = inp;
        result.push(value);
    }

    Ok((input, result))
}

fn and_or<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Vec<AndOr>> {
    debug!("and_or");

    let (mut input, value) = pipeline(input, parser)?;
    let mut result = vec![AndOr::new(value, SepKind::First)];

    while let Ok((inp, sep)) = and_or_sep(input.clone(), parser) {
        let (inp, _) = linebreak(inp, parser)?;
        let (inp, value) = pipeline(inp, parser)?;

        input = inp;
        result.push(AndOr::new(value, sep));
    }

    Ok((input, result))
}

fn and_or_sep<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, SepKind> {
    debug!("and_or_sep");

    is_token(input.clone(), "&&")
        .map(|(input, _)| (input, SepKind::And))
        .or_else(|_| {
            is_token(input, "||")
                .map(|(input, _)| (input, SepKind::Or))
                .map_err(|mut e| {
                    e.errors.clear();
                    e.errors.push(ParserErrorKind::AndOrSep);
                    e
                })
        })
}

fn var_name<'a>(input: ParseInput<'a>) -> ParseResult<'a, OsString> {
    debug!("var_name");

    general_name(input, |input| Ok((input, ())))
}

fn var_assign<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, VarAssign> {
    debug!("var_assign");

    let (input, name) = var_name(input)?;
    is_next(input, "=")
        .map(|(input, _)| {
            let (input, expr) = match word(input.clone(), parser) {
                Ok((input, expr)) => (input, Some(expr)),
                _ => {
                    let (input, _) = ignore(input).unwrap();
                    (input, None)
                }
            };
            (input, VarAssign { varname: name, value: expr })
        })
}

fn parameter<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ParamExpr> {
    debug!("parameter");

    is_next(input, "$")
        .and_then(|(input, _)| {
            match is_next(input.clone(), "{") {
                Ok((input, _)) => param_expr(input, parser)
                    .and_then(|(input, expr)| {
                        is_token(input, "}").map(|(input, _)| (input, expr))
                    }),
                Err(_) => param_name(input, delimiter)
                    .map(|(input, name)| (input, ParamExpr::new(name, ParamExprKind::Value)))
            }
        })
}

fn param_expr<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ParamExpr> {
    debug!("param_expr");

    is_next(input.clone(), "#")
        .map(|(input, _)| {
            let (input, name) = param_name(input.clone(), |input| Ok((input, ())))
                .unwrap_or_else(|_| (input, Param::Var(OsString::new())));

            (input, ParamExpr::new(name, ParamExprKind::Length))
        })
        .or_else(|_| {
            let (input, name) = param_name(input, |input| Ok((input, ())))?;
            param_subst(input.clone(), parser)
                .or_else(|_| Ok((input, ParamExprKind::Value)))
                .map(|(input, kind)| (input, ParamExpr::new(name, kind)))
        })
}

fn param_subst<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ParamExprKind> {
    debug!("param_subst");

    match is_one_of(input.clone(), &[":", "%", "#"]) {
        Ok((input, idx)) => match idx {
            0 => param_subst_value(input, parser)
                .map(|(input, kind)| {
                    (input, match kind {
                        ParamExprKind::AssignNull(value) => ParamExprKind::Assign(value),
                        ParamExprKind::UseNull(value) => ParamExprKind::Use(value),
                        ParamExprKind::ErrorNull(value) => ParamExprKind::Error(value),
                        ParamExprKind::Alternate(value) => ParamExprKind::AlternateNull(value),
                        _ => unreachable!()
                    })
                }),
            1 => param_subst_suffix(input, parser)
                .map(|(input, kind)| {
                    if let ParamExprKind::SmallSuffix(value) = kind {
                        (input, ParamExprKind::LargeSuffix(value))
                    } else {
                        unreachable!()
                    }
                }),
            2 => param_subst_prefix(input, parser)
                .map(|(input, kind)| {
                    if let ParamExprKind::SmallPrefix(value) = kind {
                        (input, ParamExprKind::LargePrefix(value))
                    } else {
                        unreachable!()
                    }
                }),
            _ => unreachable!(),
        },
        Err(_) => {
            param_subst_value(input.clone(), parser)
                .or_else(|_| param_subst_suffix(input.clone(), parser))
                .or_else(|_| param_subst_prefix(input, parser))
        }
    }
}

// FIXME: word may be incorrect as dash accepts ${var:=hello there} (i.e. two words)
//        maybe just take text until the brace and interpret after reading?
fn param_subst_value<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ParamExprKind> {
    debug!("param_subst_value");

    is_one_of(input, &["-", "=", "?", "+"])
        .and_then(|(input, idx)| {
            param_value(input.clone(), parser)
                .map(|(input, val)| {
                    (input, match idx {
                        0 => ParamExprKind::UseNull(val),
                        1 => ParamExprKind::AssignNull(val),
                        2 => ParamExprKind::ErrorNull(Some(val)),
                        3 => ParamExprKind::Alternate(val),
                        _ => unreachable!(),
                    })
                })
                .or_else(|f| {
                    // check if the value was ?
                    if idx == 2 {
                        Ok((input, ParamExprKind::ErrorNull(None)))
                    } else {
                        Err(f)
                    }
                })
        })
}

fn param_subst_prefix<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ParamExprKind> {
    debug!("param_subst_prefix");

    is_next(input, "#")
        .and_then(|(input, _)| param_value(input, parser))
        .map(|(input, val)| (input, ParamExprKind::SmallPrefix(val)))
}

fn param_subst_suffix<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ParamExprKind> {
    debug!("param_subst_suffix");

    is_next(input, "%")
        .and_then(|(input, _)| param_value(input, parser))
        .map(|(input, val)| (input, ParamExprKind::SmallSuffix(val)))
}

// FIXME: the names for *, ?, and @ suck
fn param_name<'a, F>(input: ParseInput<'a>, check_fn: F) -> ParseResult<'a, Param>
where
    F: Fn(ParseInput<'a>) -> ParseResult<'a, ()> + Clone,
{
    debug!("param");

    general_name(input.clone(), check_fn.clone())
        .map(|(input, name)| (input, Param::Var(name)))
        .or_else(|_| is_next(input.clone(), "*").map(|(input, _)| (input, Param::Star)))
        .or_else(|_| is_next(input.clone(), "?").map(|(input, _)| (input, Param::Question)))
        .or_else(|_| is_next(input.clone(), "@").map(|(input, _)| (input, Param::At)))
        .or_else(|_| is_next(input.clone(), "#").map(|(input, _)| (input, Param::NumParams)))
        .or_else(|_| is_next(input.clone(), "$").map(|(input, _)| (input, Param::ShellPid)))
        .or_else(|_| is_next(input.clone(), "!").map(|(input, _)| (input, Param::BackgroundPid)))
        .or_else(|_| number(input, check_fn).map(|(input, num)| (input, Param::Positional(num))))
}

fn param_value<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Box<Word>> {
    debug!("param_value");

    general_word(input, parser, param_delimiter).map(|(input, val)| (input, Box::new(val)))
}

fn param_delimiter(input: ParseInput) -> ParseResult<()> {
    delimiter(input.clone())
        .or_else(|_| is_next(input.clone(), "}").map(|_| (input, ())))
}

fn number<'a, F>(mut input: ParseInput<'a>, check_fn: F) -> ParseResult<'a, usize>
where
    F: Fn(ParseInput<'a>) -> ParseResult<'a, ()>,
{
    debug!("number");

    if let Some(&unit) = input.next() {
        if unit >= b'0' as _ && unit <= b'9' as _ {
            let mut buffer = String::new();
            buffer.push(unit as _);

            while let Some(&unit) = input.clone().next() {
                if unit >= b'0' as _ && unit <= b'9' as _ {
                    buffer.push(unit as _);
                } else {
                    break;
                }
            }

            let (input, _) = check_fn(input)?;

            if let Ok(num) = buffer.parse() {
                return Ok((input, num))
            }
        }
    }

    Err(ParserError::with_kind(ParserErrorKind::Number))
}

fn pipe_seq<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Pipeline> {
    debug!("pipe_seq");

    let (mut input, first) = command(input, parser)?;
    let mut result = vec![first];

    while let Ok((inp, _)) = pipe_sep(input.clone(), parser) {
        let (inp, val) = command(inp, parser)?;
        result.push(val);
        input = inp;
    }

    Ok((input, result.into_iter().collect()))
}

fn pipe_sep<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ()> {
    debug!("pipe_sep");

    is_token(input, "|").and_then(|(input, _)| linebreak(input, parser))
}

fn pipeline<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Pipeline> {
    debug!("pipeline");

    let (input, bang) = match is_keyword(input.clone(), "!") {
        Ok((input, _)) => (input, true),
        Err(_) => (input, false),
    };

    pipe_seq(input, parser)
        .map(|(input, mut seq)| {
            seq.bang = bang;
            (input, seq)
        })
}

fn command<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Command> {
    debug!("command");

    let orig_input = input.clone();

    match fname(input) {
        Ok((input, name)) => {
            match compound_command_keyword(input.clone(), parser, &name) {
                Ok(res) => return Ok(res),
                Err(f) => {
                    if f.last() != Some(&ParserErrorKind::CompoundCommandKeyword) {
                        return Err(f);
                    }
                }
            }
            // we are either a function definition or a simple command at this point
            let funcdef_res = is_token(input.clone(), "(")
                .and_then(|(input, _)| is_token(input, ")"));
            match funcdef_res {
                Ok((input, _)) => function_def(input, parser, name).map(|(input, def)| (input, def.into())),
                Err(_) => simple_command(orig_input, parser).map(|(input, cmd)| (input, cmd.into())),
            }
        }
        Err(_) => {
            // the item must be a simple command, a brace group, OR a subshell if it is valid
            if let Ok((input, _)) = is_token(orig_input.clone(), "(") {
                subshell(input, parser)
            } else if let Ok((input, _)) = is_token(orig_input.clone(), "{") {
                brace_group(input, parser)
            } else {
                simple_command(orig_input, parser).map(|(input, cmd)| (input, cmd.into()))
            }
        }
    }
}

fn compound_command_keyword<'a>(input: ParseInput<'a>, parser: &mut Parser, name: &Name) -> ParseResult<'a, Command> {
    let compound_cmd: [(&str, fn(ParseInput<'a>, &mut Parser) -> ParseResult<'a, Command>); 5] = [
        ("for", for_clause),
        ("case", case_clause),
        ("if", if_clause),
        ("while", |input, parser| while_clause(input, parser, true)),
        ("until", |input, parser| while_clause(input, parser, false)),
    ];

    for (cmd_kind, func) in &compound_cmd {
        if name == OsStr::new(cmd_kind) {
            return func(input.clone(), parser).map(|(input, mut cmd)| {
                let (input, redir) = redirect_list(input.clone(), parser)
                    .unwrap_or_else(|_| (input, Vec::with_capacity(0)));
                cmd.redirect_list = redir;

                (input, cmd)
            });
        }
    }

    // FIXME: provide more info
    Err(ParserError::with_kind(ParserErrorKind::CompoundCommandKeyword))
}

fn compound_command<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Command> {
    debug!("compound_command");

    if let Ok((input, _)) = is_token(input.clone(), "(") {
        subshell(input, parser)
    } else if let Ok((input, _)) = is_token(input.clone(), "{") {
        brace_group(input, parser)
    } else {
        fname(input)
            .and_then(|(input, name)| compound_command_keyword(input, parser, &name))
    }
}

fn subshell<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Command> {
    debug!("subshell");

    compound_list_inner(input, parser)
        .and_then(|(input, inner)| {
            is_token(input, ")")
                .map(|(input, _)| {
                    let cmd = if let CommandInner::AndOr(and_ors) = inner {
                        Command::with_inner(CommandInner::SubShell(and_ors))
                    } else {
                        unreachable!()
                    };
                    (input, cmd)
                })
        })
        .map_err(|mut e| {
            e.errors.push(ParserErrorKind::SubShell);
            e
        })
}

fn compound_list<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Command> {
    debug!("compound_list");

    compound_list_inner(input, parser).map(|(input, inner)| (input, Command::with_inner(inner)))
}

fn compound_list_inner<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, CommandInner> {
    debug!("compound_list_inner");

    let (input, _) = newline_list(input.clone(), parser).unwrap_or((input, ()));

    term(input, parser)
        .map(|(input, and_ors)| {
            let (input, _) = is_not(input.clone(), term_separator)
                .and_then(|(input, _)| separator(input, parser))
                .unwrap_or((input, ()));
            (input, CommandInner::AndOr(and_ors))
        })
}

fn term<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Vec<Vec<AndOr>>> {
    debug!("term");

    let (mut input, first) = and_or(input, parser)?;
    let mut result = vec![first];

    loop {
        let res = is_not(input.clone(), term_separator)
            .and_then(|(input, _)| separator(input, parser))
            .and_then(|(input, _)| and_or(input, parser));

        match res {
            Ok((inp, val)) => {
                result.push(val);
                input = inp;
            }
            Err(_) => break,
        }
    }

    Ok((input, result))
}

// FIXME: the only reason this needs to be done is because we don't tokenize the input
fn term_separator<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    debug!("term_separator");
    // thus far the only item that seems to confuse the parser is DSEMI
    is_next(input, ";;")
}

fn for_clause<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Command> {
    debug!("for_clause");

    name(input)
        .and_then(|(input, name)| {
            linebreak(input, parser)
                .and_then(|(input, _)| {
                    let (input, words) = match is_keyword(input.clone(), "in") {
                        Ok((input, _)) => {
                            many_matches0(input, |input| word(input, parser))
                                .and_then(|(input, words)|
                                    sequential_sep(input, parser)
                                        .map(|(input, _)| (input, words))
                                )?
                        }
                        Err(_) => (input, Vec::with_capacity(0)),
                    };
                    do_group(input, parser).map(|(input, do_stmt)| {
                        (input, ForClause::new(name, words, do_stmt).into())
                    })
                })
        })
        .map_err(|mut e| {
            e.errors.push(ParserErrorKind::For);
            e
        })
}

// FIXME: i think this will split like name @ on name@ (dunno if that's wrong though?)
fn name<'a>(mut input: ParseInput<'a>) -> ParseResult<'a, Name> {
    debug!("name");

    general_name(input, delimiter)
}

fn general_name<'a, F>(mut input: ParseInput<'a>, check_fn: F) -> ParseResult<'a, Name>
where
    F: Fn(ParseInput<'a>) -> ParseResult<'a, ()>,
{
    debug!("general_name");

    if let Some(&unit) = input.next() {
        if (unit >= b'a' as _ && unit <= b'z' as _) || (unit >= b'A' as _ && unit <= b'Z' as _) || unit == b'_' as _ {
            let mut name = String::new();
            name.push(unit as _);
            while let Some(&unit) = input.clone().next() {
                if (unit >= b'a' as _ && unit <= b'z' as _) || (unit >= b'A' as _ && unit <= b'Z' as _) || unit == b'_' as _ || (unit >= b'0' as _ && unit <= b'9' as _) {
                    name.push(unit as _);
                    input.next();
                } else {
                    break;
                }
            }

            // FIXME: is the delimiter check necessary?
            let (input, _) = check_fn(input)?;

            return Ok((input, name.into()))
        }
    }

    Err(ParserError::with_kind(ParserErrorKind::Name))
}

fn case_clause<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Command> {
    debug!("case_clause");

    word(input, parser)
        .and_then(|(input, word)| {
            linebreak(input, parser)
                .and_then(|(input, _)| is_keyword(input, "in"))
                //.and_then(|(input, _)| ignore(input))
                .and_then(|(input, _)| linebreak(input, parser))
                .and_then(|(input, _)| case_list(input, parser))
                .and_then(|(input, case_list)| {
                    is_keyword(input, "esac")
                        //.and_then(|(input, _)| ignore(input))
                        .map(|(input, _)| {
                            (input, CaseClause::new(word, case_list).into())
                        })
                })
        })
        .map_err(|mut e| {
            e.errors.push(ParserErrorKind::Case);
            e
        })
}

fn case_list<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, CaseList> {
    match case_item(input.clone(), parser) {
        Ok((mut input, item)) => {
            let mut list = vec![item];
            while let Ok((inp, item)) = case_item(input.clone(), parser) {
                list.push(item);
                input = inp;
            }
            let input = match case_item_ns(input.clone(), parser) {
                Ok((input, item)) => {
                    list.push(item);
                    input
                }
                Err(_) => input,
            };
            Ok((input, list))
        }
        Err(_) => case_item_ns(input, parser)
            .map(|(input, item)| (input, vec![item]))
    }.map(|(input, list)| (input, CaseList::new(list)))
}

fn case_item_common<'a, F>(input: ParseInput<'a>, parser: &mut Parser, func: F) -> ParseResult<'a, CaseItem>
where
    F: Fn(ParseInput<'a>, &mut Parser) -> ParseResult<'a, Option<Command>>,
{
    debug!("case_item_common");

    is_token(input.clone(), "(")
        .or_else(|_| is_not(input, |input| is_keyword(input, "esac")))
        .and_then(|(input, _)| pattern(input, parser))
        .and_then(|(input, pattern)| {
            is_token(input, ")")
                .and_then(|(input, _)| func(input, parser))
                .and_then(|(input, list)| {
                    linebreak(input, parser)
                        .map(|(input, _)| (input, CaseItem::new(pattern, list)))
                })
        })
}

fn case_item_ns<'a>(inp: ParseInput<'a>, p: &mut Parser) -> ParseResult<'a, CaseItem> {
    debug!("case_item_ns");

    case_item_common(inp, p, |input, parser| {
        compound_list(input.clone(), parser)
            .map(|(input, res)| (input, Some(res)))
            .or_else(|_| Ok((input, None)))
    })
}

fn case_item<'a>(inp: ParseInput<'a>, p: &mut Parser) -> ParseResult<'a, CaseItem> {
    debug!("case_item");

    case_item_common(inp, p, |input, parser| {
        compound_list(input.clone(), parser)
            .map(|(input, res)| (input, Some(res)))
            .or_else(|_| {
                linebreak(input, parser).map(|(input, _)| (input, None))
            })
            .and_then(|(input, res)| {
                is_token(input, ";;").map(|(input, _)| (input, res))
            })
    })
}

// TODO: this needs to actually match "patterns" rather than words
fn pattern<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Pattern> {
    debug!("pattern");

    // XXX: this is a fairly common pattern (ha ha), maybe turn into another function?
    let (mut input, value) = word(input, parser)?;
    let mut result = vec![value];

    while let Ok((inp, _)) = is_token(input.clone(), "|") {
        let (inp, value) = word(inp, parser)?;
        input = inp;
        result.push(value);
    }

    Ok((input, Pattern::new(result)))
}

fn if_clause<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Command> {
    debug!("if_clause");

    compound_list(input, parser)
        .and_then(|(input, cond)| {
            is_keyword(input, "then")
                .and_then(|(input, _)| compound_list(input, parser))
                .and_then(|(input, body)| {
                    let (input, else_stmt) = else_part(input.clone(), parser)
                        .map(|(input, stmt)| (input, Some(stmt)))
                        .unwrap_or((input, None));

                    is_keyword(input, "fi").map(|(input, _)| {
                        (input, IfClause::new(cond, body, else_stmt).into())
                    })
                })
        })
        .map_err(|mut e| {
            e.errors.push(ParserErrorKind::If);
            e
        })
}

fn else_part<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ElseClause> {
    debug!("else_part");

    match is_keyword(input.clone(), "elif") {
        Ok((input, _)) => {
            compound_list(input, parser)
                .and_then(|(input, cond)| {
                    is_keyword(input, "then")
                        .and_then(|(input, _)| compound_list(input, parser))
                        .map(|(input, body)| {
                            let (input, else_stmt) = else_part(input.clone(), parser)
                                .map(|(input, stmt)| (input, Some(Box::new(stmt))))
                                .unwrap_or((input, None));
                            (input, ElseClause::new(Some(cond), body, else_stmt))
                        })
                })
        }
        Err(_) => is_keyword(input, "else")
            .and_then(|(input, _)| compound_list(input, parser))
            .map(|(input, body)| (input, ElseClause::new(None, body, None)))
    }
}

fn while_clause<'a>(input: ParseInput<'a>, parser: &mut Parser, check: bool) -> ParseResult<'a, Command> {
    debug!("while_clause");

    compound_list(input, parser)
        .and_then(|(input, cond)| {
            do_group(input, parser).map(|(input, do_stmt)| {
                (input, WhileClause::new(cond, check, do_stmt).into())
            })
        })
        .map_err(|mut e| {
            e.errors.push(ParserErrorKind::While);
            e
        })
}

fn function_def<'a>(input: ParseInput<'a>, parser: &mut Parser, name: Name) -> ParseResult<'a, FunctionDef> {
    debug!("function_def");

    linebreak(input, parser)
        .and_then(|(input, _)| function_body(input, parser))
        .map(|(input, body)| {
            (input, FunctionDef::new(name, Rc::new(body)))
        })
        .map_err(|mut e| {
            e.errors.push(ParserErrorKind::FunctionDef);
            e
        })
}

fn function_body<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, FunctionBody> {
    debug!("function_body");

    // TODO: apply rule 9
    compound_command(input, parser)
        .map(|(input, cmd)| {
            let (input, redir) = redirect_list(input.clone(), parser)
                .unwrap_or_else(|_| (input, Vec::with_capacity(0)));

            (input, FunctionBody::new(cmd, redir))
        })
}

fn fname<'a>(input: ParseInput<'a>) -> ParseResult<'a, Name> {
    debug!("fname");

    name(input)
}

fn brace_group<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Command> {
    debug!("brace_group");

    compound_list(input, parser)
        .and_then(|(input, list)|
            is_keyword(input, "}").map(|(input, _)| (input, list))
        )
}

fn do_group<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Command> {
    debug!("do_group");

    is_keyword(input, "do")
        .and_then(|(input, _)| compound_list(input, parser))
        .and_then(|(input, list)|
            is_keyword(input, "done").map(|(input, _)| (input, list))
        )
}

fn simple_command<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, SimpleCommand> {
    debug!("simple_command");

    cmd_prefix(input.clone(), parser)
        .map(|(input, prefix)| {
            let (input, word, suffix) = match cmd_word(input.clone(), parser) {
                Ok((input, word)) => {
                    // there can only be a suffix if there was a word
                    let (input, suffix) = match cmd_suffix(input.clone(), parser) {
                        Ok(res) => res,
                        Err(_) => (input, Vec::with_capacity(0)),
                    };
                    (input, Some(word), suffix)
                }
                Err(_) => (input, None, Vec::with_capacity(0)),
            };

            (input, SimpleCommand::new(word, prefix, suffix))
        })
        .or_else(|_| {
            cmd_name(input, parser)
                .map(|(input, name)| {
                    let (input, suffix) = cmd_suffix(input.clone(), parser)
                        .unwrap_or_else(|_| (input, Vec::with_capacity(0)));

                    (input, SimpleCommand::new(Some(name), Vec::with_capacity(0), suffix))
                })
        })
        .map_err(|mut e| {
            e.errors.push(ParserErrorKind::SimpleCommand);
            e
        })
}

fn cmd_name<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, CommandName> {
    debug!("cmd_name");

    // TODO: apply rule 7a
    word(input, parser).and_then(|(input, word)| {
        // XXX: need to ensure all reserved words are being checked (a command name is not allowed
        //      to be exactly a reserved word unless it is quoted)
        if let Word::Simple(ref text) = word {
            let keywords = [
                "do",
                "done",
                "for",
                "if",
                "elif",
                "else",
                "fi",
                "while",
                "until",
                "case",
                "esac",
                "in",
                "then",
                "{",
                "}",
                "!",
            ];
            for keyword in &keywords {
                if text.as_os_str() == OsStr::new(keyword) {
                    return Err(ParserError::with_kind(ParserErrorKind::CmdNameKeyword(keyword)));
                }
            }
        }
        Ok((input, word))
    })
}

fn cmd_word<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, CommandName> {
    debug!("cmd_word");
    // TODO: apply rule 7b
    word(input, parser)
}

fn cmd_prefix<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Vec<PreAction>> {
    debug!("cmd_prefix");

    many_matches1(input, |input| {
        io_redirect(input.clone(), parser)
            .map(|(input, redir)| (input, PreAction::IoRedirect(redir)))
            .or_else(|_|
                var_assign(input, parser)
                    .map(|(input, assign)| (input, PreAction::VarAssign(assign)))
            )
    })
}

fn cmd_suffix<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Vec<PostAction>> {
    debug!("cmd_suffix");

    many_matches1(input, |input| {
        io_redirect(input.clone(), parser)
            .map(|(input, redir)| (input, PostAction::IoRedirect(redir)))
            .or_else(|_|
                word(input, parser)
                    .map(|(input, word)| (input, PostAction::Word(word)))
            )
    })
}

fn redirect_list<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Vec<IoRedirect>> {
    debug!("redirect_list");

    many_matches1(input, |input| io_redirect(input, parser))
}

fn io_redirect<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, IoRedirect> {
    debug!("io_redirect");

    let (input, num) = io_number(input.clone())
        .map(|(input, num)| (input, Some(num)))
        .unwrap_or((input, None));

    io_file(input.clone(), parser)
        .map(|(input, file)| (input, IoRedirect::File(num, file)))
        .or_else(|_|
            io_here(input, parser).map(|(input, heredoc)| (input, IoRedirect::Heredoc(num, heredoc)))
        )
}

fn io_here<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Rc<RefCell<HereDoc>>> {
    debug!("io_here");

    is_next(input, "<<")
        .map(|(input, _)| {
            is_next(input.clone(), "-")
                .map(|(new_input, _)| (new_input, true))
                .unwrap_or((input, false))
        })
        .and_then(|(input, dash)| {
            let (input, _) = ignore(input)?;
            let (input, delim) = here_end(input)?;

            let heredoc = Rc::new(RefCell::new(HereDoc::default()));
            parser.heredoc_markers.push(HereDocMarker {
                marker: delim,
                strip_tabs: dash,
                heredoc: heredoc.clone(),
            });
            Ok((input, heredoc))
        })
}

fn here_end<'a>(input: ParseInput<'a>) -> ParseResult<'a, HereDocWord> {
    debug!("here_end");
    // TODO: apply rule 3
    // FIXME: this is wrong (need to do quote removal and such)
    // FIXME: it's really unclear what is acceptable as a heredoc end marker (the spec claims that
    //        it should be a "word," but (afaict) the spec uses "word" to mean different things at
    //        different points.  dash seems to expect that the "word" is indeed a single word like
    //        a filename or something but not allowing command substitution/parameter expansion.  on
    //        the other hand, zsh allows stuff like $(printf hi) where the literal word that needs
    //        to be matched is "$(printf hi)".  because it's so unclear, i am just going with the
    //        definition that it must be a "simple" word (i.e. Word::Simple) or a quote with no
    //        inner expansions/substitutions

    unimplemented!()
    //map!(input, fix_error!(ParserError, alphanumeric1), |res| OsString::from_vec(res.to_owned()))
}

// this needs to read a heredoc until it encounters "\nEND MARKER\n"
// XXX: this could probably use some improvement
fn parse_heredoc<'a>(mut input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ()> {
    debug!("parse_heredoc");

    for marker in &mut parser.heredoc_markers {
        let mut heredoc = marker.heredoc.borrow_mut();

        let (inp, res) = take_until_consuming_matches1(input, |input| {
            newline(input)
                .and_then(|(input, _)| is_heredoc_marker(input, &marker.marker))
                .and_then(|(input, _)| newline(input))
        })?;

        input = inp;
        unimplemented!();
        // FIXME: below
        //heredoc.data = res;
    }
    parser.heredoc_markers.clear();

    Ok((input, ()))
}

fn command_subst<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, CommandSubst> {
    debug!("command_subst");

    command_subst_dollar(input.clone(), parser)
        .or_else(|_| command_subst_backtick(input, parser))
}

// FIXME: it seems stuff like $(NEWLINE cmd NEWLINE) should be supported, so ensure it is
fn command_subst_dollar<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, CommandSubst> {
    debug!("command_subst_dollar");

    is_next(input, "$(")
        .and_then(|(input, _)| {
            // avoid ambiguity between subshells and arithmetic expressions
            let (input, _) = is_not(input, |input| is_next(input, "("))?;
            let (input, _) = ignore(input)?;
            compound_list(input, parser)
                .and_then(|(input, cmd)| {
                    is_next(input, ")").map(|(input, _)| (input, CommandSubst::new(Box::new(cmd))))
                })
        })
}

// XXX: strategy should probably be collect input until first unescaped backtick (handling escaped dollar signs and such by perhaps adding single quotes around them and then feeding them to command for processing)
fn command_subst_backtick<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, CommandSubst> {
    debug!("command_subst_backtick");

    is_next(input, "`")
        .and_then(|(input, _)| {
            // TODO
            unimplemented!()
            //is_next(input, "`")
        })
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
fn single_quote<'a>(input: ParseInput<'a>) -> ParseResult<'a, Word> {
    debug!("single_quote");

    is_next(input, "'")
        .and_then(|(input, _)| {
            let (input, res) = take_until_value0(input, "'")?;
            is_next(input, "'")
                .map(|(input, _)| (input, Word::SingleQuote(res)))
        })
        .map_err(|mut e| {
            e.errors.push(ParserErrorKind::SingleQuote);
            e
        })
}

fn double_quote<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, DoubleQuote> {
    debug!("double_quote");

    is_next(input, "\"")
        .and_then(|(mut input, _)| {
            let mut words = vec![];
            loop {
                let res = parameter(input.clone(), parser)
                    .map(|(input, param)| (input, Word::Parameter(param)))
                    .or_else(|_|
                        command_subst(input.clone(), parser)
                            .map(|(input, subst)| (input, Word::CommandSubst(subst)))
                    )
                    // XXX: i think there will be special rules for the backtick version of command
                    //      substitution when in quotes
                    // TODO: arith expr
                    .or_else(|_|
                        text_or_escape_seq(input.clone())
                            .map(|(input, text)| (input, Word::SingleQuote(text)))
                    );
                match res {
                    Ok((inp, word)) => {
                        words.push(word);
                        input = inp;
                    }
                    Err(_) => break,
                }
            }
            is_next(input, "\"")
                .map(|(input, _)| (input, DoubleQuote::new(words)))
        })
        .map_err(|mut e| {
            e.errors.push(ParserErrorKind::DoubleQuote);
            e
        })
}

fn text_or_escape_seq<'a>(mut input: ParseInput<'a>) -> ParseResult<'a, OsString> {
    debug!("text_or_escape_seq");

    let mut first = true;
    let mut result = OsString::new();
    loop {
        let res = is_next(input.clone(), r"\")
            //.map()
            .map(|(input, _)| {
                // FIXME: needs to recognize line continuation
                // TODO: implement old code below
                unimplemented!()
            })
            .or_else(|_|
                take_while_matches1(input.clone(), |input|
                    is_not(input, |input|
                        is_one_of(input, &["\"", "$", "`", r"\"])
                    )
                )
            );

        match res {
            Ok((inp, s)) => {
                result.push(&s);
                input = inp;

                first = false;
            }
            Err(_) if !first => break,
            Err(f) => return Err(f),
        }
    }

    Ok((input, result))
    /*fold_many1!(input,
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
    )*/
}

fn linebreak<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ()> {
    debug!("linebreak");

    newline_list(input.clone(), parser)
        .or_else(|_| Ok((input, ())))
}

fn newline_list<'a>(mut input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ()> {
    debug!("newline_list");

    let mut first = true;
    loop {
        input = match newline(input.clone()) {
            Ok((input, _)) => input,
            Err(_) if !first => break,
            Err(f) => return Err(f),
        };

        if !parser.heredoc_markers.is_empty() {
            let (inp, _) = parse_heredoc(input, parser)?;
            input = inp;
        }

        let (inp, _) = ignore(input)?;
        input = inp;

        first = false;
    }

    Ok((input, ()))
}

// XXX: is & just run in background for this? (answer is yes, so need to keep track of which one was given instead of returning ())
fn separator_op<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    debug!("separator_op");

    is_one_of(input, &["&", ";"])
        .and_then(|(input, _)| ignore(input))
}

fn separator<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ()> {
    debug!("separator");

    separator_op(input.clone())
        .and_then(|(input, _)| linebreak(input, parser))
        .or_else(|_| newline_list(input, parser))
}

fn sequential_sep<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, ()> {
    debug!("sequential_seq");

    is_token(input.clone(), ";")
        .and_then(|(input, _)| linebreak(input, parser))
        .or_else(|_| newline_list(input, parser))
}

// XXX: list of things that use word and accept patterns: wordlist (used by for), case_item/case_item_ns, cmd_name, cmd_word, cmd_suffix, cmd_prefix (the right side)
//      things that do not accept patterns (afaict): case WORD in, name (used by e.g. for, fname), here_end, filename
fn word<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Word> {
    debug!("word");

    general_word(input, parser, delimiter)
}

fn general_word<'a, F>(input: ParseInput<'a>, parser: &mut Parser, delim: F) -> ParseResult<'a, Word>
where
    F: Fn(ParseInput<'a>) -> ParseResult<'a, ()> + Clone,
{
    word_inner(input, parser, delim).map_err(|mut e| {
        e.errors.push(ParserErrorKind::Word);
        e
    })
}

fn word_inner<'a, F>(input: ParseInput<'a>, parser: &mut Parser, delim: F) -> ParseResult<'a, Word>
where
    F: Fn(ParseInput<'a>) -> ParseResult<'a, ()> + Clone,
{
    let (input, mut first) = word_part(input, parser, delim.clone())?;

    let (mut input, second) = next_word_part(input, parser, &mut first, delim.clone());
    let word = if let Some(second) = second {
        let mut complex = vec![first, second];
        loop {
            let (inp, part) = next_word_part(input.clone(), parser, complex.last_mut().unwrap(), delim.clone());
            if let Some(part) = part {
                complex.push(part);
                input = inp;
            } else {
                break;
            }
        }
        Word::Complex(complex)
    } else {
        first
    };

    let (input, _) = ignore(input)?;

    Ok((input, word))
}

fn next_word_part<'a, F>(mut input: ParseInput<'a>, parser: &mut Parser, prev: &mut Word, delim: F) -> (ParseInput<'a>, Option<Word>)
where
    F: Fn(ParseInput<'a>) -> ParseResult<'a, ()> + Clone,
{
    loop {
        match word_part(input.clone(), parser, delim.clone()) {
            Ok((inp, second)) => {
                let second = combine_simple_words(second, prev);
                if let Some(second) = second {
                    return (input, Some(second));
                }

                input = inp;
            }
            Err(_) => {
                let (input, _) = ignore(input).unwrap();
                return (input, None);
            }
        }
    }
}

/// If both `current` and `prev` are "simple" words (_i.e._ Word::Simple), combine them.
fn combine_simple_words(current: Word, prev: &mut Word) -> Option<Word> {
    match (current, prev) {
        (Word::Simple(ref cur_text), Word::Simple(ref mut prev_text)) => {
            prev_text.push(cur_text);
            None
        }
        (current, _) => Some(current),
    }
}

fn word_part<'a, F>(input: ParseInput<'a>, parser: &mut Parser, delim: F) -> ParseResult<'a, Word>
where
    F: Fn(ParseInput<'a>) -> ParseResult<'a, ()> + Clone,
{
    debug!("word_part");

    // TODO: this might need to be able to parse quoted strings/words following different rules
    single_quote(input.clone())
        .or_else(|_|
            double_quote(input.clone(), parser)
                .map(|(input, quote)| (input, Word::DoubleQuote(quote)))
        )
        .or_else(|_|
            parameter(input.clone(), parser)
                .map(|(input, param)| (input, Word::Parameter(param)))
        )
        .or_else(|_|
            command_subst(input.clone(), parser)
                .map(|(input, subst)| (input, Word::CommandSubst(subst)))
        )
        // XXX: there may be more that need to be checked for
        // FIXME: this should not ignore "}" (this is why i likely need to tokenize first)
        // FIXME: if there is a word like \\LINE CONTINUATIONhi, the result will be two
        //        words rather than one (i.e. \ and hi instead of \hi), which is wrong
        .or_else(|_| {
            let (input, _) = is_not(input, line_continuation)?;
            is_next(input.clone(), r"\")
                .map(|(mut input, _)| {
                    let unit = input.next().map(|&unit| unit).unwrap_or(b'\\' as _);
                    (input, Word::Simple(unit_to_osstring(unit)))
                })
                .or_else(|_| {
                    let (input, data) = take_while_matches1(input.clone(), |input| is_not(input, delim.clone()))?;
                    Ok((input, Word::Simple(data)))
                })
        })
}

fn heredoc_marker_word<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Word> {
    // TODO
    unimplemented!()
}

fn io_file<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, IoRedirectFile> {
    debug!("io_file");

    let str_table = &[
        "<&",
        "<>",
        "<",
        ">&",
        ">>",
        ">|",
        ">",
    ];

    let kind_table = [
        IoRedirectKind::DupInput,
        IoRedirectKind::ReadWrite,
        IoRedirectKind::Input,
        IoRedirectKind::DupOutput,
        IoRedirectKind::Append,
        IoRedirectKind::Clobber,
        IoRedirectKind::Output,
    ];

    let (input, idx) = is_one_of(input, str_table)?;
    let (input, name) = filename(input, parser)?;

    Ok((input, IoRedirectFile::new(kind_table[idx], name)))
}

fn filename<'a>(input: ParseInput<'a>, parser: &mut Parser) -> ParseResult<'a, Word> {
    debug!("filename");
    // TODO: apply rule 2
    word(input, parser)
}

// NOTE: we only accept fds 0-9 as that is the minimum required (and seems to be the number
//       supported several shells).  if this ever changes, FD_COUNT in option.rs must be changed as
//       well
fn io_number<'a>(mut input: ParseInput<'a>) -> ParseResult<'a, RawFd> {
    debug!("io_number");

    if let Some(&unit) = input.next() {
        if unit >= b'0' as _ && unit <= b'9' as _ {
            delimiter(input.clone())?;
            let (input, _) = ignore(input)?;

            return Ok((input, unit as _));
        }
    }

    Err(ParserError::with_kind(ParserErrorKind::IoNumber))
}

fn delimiter<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    debug!("delimiter");

    delimiter_no_op(input.clone())
        .or_else(|_| op(input))
}

fn delimiter_no_op<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    debug!("delimiter_no_op");

    if input.clone().next().is_none() {
        return Ok((input, ()));
    }

    let res = skip_space1(input.clone())
        .or_else(|_| line_continuation(input.clone()))
        .or_else(|_| comment(input));

    res.and_then(|(input, _)| {
        delimiter_no_op(input.clone()).or_else(|_| Ok((input, ())))
    })
}

// XXX: maybe this should set a flag in the parser or something so can tell if we are incomplete
//      rather than an actual error
/// Parse a line continuation (a backslash followed by a newline).
fn line_continuation<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    debug!("line_continuation");

    is_next(input, r"\").and_then(|(input, _)| newline(input))
}

/// Parse (and then ignore) a comment.
fn comment<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    debug!("comment");

    let (input, _) = is_next(input, "#")?;
    Ok((skip_until_matches(input, newline), ()))
}

/// Check if the next item in `input` is an operator without advancing `input`.
fn op<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    debug!("op");

    match is_one_of(input.clone(), &["|", "&", ";", "<", ">", "(", ")", "$", "`", r"\", "\"", "'"]) {
        Ok(_) => Ok((input, ())),
        Err(_) => newline(input),
    }
}

// TODO: eliminate from as many parsers as possible
fn ignore<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    debug!("ignore");

    let input = skip_space0(input);
    line_continuation(input.clone())
        .and_then(|(input, _)| ignore(input))
        .or_else(|_| {
            comment(input.clone())
                .or_else(|_| Ok((input, ())))
        })
}

/// Parse a newline character.
fn newline<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    debug!("newline");

    // FIXME: locale support
    is_next(input, "\n")
}

/// Skip zero or more spaces in `input`.
fn skip_space0(input: ParseInput) -> ParseInput {
    debug!("skip_space0");

    // FIXME: locale support (plus less common ascii spaces)
    skip_values_many(input, &[" ", "\t"])
}

/// Skip one or more spaces in `input`.
fn skip_space1<'a>(input: ParseInput<'a>) -> ParseResult<'a, ()> {
    debug!("skip_space1");

    // FIXME: locale support (plus less common ascii spaces)
    let (input, _) = is_one_of(input, &[" ", "\t"])?;
    Ok((skip_values_many(input, &[" ", "\t"]), ()))
}

/// Skip zero or more spaces (including newlines) in `input`.
fn skip_whitespace0<'a>(input: ParseInput<'a>) -> ParseInput<'a> {
    let input = skip_space0(input);
    // XXX: should make skip_while_matches
    skip_until_matches(input, |input| is_not(input, newline))
}
