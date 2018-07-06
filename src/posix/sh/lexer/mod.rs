use nom::{self, *};

use std::cell::RefCell;
use std::ffi::OsString;
use std::rc::Rc;
use std::os::unix::ffi::OsStringExt;

use super::ast::{HereDoc, IoRedirectFile, IoRedirectKind, Param};

pub use self::iter::TokenIter;
use self::arith::arith;
use self::heredoc::io_here;
use self::param::{param, param_name};

mod arith;
mod heredoc;
mod param;
mod iter;

#[derive(Debug)]
pub struct Lexer {
    after_newline: bool,
    // TODO: add the "strip tabs" feature (the dash after <<)
    heredoc_markers: Vec<HereDocMarker>,
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            after_newline: false,
            heredoc_markers: vec![],
        }
    }

    method!(pub tokenize<Lexer, &[u8], Vec<Token>>, mut self,
        call!(tokenize, &mut self)
    );
}

fn tokenize_with_op<'a, F: Fn(u8) -> bool>(input: &'a [u8], lexer: &mut Lexer, op_fn: &F) -> nom::IResult<&'a [u8], Vec<Token<'a>>> {
    fold_many1!(input,
        call!(token, lexer, |input: &'a [u8], lexer: &mut Lexer| call!(input, single_token, lexer, op_fn)),
        vec![],
        |mut acc: Vec<_>, (token, delim): (Option<_>, Vec<_>)| {
            if let Some(token) = token {
                acc.push(token);
            }
            acc.extend(delim.into_iter());
            acc
        }
    )
}

named_args!(tokenize<'a>(lexer: &mut Lexer)<&'a [u8], Vec<Token<'a>>>,
    call!(tokenize_with_op, lexer, &is_normal_op)
);

// TODO: FIGURE OUT HOW TO HANDLE NAMES (should they be Words and then we just split the word if
//       the latter part contains invalid characters for a name?  problem with this is it makes
//       parsing somewhat more difficult)
#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Parameter(Box<ParamExprToken<'a>>),
    CommandSubst(Vec<Token<'a>>),
    // TODO: arithmetic expressions are not supported right now
    ArithExpr,
    SingleQuote(&'a [u8]),
    DoubleQuote(Vec<DoubleQuotePart<'a>>),
    Word(&'a [u8]),
    Escape(u8),
    ComplexWord(Vec<Token<'a>>),

    // because we need to handle heredocs here, we handle redirections as well
    HereDoc(Rc<RefCell<HereDoc>>),
    IoRedirect(IoRedirectKind),

    LineContinuation,
    Newline,
    And,        // &&
    Or,         // ||
    Background, // &
    Pipe,       // |
    Equal,      // =
    DoubleSemi, // ;;
    CommandEnd, // ;
    LeftParen,  // (
    RightParen, // )

}

impl<'a> Token<'a> {
    //pub fn to_word(&self) 
/*
    pub fn as_name(&self) -> OsString {

    }*/
}

#[derive(Debug, PartialEq)]
pub struct ParamExprToken<'a> {
    pub param: ParamName<'a>,
    pub kind: ParamExprKind,
    pub rhs: Vec<Token<'a>>,
}

impl<'a> ParamExprToken<'a> {
    pub fn new(name: ParamName<'a>, kind: ParamExprKind, rhs: Vec<Token<'a>>) -> Self {
        Self {
            param: name,
            kind: kind,
            rhs: rhs,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParamName<'a> {
    Var(&'a [u8]),
    Star,
    Question,
    At,
}

impl<'a> ParamName<'a> {
    pub fn to_param(&self) -> Param {
        match self {
            ParamName::Var(name) => Param::Var(OsString::from_vec(name.to_vec())),
            ParamName::Question => Param::Question,
            ParamName::Star => Param::Star,
            ParamName::At => Param::At,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParamExprKind {
    Assign,
    AssignNull,

    Use,
    UseNull,

    Error,
    ErrorNull,

    Alternate,
    AlternateNull,

    SmallPrefix,
    LargePrefix,

    SmallSuffix,
    LargeSuffix,

    Value,
    Length,
}

#[derive(Debug, PartialEq)]
pub enum DoubleQuotePart<'a> {
    Parameter(ParamExprToken<'a>),
    CommandSubst(Vec<Token<'a>>),
    // TODO: arithmetic expressions are not supported right now
    ArithExpr,
    Escape(u8),
    Text(&'a [u8]),
}

trait StoreItems<T> {
    fn store_many(data: Vec<T>) -> Self;
    fn store_one(item: T) -> Self;
}

impl<'a> StoreItems<Token<'a>> for Token<'a> {
    fn store_many(tokens: Vec<Token<'a>>) -> Self {
        Token::ComplexWord(tokens)
    }

    fn store_one(token: Token<'a>) -> Self {
        token
    }
}

impl<'a> StoreItems<DoubleQuotePart<'a>> for Token<'a> {
    fn store_many(quote_parts: Vec<DoubleQuotePart<'a>>) -> Self {
        Token::DoubleQuote(quote_parts)
    }

    fn store_one(part: DoubleQuotePart<'a>) -> Self {
        Token::DoubleQuote(vec![part])
    }
}

#[derive(Debug)]
struct HereDocMarker {
    marker: HereDocWord,
    strip_tabs: bool,
    heredoc: Rc<RefCell<HereDoc>>,
}

// NOTE: for better or worse, the heredoc ending word seems to be pretty much literal (it removes quotes but does not expand anything)
//       we thus need to process it specially
pub type HereDocWord = OsString;

fn token<'a, F, T: StoreItems<U>, U>(input: &'a [u8], lexer: &mut Lexer, token_fn: F) -> nom::IResult<&'a [u8], (Option<T>, Vec<Token<'a>>)>
where
    F: Fn(&'a [u8], &mut Lexer) -> nom::IResult<&'a [u8], Option<U>>
{
    do_parse!(input,
        first: call!(token_fn, lexer) >>
        res: alt_complete!(
            delimiter => { |delim| (first.map(|val| T::store_one(val)), delim) } |
            do_parse!(
                res: fold_many1!(call!(token_fn, lexer), vec![first.unwrap()], |mut acc: Vec<_>, item| {
                    if let Some(item) = item {
                        acc.push(item);
                    }
                    acc
                }) >>
                delim: delimiter >>
                ((Some(T::store_many(res)), delim))
            )
        ) >>
        (res)
    )
}

named_args!(in_quote_tokenize<'a>(lexer: &mut Lexer)<&'a [u8], Vec<DoubleQuotePart<'a>>>,
    fold_many0!(
        do_parse!(
            not!(tag!("\"")) >>
            res: call!(in_quote_token, lexer) >>
            (res)
        ),
        vec![],
        |mut acc: Vec<_>, part: DoubleQuotePart<'a>| {
            acc.push(part);
            acc
        }
    )
);

named_args!(in_quote_token<'a>(lexer: &mut Lexer)<&'a [u8], DoubleQuotePart<'a>>,
    alt_complete!(
        switch!(take!(1),
            b"$" => map!(call!(param_subst_arith, lexer, is_normal_op), |token| {
                match token {
                    // FIXME: the Box for the ParamExprToken is a wasted allocation
                    Token::Parameter(expr) => DoubleQuotePart::Parameter(*expr),
                    Token::CommandSubst(subst) => DoubleQuotePart::CommandSubst(subst),
                    Token::ArithExpr => DoubleQuotePart::ArithExpr,
                    _ => unreachable!()                    
                }
            }) |
            b"`" => map!(call!(backtick_subst, lexer), |token| {
                match token {
                    Token::CommandSubst(subst) => DoubleQuotePart::CommandSubst(subst),
                    _ => unreachable!()
                }
            }) |
            b"\\" => map!(in_quote_escape_seq, |token| {
                match token {
                    Token::Escape(ch) => DoubleQuotePart::Escape(ch),
                    _ => unreachable!()
                }
            })
        ) |
        is_not!("\"$`\\") => { |text: &'a [u8]| DoubleQuotePart::Text(text) }
    )
);

fn single_token<'a, F: Fn(u8) -> bool>(input: &'a [u8], lexer: &mut Lexer, op_fn: &F) -> nom::IResult<&'a [u8], Option<Token<'a>>> {
    map!(input, alt_complete!(
        switch!(take!(1),
            b"$" => call!(param_subst_arith, lexer, op_fn) |
            b"`" => call!(backtick_subst, lexer) |
            b"'" => call!(single_quote) |
            b"\"" => call!(double_quote, lexer) |
            /*b"&" => call!(and_op_or_background) |
            b"|" => call!(or_op_or_pipe) |
            b";" => call!(semi_or_dsemi) |*/
            b"(" => value!(Token::LeftParen) |
            b")" => value!(Token::RightParen) |
            b"<" => call!(input_redirect, lexer) |
            b">" => call!(output_redirect) |
            b"#" => do_parse!(
                comment >>
                (Token::Newline)
            ) |
            b"\\" => call!(escape_seq)
        ) |
        call!(word, op_fn)
    ), |v| {
        if v == Token::Newline {
            None
        } else {
            Some(v)
        }
    })
}

named_args!(single_token_filename<'a>(lexer: &mut Lexer)<&'a [u8], Option<Token<'a>>>,
    map!(alt_complete!(
        switch!(take!(1),
            b"$" => call!(param_subst_arith, lexer, is_normal_op) |
            b"`" => call!(backtick_subst, lexer) |
            b"'" => call!(single_quote) |
            b"\"" => call!(double_quote, lexer) |
            b"<" => call!(input_redirect, lexer) |
            b">" => call!(output_redirect) |
            b"\\" => call!(escape_seq)
        ) |
        call!(word, is_normal_op)
    ), |v| Some(v))
);

named_args!(input_redirect<'a>(lexer: &mut Lexer)<&'a [u8], Token<'a>>,
    alt_complete!(
        switch!(take!(1),
            b"<" => call!(io_here, lexer) |
            b"&" => value!(Token::IoRedirect(IoRedirectKind::DupInput)) |
            b">" => value!(Token::IoRedirect(IoRedirectKind::ReadWrite))
        ) |
        value!(Token::IoRedirect(IoRedirectKind::Input))
    )
);

named_args!(output_redirect<'a>()<&'a [u8], Token<'a>>,
    alt_complete!(
        switch!(take!(1),
            b">" => value!(Token::IoRedirect(IoRedirectKind::Append)) |
            b"&" => value!(Token::IoRedirect(IoRedirectKind::DupOutput)) |
            b"|" => value!(Token::IoRedirect(IoRedirectKind::Clobber))
        ) |
        value!(Token::IoRedirect(IoRedirectKind::Output))
    )
);

// FIXME: this should not eat the delimiter (solution will probably be to create another function to work as token() here)
named_args!(filename<'a>(lexer: &mut Lexer)<&'a [u8], Token<'a>>,
    map!(call!(token, lexer, single_token_filename), |res| res.0.unwrap())
);

pub fn name(input: &[u8]) -> nom::IResult<&[u8], &[u8]> {
    use nom::types::CompleteByteSlice;
    // avoid nom's annoying Incomplete by wrapping the input in CompleteByteSlice
    recognize!(CompleteByteSlice(input),
        pair!(
            alt_complete!(alpha1 | tag!("_")),
            many0!(alt_complete!(alphanumeric1 | tag!("_")))
        )
    ).map(|(CompleteByteSlice(inp), CompleteByteSlice(output))| {
        (inp, output)
    }).map_err(|err| {
        match err {
            nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
            nom::Err::Error(nom::Context::Code(CompleteByteSlice(inp), e)) => nom::Err::Error(nom::Context::Code(inp, e)),
            nom::Err::Failure(nom::Context::Code(CompleteByteSlice(inp), e)) => nom::Err::Failure(nom::Context::Code(inp, e)),
            _ => unreachable!()
        }
    })
}

named!(and_op_or_background<&[u8], Token>,
    alt_complete!(
        tag!("&") => { |_| Token::And } |
        value!(()) => { |_| Token::Background }
    )
);

named!(or_op_or_pipe<&[u8], Token>,
    alt_complete!(
        tag!("|") => { |_| Token::Or } |
        value!(()) => { |_| Token::Pipe }
    )
);

named!(semi_or_dsemi<&[u8], Token>,
    alt_complete!(
        tag!(";") => { |_| Token::DoubleSemi } |
        // FIXME: doesn't like this either (so tokenize is broken)
        value!(()) => { |_| Token::CommandEnd }
    )
);

fn param_subst_arith<'a, F: Fn(u8) -> bool>(input: &'a [u8], lexer: &mut Lexer, op_fn: F) -> nom::IResult<&'a [u8], Token<'a>> {
    switch!(input, peek!(take!(1)),
        // FIXME: this only seems to work without rhs values (this + command subst problem seem to indicate issue with outer tokenization funcs)
        b"{" => preceded!(tag!("{"), call!(param, lexer)) |
        // FIXME: not being recognized
        b"(" => preceded!(tag!("("), call!(subst_arith, lexer)) |
        // check if there's anything that could start an operator (if so, we are NOT a parameter)
        ch => alt_complete!(
            cond_reduce!(!is_param_op(ch[0]), map!(
                call!(param_name),
                |res| {
                    Token::Parameter(Box::new(ParamExprToken::new(res, ParamExprKind::Value, Vec::with_capacity(0))))
                }
            )) |
            // FIXME; need to somehow include $
            call!(word, op_fn)
        )
    )
}

named_args!(subst_arith<'a>(lexer: &mut Lexer)<&'a [u8], Token<'a>>,
    alt_complete!(
        do_parse!(
            tag!("(") >>
            res: call!(arith, lexer) >>
            (res)
        ) |
        call!(dollar_subst, lexer)
    )
);

named_args!(backtick_subst<'a>(lexer: &mut Lexer)<&'a [u8], Token<'a>>,
    terminated!(
        map!(call!(tokenize, lexer), |res| Token::CommandSubst(res)),
        tag!("`")
    )
);

named_args!(dollar_subst<'a>(lexer: &mut Lexer)<&'a [u8], Token<'a>>,
    terminated!(
        // FIXME: the issue with this (i think) is that tokenize requires a delimeter at the end
        map!(call!(tokenize, lexer), |res| Token::CommandSubst(res)),
        tag!(")")
    )
);

named!(single_quote<&[u8], Token>,
    map!(take_until_and_consume!("'"), |res| Token::SingleQuote(res))
);

named_args!(double_quote<'a>(lexer: &mut Lexer)<&'a [u8], Token<'a>>,
    map!(terminated!(call!(in_quote_tokenize, lexer), tag!("\"")), |res| Token::DoubleQuote(res))
);

// XXX: don't like this as it will lead to unnecessary allocations (due to splitting into too many
//      tokens)
named_args!(escape_seq_common<'a>(in_quote: bool)<&'a [u8], Token>,
    alt_complete!(
        newline => { |_| Token::LineContinuation } |
        tag!("$") => { |_| Token::Escape(b'$') } |
        tag!("`") => { |_| Token::Escape(b'`') } |
        tag!("\"") => { |_| Token::Escape(b'"') } |
        tag!("\\") => { |_| Token::Escape(b'\\') } |
        cond_reduce!(!in_quote, take!(1)) => { |ch: &[u8]| Token::Escape(ch[0]) } |
        // FIXME: we should not need to allocate for this
        value!(()) => { |_| Token::Word(b"\\") }
    )
);

named!(in_quote_escape_seq<&[u8], Token>,
    call!(escape_seq_common, true)
);

named!(escape_seq<&[u8], Token>,
    call!(escape_seq_common, false)
);

// FIXME
fn word<'a, F: Fn(u8) -> bool>(input: &'a [u8], op_fn: F) -> nom::IResult<&'a [u8], Token<'a>> {
    map!(input, take_till1!(|ch| delimiter(&[ch]).is_ok() || op_fn(ch)), |res| Token::Word(res))
}

// XXX: does this work on the last line of a file?
named!(comment,
    //do_parse!(res: not_line_ending >> (res))
    take_until!("\n")
);

named!(blank,
    do_parse!(res: space1 >> (res))
);

// TODO: add newlines to list of read tokens (this is to increment parser linenum and such)
// XXX: might want to add stuff like ';' to list of delimiters, but POSIX seems to treat ';' by itself as an error
// XXX: because i keep thinking about adding things here, maybe instead of calling single_token() over and over
//      in token(), have a complex_word() function that returns either Token::ComplexWord or one of the other things
//      that can exist in a word (such as Token::DoubleQuote or Token::SingleQuote).  it may be possible to remove
//      this delimiter rule in that case (or reserved it for blanks/eof only)
named_args!(delimiter<'a>()<&'a [u8], Vec<Token<'a>>>,
    fold_many1!(
        alt_complete!(
            blank => { |_| None } |
            newline => { |_| Some(Token::Newline) } |
            eof!() => { |_| None } |
            // XXX: should we be using op_fn?
            map!(switch!(peek!(take!(1)),
                b"&" => preceded!(take!(1), call!(and_op_or_background)) |
                b"|" => preceded!(take!(1), call!(or_op_or_pipe)) |
                b";" => preceded!(take!(1), call!(semi_or_dsemi))
            ), |token| Some(token))
        ),
        Vec::with_capacity(0),
        |mut acc: Vec<_>, item| {
            if let Some(val) = item {
                acc.push(val);
            }
            acc
        }
    )
);

fn is_normal_op(ch: u8) -> bool {
    // XXX: not sure about \
    // XXX: check about the "special circumstances" characters
    [b'|', b'&', b';', b'<', b'>', b'(', b')', b'$', b'`', b'\\', b'"', b'\'', b'\\', b' ', b'\t', b'\n'].contains(&ch)
}

fn is_param_op(ch: u8) -> bool {
    is_normal_op(ch) || ch == b'}'
}
