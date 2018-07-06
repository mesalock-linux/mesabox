use super::{Lexer, Token};

named_args!(pub arith<'a>(lexer: &mut Lexer)<&'a [u8], Token<'a>>,
    do_parse!(
        // TODO: stuff goes here
        tag!("))") >>
        (unimplemented!())
    )
);
