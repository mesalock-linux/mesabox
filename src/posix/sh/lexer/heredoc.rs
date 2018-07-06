use nom::{self, alphanumeric1, newline, space0};

use std::cell::RefCell;
use std::ffi::OsString;
use std::rc::Rc;
use std::os::unix::ffi::{OsStringExt, OsStrExt};

use super::{Lexer, Token, HereDoc, HereDocMarker, HereDocWord};

// NOTE: we are just handling heredocs in the lexer as they will mess up tokenization otherwise
named_args!(pub io_here<'a>(lexer: &mut Lexer)<&'a [u8], Token<'a>>,
    do_parse!(
        dash: opt!(tag!("-")) >>
        space0 >>
        delim: call!(here_end, lexer) >>
        // XXX: can there be more than one heredoc at a time? (the answer is yes, for different commands on the same line)
        ({
            // NOTE: not really happy about using an Rc here, so see if there's an easy way to do without it
            let heredoc = Rc::new(RefCell::new(HereDoc::default()));
            lexer.heredoc_markers.push(HereDocMarker {
                marker: delim,
                strip_tabs: dash.is_some(),
                heredoc: heredoc.clone()
            });
            Token::HereDoc(heredoc)
        })
    )
);

named_args!(here_end<'a>(lexer: &mut Lexer)<&'a [u8], HereDocWord>,
    // TODO: apply rule 3
    // FIXME: this is wrong (need to do quote removal and such)
    map!(alphanumeric1, |res| OsString::from_vec(res.to_owned()))
);

// this needs to read a heredoc until it encounters "\nEND MARKER\n"
// XXX: this could probably use some improvement
fn parse_heredoc<'a>(mut input: &'a [u8], lexer: &mut Lexer) -> nom::IResult<&'a [u8], ()> {
    for marker in &mut lexer.heredoc_markers {
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
    lexer.heredoc_markers.clear();

    Ok((input, ()))
}
