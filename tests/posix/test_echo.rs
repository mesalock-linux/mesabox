//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//
// This file incorporates work covered by the following copyright and
// permission notice:
//
//     Copyright (c) 2013-2018, Jordi Boggiano
//     Copyright (c) 2013-2018, Alex Lyon
//
//     Permission is hereby granted, free of charge, to any person obtaining a
//     copy of this software and associated documentation files (the
//     "Software"), to deal in the Software without restriction, including
//     without limitation the rights to use, copy, modify, merge, publish,
//     distribute, sublicense, and/or sell copies of the Software, and to
//     permit persons to whom the Software is furnished to do so, subject to
//     the following conditions:
//
//     The above copyright notice and this permission notice shall be included
//     in all copies or substantial portions of the Software.
//
//     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
//     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
//     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
//     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
//     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
//     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
//     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//

use util::*;

#[test]
fn test_default() {
    //CmdResult.stdout_only(...) trims trailing newlines
    assert_eq!("hi\n".as_bytes(), &new_ucmd!().arg("hi").succeeds().no_stderr().stdout[..]);
}

#[test]
fn test_no_trailing_newline() {
    //CmdResult.stdout_only(...) trims trailing newlines
    assert_eq!("hi".as_bytes(), &new_ucmd!().arg(r"hi\c").succeeds().no_stderr().stdout[..]);
}

#[test]
fn test_escape_alert() {
    new_ucmd!().arg(r"\a").succeeds().stdout_only("\x07\n");
}

#[test]
fn test_escape_backslash() {
    new_ucmd!().arg(r"\\").succeeds().stdout_only("\\\n");
}

#[test]
fn test_escape_backspace() {
    new_ucmd!().arg(r"\b").succeeds().stdout_only("\x08\n");
}

#[test]
fn test_escape_carriage_return() {
    new_ucmd!().arg(r"\r").succeeds().stdout_only("\r\n");
}

#[test]
fn test_escape_form_feed() {
    new_ucmd!().arg(r"\f").succeeds().stdout_only("\x0C\n");
}

#[test]
fn test_escape_newline() {
    new_ucmd!().arg(r"\na").succeeds().stdout_only("\na\n");
}

#[test]
fn test_escape_no_further_output() {
    new_ucmd!().arg(r"a\cb").succeeds().stdout_only("a");
}

#[test]
fn test_escape_octal() {
    new_ucmd!().arg(r"\0100").succeeds().stdout_only("@\n");
}

#[test]
fn test_escape_tab() {
    new_ucmd!().arg(r"\t").succeeds().stdout_only("\t\n");
}

#[test]
fn test_escape_vertical_tab() {
    new_ucmd!().arg(r"\v").succeeds().stdout_only("\x0B\n");
}

#[test]
fn test_escape_others() {
    new_ucmd!().arg(r"\o").succeeds().stdout_only("\\o\n");
}

#[test]
fn test_escape_octal_break() {
    new_ucmd!().arg(r"\0178").succeeds().stdout_only("\x0f\x38\n");
}

#[test]
fn test_escape_octal_nothing() {
    new_ucmd!().arg(r"\0").succeeds().stdout_only("\x00\n");
}

#[test]
fn test_multiple_args() {
    new_ucmd!().args(&[r"a", r"b", r"c"]).succeeds().stdout_only("a b c\n");
}

#[test]
fn test_no_arg() {
    new_ucmd!().succeeds().stdout_only("\n");
}

#[test]
fn test_escape_none() {
    new_ucmd!().arg(r"\").succeeds().stdout_only("\\\n");
}
