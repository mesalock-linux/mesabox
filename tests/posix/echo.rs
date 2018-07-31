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

use assert_cmd::prelude::*;
use std::process::Command;

const NAME: &str = "echo";

#[test]
fn test_default() {
    new_cmd!()
        .arg("hi")
        .assert()
        .success()
        .stdout("hi\n")
        .stderr("");
}

#[test]
fn test_no_trailing_newline() {
    new_cmd!()
        .arg(r"hi\c")
        .assert()
        .success()
        .stdout("hi")
        .stderr("");
}

#[test]
fn test_escape_alert() {
    new_cmd!().arg(r"\a").assert().success().stdout("\x07\n").stderr("");
}

#[test]
fn test_escape_backslash() {
    new_cmd!().arg(r"\\").assert().success().stdout("\\\n").stderr("");
}

#[test]
fn test_escape_backspace() {
    new_cmd!().arg(r"\b").assert().success().stdout("\x08\n").stderr("");
}

#[test]
fn test_escape_carriage_return() {
    new_cmd!().arg(r"\r").assert().success().stdout("\r\n").stderr("");
}

#[test]
fn test_escape_form_feed() {
    new_cmd!().arg(r"\f").assert().success().stdout("\x0C\n").stderr("");
}

#[test]
fn test_escape_newline() {
    new_cmd!().arg(r"\na").assert().success().stdout("\na\n").stderr("");
}

#[test]
fn test_escape_no_further_output() {
    new_cmd!().arg(r"a\cb").assert().success().stdout("a").stderr("");
}

#[test]
fn test_escape_octal() {
    new_cmd!().arg(r"\0100").assert().success().stdout("@\n").stderr("");
}

#[test]
fn test_escape_tab() {
    new_cmd!().arg(r"\t").assert().success().stdout("\t\n").stderr("");
}

#[test]
fn test_escape_vertical_tab() {
    new_cmd!().arg(r"\v").assert().success().stdout("\x0B\n").stderr("");
}

#[test]
fn test_escape_others() {
    new_cmd!().arg(r"\o").assert().success().stdout("\\o\n").stderr("");
}

#[test]
fn test_escape_octal_break() {
    new_cmd!().arg(r"\0178").assert().success().stdout("\x0f\x38\n").stderr("");
}

#[test]
fn test_escape_octal_nothing() {
    new_cmd!().arg(r"\0").assert().success().stdout("\x00\n").stderr("");
}

#[test]
fn test_multiple_args() {
    new_cmd!().args(&[r"a", r"b", r"c"]).assert().success().stdout("a b c\n").stderr("");
}

#[test]
fn test_no_arg() {
    new_cmd!().assert().success().stdout("\n").stderr("");
}

#[test]
fn test_escape_none() {
    new_cmd!().arg(r"\").assert().success().stdout("\\\n").stderr("");
}

#[test]
fn test_many_backslash() {
    // should output 4 backslashes + newline
    new_cmd!().arg(r"\\\\\\\").assert().success().stdout("\\\\\\\\\n").stderr("");
}
