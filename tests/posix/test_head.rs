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
use predicates::prelude::*;
use std::process::Command;

const INPUT: &str = "lorem_ipsum.txt";
const INPUT2: &str = "lorem_ipsum_reverse.txt";

#[test]
fn test_stdin_default() {
    new_cmd!()
        .with_stdin().path(fixtures_path!(INPUT)).unwrap()
        .assert()
        .success()
        .stdout(pred_eq_file!("lorem_ipsum_default.expected"))
        .stderr("");
}

#[test]
fn test_stdin_1_line_obsolete() {
    new_cmd!()
        .args(&["-1"])
        .with_stdin().path(fixtures_path!(INPUT)).unwrap()
        .assert()
        .success()
        .stdout(pred_eq_file!("lorem_ipsum_1_line.expected"))
        .stderr("");
}

#[test]
fn test_stdin_1_line() {
    new_cmd!()
        .args(&["-n", "1"])
        .with_stdin().path(fixtures_path!(INPUT)).unwrap()
        .assert()
        .success()
        .stdout(pred_eq_file!("lorem_ipsum_1_line.expected"))
        .stderr("");
}

#[test]
fn test_stdin_5_chars() {
    new_cmd!()
        .args(&["-c", "5"])
        .with_stdin().path(fixtures_path!(INPUT)).unwrap()
        .assert()
        .success()
        .stdout(pred_eq_file!("lorem_ipsum_5_chars.expected"))
        .stderr("");
}

#[test]
fn test_single_default() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .arg(INPUT)
        .assert()
        .success()
        .stdout(pred_eq_file!("lorem_ipsum_default.expected"))
        .stderr("");
}

#[test]
fn test_single_1_line_obsolete() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&["-1", INPUT])
        .assert()
        .success()
        .stdout(pred_eq_file!("lorem_ipsum_1_line.expected"))
        .stderr("");
}

#[test]
fn test_single_1_line() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&["-n", "1", INPUT])
        .assert()
        .success()
        .stdout(pred_eq_file!("lorem_ipsum_1_line.expected"))
        .stderr("");
}

#[test]
fn test_single_5_chars() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&["-c", "5", INPUT])
        .assert()
        .success()
        .stdout(pred_eq_file!("lorem_ipsum_5_chars.expected"))
        .stderr("");
}

#[test]
fn test_minus_1_line() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&["-n", "-1", INPUT])
        .assert()
        .success()
        .stdout(pred_eq_file!("lorem_ipsum_minus_1_line.expected"))
        .stderr("");
}

#[test]
fn test_minus_5_chars() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&["-c", "-5", INPUT])
        .assert()
        .success()
        .stdout(pred_eq_file!("lorem_ipsum_minus_5_chars.expected"))
        .stderr("");
}

#[test]
fn test_multiple_input_files() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&[INPUT, INPUT2])
        .assert()
        .success()
        .stdout(pred_eq_file!("lorem_ipsum_multiple_input_files.expected"))
        .stderr("");
}

#[test]
fn test_verbose() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&["-v", INPUT])
        .assert()
        .success()
        .stdout(pred_eq_file!("lorem_ipsum_verbose.expected"))
        .stderr("");
}
