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

const INPUT: &str = "lorem_ipsum.txt";
const INPUT2: &str = "lorem_ipsum_reverse.txt";

#[test]
fn test_stdin_default() {
    new_ucmd!()
        .pipe_in_fixture(INPUT)
        .run().stdout_is_fixture("lorem_ipsum_default.expected");
}

#[test]
fn test_stdin_1_line_obsolete() {
    new_ucmd!()
        .args(&["-1"])
        .pipe_in_fixture(INPUT)
        .run().stdout_is_fixture("lorem_ipsum_1_line.expected");
}

#[test]
fn test_stdin_1_line() {
    new_ucmd!()
        .args(&["-n", "1"])
        .pipe_in_fixture(INPUT)
        .run().stdout_is_fixture("lorem_ipsum_1_line.expected");
}

#[test]
fn test_stdin_5_chars() {
    new_ucmd!()
        .args(&["-c", "5"])
        .pipe_in_fixture(INPUT)
        .run().stdout_is_fixture("lorem_ipsum_5_chars.expected");
}

#[test]
fn test_single_default() {
    new_ucmd!()
        .arg(INPUT)
        .run().stdout_is_fixture("lorem_ipsum_default.expected");
}

#[test]
fn test_single_1_line_obsolete() {
    new_ucmd!()
        .args(&["-1", INPUT])
        .run().stdout_is_fixture("lorem_ipsum_1_line.expected");
}

#[test]
fn test_single_1_line() {
    new_ucmd!()
        .args(&["-n", "1", INPUT])
        .run().stdout_is_fixture("lorem_ipsum_1_line.expected");
}

#[test]
fn test_single_5_chars() {
    new_ucmd!()
        .args(&["-c", "5", INPUT])
        .run().stdout_is_fixture("lorem_ipsum_5_chars.expected");
}

#[test]
fn test_minus_1_line() {
    new_ucmd!()
        .args(&["-n", "-1", INPUT])
        .run().stdout_is_fixture("lorem_ipsum_minus_1_line.expected");
}

#[test]
fn test_minus_5_chars() {
    new_ucmd!()
        .args(&["-c", "-5", INPUT])
        .run().stdout_is_fixture("lorem_ipsum_minus_5_chars.expected");
}

#[test]
fn test_multiple_input_files() {
    new_ucmd!()
        .args(&[INPUT, INPUT2])
        .run().stdout_is_fixture("lorem_ipsum_multiple_input_files.expected");
}

#[test]
fn test_verbose() {
    new_ucmd!()
        .args(&["-v", INPUT])
        .run().stdout_is_fixture("lorem_ipsum_verbose.expected");
}
