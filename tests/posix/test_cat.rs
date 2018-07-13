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

use assert_cli;

#[test]
fn test_output_multi_files_print_all_chars() {
    new_cli!()
        .with_args(&["alpha.txt", "256.txt", "-A", "-n"])
        .succeeds()
        .and()
        .stderr().is("")
        .stdout().is("     1\tabcde$\n     2\tfghij$\n     3\tklmno$\n     4\tpqrst$\n     \
                5\tuvwxyz$\n     6\t^@^A^B^C^D^E^F^G^H^I$\n     \
                7\t^K^L^M^N^O^P^Q^R^S^T^U^V^W^X^Y^Z^[^\\^]^^^_ \
                !\"#$%&\'()*+,-./0123456789:;\
                <=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~^?M-^@M-^AM-^\
                BM-^CM-^DM-^EM-^FM-^GM-^HM-^IM-^JM-^KM-^LM-^MM-^NM-^OM-^PM-^QM-^RM-^SM-^TM-^UM-^V\
                M-^WM-^XM-^YM-^ZM-^[M-^\\M-^]M-^^M-^_M- \
                M-!M-\"M-#M-$M-%M-&M-\'M-(M-)M-*M-+M-,M--M-.M-/M-0M-1M-2M-3M-4M-5M-6M-7M-8M-9M-:\
                M-;M-<M-=M->M-?M-@M-AM-BM-CM-DM-EM-FM-GM-HM-IM-JM-KM-LM-MM-NM-OM-PM-QM-RM-SM-TM-U\
                M-VM-WM-XM-YM-ZM-[M-\\M-]M-^M-_M-`M-aM-bM-cM-dM-eM-fM-gM-hM-iM-jM-kM-lM-mM-nM-oM-\
                pM-qM-rM-sM-tM-uM-vM-wM-xM-yM-zM-{M-|M-}M-~M-^?")
        .unwrap();
}

#[test]
fn test_numbered_lines_no_trailing_newline() {
    new_cli!()
        .with_args(&["nonewline.txt", "alpha.txt", "-n"])
        .succeeds()
        .and()
        .stderr().is("")
        .stdout().is("     1\ttext without a trailing newlineabcde\n     2\tfghij\n     \
                3\tklmno\n     4\tpqrst\n     5\tuvwxyz\n")
        .unwrap();
}

#[test]
fn test_stdin_show_nonprinting() {
    for same_param in vec!["-v", "--show-nonprinting"] {
        new_cli!()
            .with_args(&[same_param])
            .stdin("\t\0\n")
            .succeeds()
            .and()
            .stderr().is("")
            .stdout().is("\t^@\n")
            .unwrap();
    }
}

#[test]
fn test_stdin_show_tabs() {
    for same_param in vec!["-T", "--show-tabs"] {
        new_cli!()
            .with_args(&[same_param])
            .stdin("\t\0\n")
            .succeeds()
            .and()
            .stderr().is("")
            .stdout().is("^I\0\n")
            .unwrap();
    }
}


#[test]
fn test_stdin_show_ends() {
    for same_param in vec!["-E", "--show-ends"] {
        new_cli!()
            .with_args(&[same_param,"-"])
            .stdin("\t\0\n")
            .succeeds()
            .and()
            .stderr().is("")
            .stdout().is("\t\0$\n")
            .unwrap();
    }
}

#[test]
fn test_stdin_show_all() {
    for same_param in vec!["-A", "--show-all"] {
        new_cli!()
            .with_args(&[same_param])
            .stdin("\t\0\n")
            .succeeds()
            .and()
            .stderr().is("")
            .stdout().is("^I^@$\n")
            .unwrap();
    }
}

#[test]
fn test_stdin_nonprinting_and_endofline() {
    new_cli!()
        .with_args(&["-e"])
        .stdin("\t\0\n")
        .succeeds()
        .and()
        .stderr().is("")
        .stdout().is("\t^@$\n")
        .unwrap();
}

#[test]
fn test_stdin_nonprinting_and_tabs() {
    new_cli!()
        .with_args(&["-t"])
        .stdin("\t\0\n")
        .succeeds()
        .stderr().is("")
        .stdout().is("^I^@\n")
        .unwrap();
}

#[test]
fn test_stdin_squeeze_blank() {
    for same_param in vec!["-s", "--squeeze-blank"] {
        new_cli!()
            .with_args(&[same_param])
            .stdin("\n\na\n\n\n\n\nb\n\n\n")
            .succeeds()
            .stderr().is("")
            .stdout().is("\na\n\nb\n\n")
            .unwrap();
    }
}

#[test]
fn test_stdin_number_non_blank() {
    for same_param in vec!["-b", "--number-nonblank"] {
        new_cli!()
            .with_args(&[same_param, "-"])
            .stdin("\na\nb\n\n\nc")
            .succeeds()
            .stderr().is("")
            .stdout().is("\n     1\ta\n     2\tb\n\n\n     3\tc")
            .unwrap();
    }
}

#[test]
fn test_non_blank_overrides_number() {
    for same_param in vec!["-b", "--number-nonblank"] {
        new_cli!()
            .with_args(&[same_param, "-"])
            .stdin("\na\nb\n\n\nc")
            .succeeds()
            .stderr().is("")
            .stdout().is("\n     1\ta\n     2\tb\n\n\n     3\tc")
            .unwrap();
    }
}

#[test]
fn test_squeeze_blank_before_numbering() {
    for same_param in vec!["-s", "--squeeze-blank"] {
        new_cli!()
            .with_args(&[same_param, "-n", "-"])
            .stdin("a\n\n\nb")
            .succeeds()
            .stderr().is("")
            .stdout().is("     1\ta\n     2\t\n     3\tb")
            .unwrap();
    }
}



#[test]
fn test_domain_socket() {
    use std::thread;
    use std::os::unix::net::UnixListener;
    use tempfile::TempDir;
    use std::io::prelude::*;

    let dir = TempDir::new().expect("failed to create dir");
    let socket_path = dir.path().join("sock");
    let listener = UnixListener::bind(&socket_path).expect("failed to create socket");

    let thread = thread::spawn(move || {
        let mut stream = listener.accept().expect("failed to accept connection").0;
        stream.write_all(b"a\tb").expect("failed to write test data");
    });

    new_cli!()
        .with_args(&[socket_path])
        .succeeds()
        .stderr().is("")
        .stdout().is("a\tb")
        .unwrap();

    thread.join().unwrap();
}
