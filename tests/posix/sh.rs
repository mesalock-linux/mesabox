//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

// NOTE: the compiler claims this is unused, but that's a lie and removing this line will cause
//       compilation to fail
#[allow(unused_imports)]
use assert_cmd::prelude::*;
use predicates::prelude::*;
use timebomb;
use std::process::Command;

const NAME: &str = "sh";

const PIPELINE: &str = "pipeline";
const PIPELINE_SUBSHELL: &str = "pipeline_subshell";

mod stdin {
    use super::*;

    #[test]
    fn test_pipeline() {
        new_cmd!()
            .with_stdin().path(fixtures_path!(input_fixture(PIPELINE))).unwrap()
            .assert()
            .success()
            .stdout(pred_eq_file!(expected_fixture(PIPELINE)))
            .stderr("");
    }

    #[test]
    fn test_pipeline_subshell() {
        new_cmd!()
            .with_stdin().path(fixtures_path!(input_fixture(PIPELINE_SUBSHELL))).unwrap()
            .assert()
            .success()
            .stdout(pred_eq_file!(expected_fixture(PIPELINE_SUBSHELL)))
            .stderr("");
    }

    #[test]
    fn test_invalid_subshell_loc() {
        new_cmd!()
            .with_stdin().buffer("echo hi | echo hello (echo hi; cat) | cat")
            .assert()
            .failure();
    }

    #[test]
    fn test_param_value_no_braces() {
        new_cmd!()
            .with_stdin().buffer("x=hi; echo $x")
            .assert()
            .success()
            .stdout("hi\n")
            .stderr("");
    }

    #[test]
    fn test_param_value_braces() {
        new_cmd!()
            .with_stdin().buffer("x=hi; echo ${x}")
            .assert()
            .success()
            .stdout("hi\n")
            .stderr("");
    }

    #[test]
    fn test_define_function_one_line() {
        // we execute the function twice to ensure it doesn't run the command during definition
        new_cmd!()
            .with_stdin().buffer("test() { echo hi; }; test; test")
            .assert()
            .success()
            .stdout("hi\nhi\n")
            .stderr("");
    }

    #[test]
    fn test_define_function_multiple_lines() {
        new_cmd!()
            .with_stdin().buffer("test() {\n  echo hi\n}\ntest\ntest")
            .assert()
            .success()
            .stdout("hi\nhi\n")
            .stderr("");
    }

    // TODO: add tests ensuring that positional parameters and special parameters can't be assigned
    #[test]
    fn test_param_assign_null_with_unset() {
        new_cmd!()
            .with_stdin().buffer("echo ${hi:=hello}; echo $hi")
            .assert()
            .success()
            .stdout("hello\nhello\n")
            .stderr("");
    }

    #[test]
    fn test_param_assign_null_with_null() {
        new_cmd!()
            .with_stdin().buffer("hi=; echo $hi; echo ${hi:=hello}; echo $hi")
            .assert()
            .success()
            .stdout("\nhello\nhello\n")
            .stderr("");
    }

    #[test]
    fn test_param_assign_null_with_value() {
        new_cmd!()
            .with_stdin().buffer("hi=value; echo $hi; echo ${hi:=hello}; echo $hi")
            .assert()
            .success()
            .stdout("value\nvalue\nvalue\n")
            .stderr("");
    }

    #[test]
    fn test_param_assign_with_unset() {
        new_cmd!()
            .with_stdin().buffer("echo ${hi=hello}; echo $hi")
            .assert()
            .success()
            .stdout("hello\nhello\n")
            .stderr("");
    }

    #[test]
    fn test_param_assign_with_null() {
        new_cmd!()
            .with_stdin().buffer("hi=; echo $hi; echo ${hi=hello}; echo $hi")
            .assert()
            .success()
            .stdout("\n\n\n")
            .stderr("");
    }

    #[test]
    fn test_param_assign_with_value() {
        new_cmd!()
            .with_stdin().buffer("hi=value; echo $hi; echo ${hi=hello}; echo $hi")
            .assert()
            .success()
            .stdout("value\nvalue\nvalue\n")
            .stderr("");
    }

    #[test]
    fn test_param_use_null_with_unset() {
        new_cmd!()
            .with_stdin().buffer("echo ${hi:-hello}; echo $hi")
            .assert()
            .success()
            .stdout("hello\n\n")
            .stderr("");
    }

    #[test]
    fn test_param_use_null_with_null() {
        new_cmd!()
            .with_stdin().buffer("hi=; echo $hi; echo ${hi:-hello}; echo $hi")
            .assert()
            .success()
            .stdout("\nhello\n\n")
            .stderr("");
    }

    #[test]
    fn test_param_use_null_with_value() {
        new_cmd!()
            .with_stdin().buffer("hi=value; echo $hi; echo ${hi:-hello}; echo $hi")
            .assert()
            .success()
            .stdout("value\nvalue\nvalue\n")
            .stderr("");
    }

    #[test]
    fn test_param_use_with_unset() {
        new_cmd!()
            .with_stdin().buffer("echo ${hi-hello}; echo $hi")
            .assert()
            .success()
            .stdout("hello\n\n")
            .stderr("");
    }

    #[test]
    fn test_param_use_with_null() {
        new_cmd!()
            .with_stdin().buffer("hi=; echo $hi; echo ${hi-hello}; echo $hi")
            .assert()
            .success()
            .stdout("\n\n\n")
            .stderr("");
    }

    #[test]
    fn test_param_use_with_value() {
        new_cmd!()
            .with_stdin().buffer("hi=value; echo $hi; echo ${hi-hello}; echo $hi")
            .assert()
            .success()
            .stdout("value\nvalue\nvalue\n")
            .stderr("");
    }

    #[test]
    fn test_param_alternate_null_with_unset() {
        new_cmd!()
            .with_stdin().buffer("echo ${hi+hello}; echo $hi")
            .assert()
            .success()
            .stdout("\n\n")
            .stderr("");
    }

    #[test]
    fn test_param_alternate_null_with_null() {
        new_cmd!()
            .with_stdin().buffer("hi=; echo $hi; echo ${hi+hello}; echo $hi")
            .assert()
            .success()
            .stdout("\nhello\n\n")
            .stderr("");
    }

    #[test]
    fn test_param_alternate_null_with_value() {
        new_cmd!()
            .with_stdin().buffer("hi=value; echo $hi; echo ${hi+hello}; echo $hi")
            .assert()
            .success()
            .stdout("value\nhello\nvalue\n")
            .stderr("");
    }

    #[test]
    fn test_param_alternate_with_unset() {
        new_cmd!()
            .with_stdin().buffer("echo ${hi:+hello}; echo $hi")
            .assert()
            .success()
            .stdout("\n\n")
            .stderr("");
    }

    #[test]
    fn test_param_alternate_with_null() {
        new_cmd!()
            .with_stdin().buffer("hi=; echo $hi; echo ${hi:+hello}; echo $hi")
            .assert()
            .success()
            .stdout("\n\n\n")
            .stderr("");
    }

    #[test]
    fn test_param_alternate_with_value() {
        new_cmd!()
            .with_stdin().buffer("hi=value; echo $hi; echo ${hi:+hello}; echo $hi")
            .assert()
            .success()
            .stdout("value\nhello\nvalue\n")
            .stderr("");
    }

    // TODO: test ${var?val}, ${var:?val}, ${var?}, etc.

    #[test]
    fn test_if_simple() {
        new_cmd!()
            .with_stdin().buffer("if true; then echo hello; fi; if false; then echo whoops; fi")
            .assert()
            .success()
            .stdout("hello\n")
            .stderr("");
    }

    #[test]
    fn test_if_elif_simple() {
        new_cmd!()
            .with_stdin().buffer("if false; then echo whoops; elif true; then echo yay; fi")
            .assert()
            .success()
            .stdout("yay\n")
            .stderr("");
    }

    #[test]
    fn test_if_elif_first_found() {
        new_cmd!()
            .with_stdin().buffer("if false; then echo whoops; elif true; then echo yay; elif true; then echo whoops again; fi")
            .assert()
            .success()
            .stdout("yay\n")
            .stderr("");
    }

    #[test]
    fn test_if_elif_if_works() {
        new_cmd!()
            .with_stdin().buffer("if true; then echo yay; elif true; then echo whoops; fi")
            .assert()
            .success()
            .stdout("yay\n")
            .stderr("");
    }

    #[test]
    fn test_if_else() {
        new_cmd!()
            .with_stdin().buffer("if false; then echo whoops; else echo yay; fi")
            .assert()
            .success()
            .stdout("yay\n")
            .stderr("");
    }

    #[test]
    fn test_if_elif_else() {
        new_cmd!()
            .with_stdin().buffer("if false; then echo wrong; elif false; then echo wrong; else echo correct; fi")
            .assert()
            .success()
            .stdout("correct\n")
            .stderr("");
    }

    #[test]
    fn test_for_simple_params() {
        new_cmd!()
            .with_stdin().buffer("for var in x y z; do echo ${var}; done")
            .assert()
            .success()
            .stdout("x\ny\nz\n")
            .stderr("");
    }

    #[test]
    fn test_cmd_subst_simple() {
        new_cmd!()
            .with_stdin().buffer("echo $(echo testing)")
            .assert()
            .success()
            .stdout("testing\n")
            .stderr("");
    }

    #[test]
    fn test_cmd_subst_var() {
        new_cmd!()
            .with_stdin().buffer("x=$(echo value; echo value2); echo before \"$x\" after")
            .assert()
            .success()
            .stdout("before value\nvalue2 after\n")
            .stderr("");
    }

    // FIXME: this failure is due to us not handling IFS
    #[test]
    #[ignore]
    fn test_cmd_subst_var_with_expansion() {
        new_cmd!()
            .with_stdin().buffer("x=$(echo value; echo value2); echo before $x after")
            .assert()
            .success()
            .stdout("before value value2 after\n")
            .stderr("");
    }

    #[test]
    fn test_word_inner_single_quote() {
        timebomb::timeout_ms(|| {
            new_cmd!()
                .with_stdin().buffer("echo test'ing values    here'")
                .assert()
                .success()
                .stdout("testing values    here\n")
                .stderr("");
        }, 5000);
    }

    #[test]
    fn test_word_single_quote() {
        timebomb::timeout_ms(|| {
            new_cmd!()
                .with_stdin().buffer("echo 'testing  many  words'")
                .assert()
                .success()
                .stdout("testing  many  words\n")
                .stderr("");
        }, 5000);
    }

    #[test]
    fn test_heredoc_simple() {
        new_cmd!()
            .with_stdin().buffer("cat <<eof1\nHello,\nMore stuff\neof1")
            .assert()
            .success()
            .stdout("Hello,\nMore stuff\n")
            .stderr("");
    }

    #[test]
    fn test_heredoc_multiple() {
        new_cmd!()
            .with_stdin().buffer("cat <<eof1; cat <<eof2\nHello,\nMore stuff\neof1\nother data\nmore data\neof2")
            .assert()
            .success()
            .stdout("Hello,\nMore stuff\nother data\nmore data\n")
            .stderr("");
    }

    #[test]
    fn test_heredoc_multiple_no_data() {
        new_cmd!()
            .with_stdin().buffer("cat <<eof1; cat <<eof2\neof1\neof2")
            .assert()
            .success()
            .stdout("")
            .stderr("");
    }
}

mod script {
    use super::*;

    #[test]
    fn test_pipeline() {
        new_cmd!()
            .arg(fixtures_path!(input_fixture(PIPELINE)))
            .assert()
            .success()
            .stdout(pred_eq_file!(expected_fixture(PIPELINE)))
            .stderr("");
    }

    #[test]
    fn test_pipeline_subshell() {
        new_cmd!()
            .arg(fixtures_path!(input_fixture(PIPELINE_SUBSHELL)))
            .assert()
            .success()
            .stdout(pred_eq_file!(expected_fixture(PIPELINE_SUBSHELL)))
            .stderr("");
    }
}

fn input_fixture(name: &str) -> String {
    format!("{}.sh", name)
}

fn expected_fixture(name: &str) -> String {
    format!("{}.expected", name)
}
