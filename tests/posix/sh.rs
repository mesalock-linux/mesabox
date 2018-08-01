//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use assert_cmd::prelude::*;
use predicates::prelude::*;
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
