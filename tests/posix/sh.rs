//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

use util::*;

const NAME: &str = "sh";

const PIPELINE: &str = "pipeline";
const PIPELINE_SUBSHELL: &str = "pipeline_subshell";

mod stdin {
    use super::*;

    #[test]
    fn test_pipeline() {
        new_ucmd!()
            .pipe_in_fixture(input_fixture(PIPELINE))
            .run()
            .stdout_is_fixture(expected_fixture(PIPELINE));
    }

    #[test]
    fn test_pipeline_subshell() {
        new_ucmd!()
            .pipe_in_fixture(input_fixture(PIPELINE_SUBSHELL))
            .run()
            .stdout_is_fixture(expected_fixture(PIPELINE_SUBSHELL));
    }

    // XXX: this is likely a parser issue (again)
    #[test]
    #[ignore]
    fn test_invalid_subshell_loc() {
        new_ucmd!()
            .pipe_in("echo hi | echo hello (echo hi; cat) | cat")
            .fails();
    }
}

mod script {
    use super::*;

    #[test]
    fn test_pipeline() {
        new_ucmd!()
            .arg(input_fixture(PIPELINE))
            .run()
            .stdout_is_fixture(expected_fixture(PIPELINE));
    }

    #[test]
    fn test_pipeline_subshell() {
        new_ucmd!()
            .arg(input_fixture(PIPELINE_SUBSHELL))
            .run()
            .stdout_is_fixture(expected_fixture(PIPELINE_SUBSHELL));
    }
}

fn input_fixture(name: &str) -> String {
    format!("{}.sh", name)
}

fn expected_fixture(name: &str) -> String {
    format!("{}.expected", name)
}
