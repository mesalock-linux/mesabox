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
//     Copyright (c) 2015,      Joseph Crail
//     Copyright (c) 2015-2016, Nathan Ross
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

#![allow(unused)]

use std::borrow::Cow;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Result, Write};
use std::os::unix::ffi::OsStringExt;
#[cfg(unix)]
use std::os::unix::fs::symlink as symlink_file;
#[cfg(windows)]
use std::os::windows::fs::symlink_file;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::thread::{self, sleep, JoinHandle};
use std::time::Duration;

use mesabox::{self, UtilSetup};
use tempfile::TempDir;

const TESTS_DIR: &str = "tests";
const FIXTURES_DIR: &str = "fixtures";

const ALREADY_RUN: &str = " you have already run this UCommand, if you want to run \
                           another command in the same test, use TestScenario::new instead of \
                           testing();";
const MULTIPLE_STDIN_MEANINGLESS: &str = "Ucommand is designed around a typical use case of: provide args and input stream -> spawn process -> block until completion -> return output streams. For verifying that a particular section of the input stream is what causes a particular behavior, use the Command type directly.";

fn read_scenario_fixture<S: AsRef<OsStr>>(tmpd: &Option<Rc<TempDir>>, file_rel_path: S) -> Vec<u8> {
    let tmpdir_path = tmpd.as_ref().unwrap().as_ref().path();
    AtPath::new(tmpdir_path).read(file_rel_path)
}

pub fn repeat_str(s: &str, n: u32) -> String {
    let mut repeated = String::new();
    for _ in 0..n {
        repeated.push_str(s);
    }
    repeated
}

/// A command result is the outputs of a command (streams and status code)
/// within a struct which has convenience assertion functions about those outputs
pub struct CmdResult {
    //tmpd is used for convenience functions for asserts against fixtures
    tmpd: Option<Rc<TempDir>>,
    pub success: bool,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

impl CmdResult {
    /// asserts that the command resulted in a success (zero) status code
    pub fn success(&self) -> &CmdResult {
        assert!(self.success);
        self
    }

    /// asserts that the command resulted in a failure (non-zero) status code
    pub fn failure(&self) -> &CmdResult {
        assert!(!self.success);
        self
    }

    /// asserts that the command resulted in empty (zero-length) stderr stream output
    /// generally, it's better to use stdout_only() instead,
    /// but you might find yourself using this function if
    /// 1. you can not know exactly what stdout will be
    /// or 2. you know that stdout will also be empty
    pub fn no_stderr(&self) -> &CmdResult {
        assert!(self.stderr.is_empty());
        self
    }

    /// asserts that the command resulted in empty (zero-length) stderr stream output
    /// unless asserting there was neither stdout or stderr, stderr_only is usually a better choice
    /// generally, it's better to use stderr_only() instead,
    /// but you might find yourself using this function if
    /// 1. you can not know exactly what stderr will be
    /// or 2. you know that stderr will also be empty
    pub fn no_stdout(&self) -> &CmdResult {
        assert!(self.stdout.is_empty());
        self
    }

    /// asserts that the command resulted in stdout stream output that equals the
    /// passed in value
    /// stdout_only is a better choice unless stderr may or will be non-empty
    pub fn stdout_is<T: AsRef<[u8]>>(&self, msg: T) -> &CmdResult {
        assert_eq!(msg.as_ref(), &self.stdout[..]);
        self
    }

    /// like stdout_is(...), but expects the contents of the file at the provided relative path
    pub fn stdout_is_fixture<T: AsRef<OsStr>>(&self, file_rel_path: T) -> &CmdResult {
        let contents = read_scenario_fixture(&self.tmpd, file_rel_path);
        self.stdout_is(contents)
    }

    /// like stdout_is(...), but only checks if the stdout stream contains the given value
    pub fn stdout_contains<T: AsRef<str>>(&self, msg: T) -> &CmdResult {
        assert!(self.stdout_str().contains(msg.as_ref()));
        self
    }

    /// asserts that the command resulted in stderr stream output that equals the
    /// passed in value
    /// stderr_only is a better choice unless stdout may or will be non-empty
    pub fn stderr_is<T: AsRef<[u8]>>(&self, msg: T) -> &CmdResult {
        assert_eq!(msg.as_ref(), &self.stderr[..]);
        self
    }

    /// like stderr_is(...), but expects the contents of the file at the provided relative path
    pub fn stderr_is_fixture<T: AsRef<OsStr>>(&self, file_rel_path: T) -> &CmdResult {
        let contents = read_scenario_fixture(&self.tmpd, file_rel_path);
        self.stderr_is(contents)
    }

    /// like stderr_is(...), but only checks if the stderr stream contains the given value
    pub fn stderr_contains<T: AsRef<str>>(&self, msg: T) -> &CmdResult {
        assert!(self.stderr_str().contains(&msg.as_ref()));
        self
    }

    /// asserts that
    /// 1. the command resulted in stdout stream output that equals the
    /// passed in value
    /// and 2. the command resulted in empty (zero-length) stderr stream output
    pub fn stdout_only<T: AsRef<[u8]>>(&self, msg: T) -> &CmdResult {
        self.no_stderr().stdout_is(msg)
    }

    /// like stdout_only(...), but expects the contents of the file at the provided relative path
    pub fn stdout_only_fixture<T: AsRef<OsStr>>(&self, file_rel_path: T) -> &CmdResult {
        let contents = read_scenario_fixture(&self.tmpd, file_rel_path);
        self.stdout_only(contents)
    }

    /// asserts that
    /// 1. the command resulted in stderr stream output that equals the
    /// passed in value
    /// and 2. the command resulted in empty (zero-length) stdout stream output
    pub fn stderr_only<T: AsRef<[u8]>>(&self, msg: T) -> &CmdResult {
        self.no_stdout().stderr_is(msg)
    }

    /// like stderr_only(...), but expects the contents of the file at the provided relative path
    pub fn stderr_only_fixture<T: AsRef<OsStr>>(&self, file_rel_path: T) -> &CmdResult {
        let contents = read_scenario_fixture(&self.tmpd, file_rel_path);
        self.stderr_only(contents)
    }

    pub fn fails_silently(&self) -> &CmdResult {
        assert!(!self.success);
        assert!(self.stderr.is_empty());
        self
    }

    pub fn stdout_str(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.stdout)
    }

    pub fn stderr_str(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.stderr)
    }
}

pub fn log_info<T: AsRef<str>, U: AsRef<str>>(msg: T, par: U) {
    println!("{}: {}", msg.as_ref(), par.as_ref());
}

// XXX: there is a similar function in src/util.rs, maybe share common code?
pub fn recursive_copy(src: &Path, dest: &Path) -> Result<()> {
    if fs::metadata(src)?.is_dir() {
        for entry in fs::read_dir(src)? {
            let entry = entry?;
            let mut new_dest = PathBuf::from(dest);
            new_dest.push(entry.file_name());
            if fs::metadata(entry.path())?.is_dir() {
                fs::create_dir(&new_dest)?;
                recursive_copy(&entry.path(), &new_dest)?;
            } else {
                fs::copy(&entry.path(), new_dest)?;
            }
        }
    }
    Ok(())
}

pub fn get_root_path() -> &'static str {
    if cfg!(windows) {
        "C:\\"
    } else {
        "/"
    }
}

/// Object-oriented path struct that represents and operates on
/// paths relative to the directory it was constructed for.
#[derive(Clone)]
pub struct AtPath {
    pub subdir: PathBuf,
}

impl AtPath {
    pub fn new(subdir: &Path) -> AtPath {
        AtPath {
            subdir: PathBuf::from(subdir),
        }
    }

    pub fn to_string_lossy(&self) -> Cow<str> {
        self.subdir.to_string_lossy()
    }

    pub fn plus<S: AsRef<OsStr>>(&self, name: S) -> PathBuf {
        let mut pathbuf = self.subdir.clone();
        pathbuf.push(name.as_ref());
        pathbuf
    }

    pub fn plus_as_string<S: AsRef<OsStr>>(&self, name: S) -> String {
        self.plus(name).to_string_lossy().into_owned()
    }

    fn minus<S: AsRef<OsStr>>(&self, name: S) -> PathBuf {
        let prefixed = PathBuf::from(name.as_ref());
        if prefixed.starts_with(&self.subdir) {
            let mut unprefixed = PathBuf::new();
            for component in prefixed.components().skip(self.subdir.components().count()) {
                unprefixed.push(component.as_os_str().to_str().unwrap());
            }
            unprefixed
        } else {
            prefixed
        }
    }

    pub fn minus_as_string<S: AsRef<OsStr>>(&self, name: S) -> String {
        self.minus(name).to_string_lossy().into_owned()
    }

    pub fn open<S: AsRef<OsStr>>(&self, name: S) -> File {
        log_info("open", self.plus_as_string(&name));
        File::open(self.plus(name)).unwrap()
    }

    pub fn read_to_string<S: AsRef<OsStr>>(&self, name: S) -> String {
        let mut f = self.open(name);
        let mut contents = String::new();
        let _ = f.read_to_string(&mut contents);
        contents
    }

    pub fn read<S: AsRef<OsStr>>(&self, name: S) -> Vec<u8> {
        let mut f = self.open(name);
        let mut contents = vec![];
        let _ = f.read_to_end(&mut contents);
        contents
    }

    pub fn write<S: AsRef<OsStr>>(&self, name: S, contents: &[u8]) {
        let mut f = self.open(name);
        let _ = f.write_all(contents);
    }

    pub fn append<S: AsRef<OsStr>>(&self, name: S, contents: &[u8]) {
        log_info("open(append)", self.plus_as_string(&name));
        let mut f = OpenOptions::new()
            .write(true)
            .append(true)
            .open(self.plus(name))
            .unwrap();
        let _ = f.write_all(contents);
    }

    pub fn mkdir<S: AsRef<OsStr>>(&self, dir: S) {
        log_info("mkdir", self.plus_as_string(&dir));
        fs::create_dir(&self.plus(dir)).unwrap();
    }
    pub fn mkdir_all<S: AsRef<OsStr>>(&self, dir: S) {
        log_info("mkdir_all", self.plus_as_string(&dir));
        fs::create_dir_all(self.plus(dir)).unwrap();
    }

    pub fn make_file<S: AsRef<OsStr>>(&self, name: S) -> File {
        File::create(&self.plus(name)).unwrap()
    }

    pub fn touch<S: AsRef<OsStr>>(&self, file: S) {
        log_info("touch", self.plus_as_string(&file));
        File::create(&self.plus(file)).unwrap();
    }

    pub fn symlink<S, D>(&self, src: S, dst: D)
    where
        S: AsRef<OsStr>,
        D: AsRef<OsStr>,
    {
        log_info(
            "symlink",
            &format!(
                "{},{}",
                self.plus_as_string(&src),
                self.plus_as_string(&dst)
            ),
        );
        symlink_file(&self.plus(src), &self.plus(dst)).unwrap();
    }

    pub fn is_symlink<S: AsRef<OsStr>>(&self, path: S) -> bool {
        log_info("is_symlink", self.plus_as_string(&path));
        fs::symlink_metadata(&self.plus(path))
            .map(|m| m.file_type().is_symlink())
            .unwrap_or(false)
    }

    pub fn resolve_link<S: AsRef<OsStr>>(&self, path: S) -> PathBuf {
        log_info("resolve_link", self.plus_as_string(&path));
        fs::read_link(&self.plus(path))
            .map(|p| self.minus(p))
            .unwrap_or_default()
    }

    pub fn symlink_metadata<S: AsRef<OsStr>>(&self, path: S) -> fs::Metadata {
        fs::symlink_metadata(&self.plus(path)).unwrap()
    }

    pub fn metadata<S: AsRef<OsStr>>(&self, path: S) -> fs::Metadata {
        fs::metadata(&self.plus(path)).unwrap()
    }

    pub fn file_exists<S: AsRef<OsStr>>(&self, path: S) -> bool {
        fs::metadata(&self.plus(path))
            .map(|m| m.is_file())
            .unwrap_or(false)
    }

    pub fn dir_exists<S: AsRef<OsStr>>(&self, path: S) -> bool {
        fs::metadata(&self.plus(path))
            .map(|m| m.is_dir())
            .unwrap_or(false)
    }

    pub fn cleanup<S: AsRef<OsStr>>(&self, path: S) {
        let p = &self.plus(path);
        if let Ok(m) = fs::metadata(p) {
            if m.is_file() {
                fs::remove_file(&p).unwrap();
            } else {
                fs::remove_dir(&p).unwrap();
            }
        }
    }

    pub fn root_dir(&self) -> &Path {
        log_info("current_directory", "");
        &self.subdir
    }

    pub fn root_dir_resolved(&self) -> PathBuf {
        log_info("current_directory_resolved", "");
        let s = self.subdir.canonicalize().unwrap();

        // Due to canonicalize()'s use of GetFinalPathNameByHandleW() on Windows, the resolved path
        // starts with '\\?\' to extend the limit of a given path to 32,767 wide characters.
        //
        // To address this issue, we remove this prepended string if available.
        //
        // Source:
        // http://stackoverflow.com/questions/31439011/getfinalpathnamebyhandle-without-prepended
        let prefix = "\\\\?\\";
        if s.starts_with(prefix) {
            s.strip_prefix(prefix).unwrap().to_path_buf()
        } else {
            s
        }
    }
}

/// An environment for running a single uutils test case, serves three functions:
/// 1. centralizes logic for locating the uutils binary and calling the utility
/// 2. provides a temporary directory for the test case
/// 3. copies over fixtures for the utility to the temporary directory
pub struct TestScenario {
    util_name: String,
    pub fixtures: AtPath,
    tmpd: Rc<TempDir>,
}

impl TestScenario {
    pub fn new(util_name: &str) -> TestScenario {
        let tmpd = Rc::new(TempDir::new().unwrap());
        let ts = TestScenario {
            util_name: String::from(util_name),
            fixtures: AtPath::new(tmpd.as_ref().path()),
            tmpd: tmpd,
        };
        let mut fixture_path_builder = env::current_dir().unwrap();
        fixture_path_builder.push(TESTS_DIR);
        fixture_path_builder.push(FIXTURES_DIR);
        fixture_path_builder.push(util_name);
        if let Ok(m) = fs::metadata(&fixture_path_builder) {
            if m.is_dir() {
                recursive_copy(&fixture_path_builder, &ts.fixtures.subdir).unwrap();
            }
        }
        ts
    }

    pub fn ucmd(&self) -> UCommand {
        let mut cmd = self.cmd(Some("mesabox"));
        cmd.arg(&self.util_name);
        cmd
    }

    pub fn cmd<S: AsRef<OsStr>>(&self, bin: Option<S>) -> UCommand {
        UCommand::new_from_tmp(
            bin.as_ref()
                .map(|s| s.as_ref())
                .unwrap_or(self.util_name.as_ref()),
            self.tmpd.clone(),
            true,
        )
    }

    // different names are used rather than an argument
    // because the need to keep the environment is exceedingly rare.
    pub fn ucmd_keepenv(&self) -> UCommand {
        let mut cmd = self.cmd_keepenv(Some("mesabox"));
        cmd.arg(&self.util_name);
        cmd
    }

    pub fn cmd_keepenv<S: AsRef<OsStr>>(&self, bin: Option<S>) -> UCommand {
        UCommand::new_from_tmp(
            bin.as_ref()
                .map(|s| s.as_ref())
                .unwrap_or(self.util_name.as_ref()),
            self.tmpd.clone(),
            false,
        )
    }
}

/// A `UCommand` is a wrapper around an individual Command that provides several additional features
/// 1. it has convenience functions that are more ergonomic to use for piping in stdin, spawning the command
///       and asserting on the results.
/// 2. it tracks arguments provided so that in test cases which may provide variations of an arg in loops
///     the test failure can display the exact call which preceded an assertion failure.
/// 3. it provides convenience construction arguments to set the Command working directory and/or clear its environment.
pub struct UCommand {
    args: Vec<OsString>,
    env: Vec<(OsString, OsString)>,
    current_dir: PathBuf,
    comm_string: String,
    tmpd: Option<Rc<TempDir>>,
    has_run: bool,
    stdin: Option<Vec<u8>>,
}

impl UCommand {
    pub fn new<T: AsRef<OsStr>, U: AsRef<OsStr>>(arg: T, curdir: U, env_clear: bool) -> UCommand {
        let mut env = vec![];

        if !env_clear {
            env.extend(env::vars_os());
        }
        if cfg!(windows) {
            // %SYSTEMROOT% is required on Windows to initialize crypto provider
            // ... and crypto provider is required for std::rand
            // From procmon: RegQueryValue HKLM\SOFTWARE\Microsoft\Cryptography\Defaults\Provider\Microsoft Strong Cryptographic Provider\Image Path
            // SUCCESS  Type: REG_SZ, Length: 66, Data: %SystemRoot%\system32\rsaenh.dll"
            env.push(("SYSTEMROOT".into(), env::var_os("SYSTEMROOT").unwrap()));
        }

        let current_dir = PathBuf::from(curdir.as_ref());

        UCommand {
            tmpd: None,
            has_run: false,
            args: vec![arg.as_ref().to_owned()],
            env: env,
            current_dir: current_dir,

            comm_string: String::from(arg.as_ref().to_str().unwrap()),
            stdin: None,
        }
    }

    pub fn new_from_tmp<T: AsRef<OsStr>>(arg: T, tmpd: Rc<TempDir>, env_clear: bool) -> UCommand {
        let tmpd_path_buf = tmpd.as_ref().path().to_path_buf();
        let mut ucmd: UCommand = UCommand::new(arg.as_ref(), tmpd_path_buf, env_clear);
        ucmd.tmpd = Some(tmpd);
        ucmd
    }

    pub fn arg<S: Into<OsString>>(&mut self, arg: S) -> &mut UCommand {
        if self.has_run {
            panic!(ALREADY_RUN);
        }
        let arg = arg.into();

        self.comm_string.push_str(" ");
        // FIXME: should never use to_str().unwrap() for args
        self.comm_string.push_str(arg.to_str().unwrap());
        self.args.push(arg);
        self
    }

    /// like arg(...), but uses the contents of the file at the provided relative path as the argument
    pub fn arg_fixture<S: AsRef<OsStr>>(&mut self, file_rel_path: S) -> &mut UCommand {
        let contents = read_scenario_fixture(&self.tmpd, file_rel_path);
        self.arg(OsString::from_vec(contents))
    }

    pub fn args<S: AsRef<OsStr>>(&mut self, args: &[S]) -> &mut UCommand {
        if self.has_run {
            panic!(MULTIPLE_STDIN_MEANINGLESS);
        }
        for s in args {
            let s = s.as_ref();

            self.comm_string.push_str(" ");
            // FIXME: don't use to_str()
            self.comm_string.push_str(s.to_str().unwrap());
            self.args.push(s.to_owned());
        }

        self
    }

    /// provides stdinput to feed in to the command when spawned
    pub fn pipe_in<T: Into<Vec<u8>>>(&mut self, input: T) -> &mut UCommand {
        if self.stdin.is_some() {
            panic!(MULTIPLE_STDIN_MEANINGLESS);
        }
        self.stdin = Some(input.into());
        self
    }

    /// like pipe_in(...), but uses the contents of the file at the provided relative path as the piped in data
    pub fn pipe_in_fixture<S: AsRef<OsStr>>(&mut self, file_rel_path: S) -> &mut UCommand {
        let contents = read_scenario_fixture(&self.tmpd, file_rel_path);
        self.pipe_in(contents)
    }

    pub fn env<K, V>(&mut self, key: K, val: V) -> &mut UCommand
    where
        K: Into<OsString>,
        V: Into<OsString>,
    {
        if self.has_run {
            panic!(ALREADY_RUN);
        }
        self.env.push((key.into(), val.into()));
        self
    }

    /// Spawns the command, feeds the stdin if any, and returns the
    /// child process immediately.
    pub fn run_no_wait(&mut self) -> UChild {
        if self.has_run {
            panic!(ALREADY_RUN);
        }
        self.has_run = true;
        log_info("run", &self.comm_string);

        let stdin = self.stdin.clone().unwrap_or_default();
        let current_dir = Some(self.current_dir.clone());
        let env = Box::new(self.env.clone().into_iter());
        let args = self.args.clone().into_iter();

        let stdout = Arc::new(Mutex::new(vec![]));
        let stderr = Arc::new(Mutex::new(vec![]));

        let stdout_clone = stdout.clone();
        let stderr_clone = stderr.clone();
        UChild::new(
            thread::spawn(move || {
                let stdin = stdin;
                let mut args = args;
                let mut stdout = stdout_clone.lock().unwrap();
                let mut stderr = stderr_clone.lock().unwrap();
                let mut setup =
                    UtilSetup::new(&stdin[..], &mut *stdout, &mut *stderr, env, current_dir);
                mesabox::execute(&mut setup, &mut args)
            }),
            stdout,
            stderr,
        )
    }

    /// Spawns the command, feeds the stdin if any, waits for the result
    /// and returns a command result.
    /// It is recommended that you instead use succeeds() or fails()
    pub fn run(&mut self) -> CmdResult {
        let prog = self.run_no_wait().wait_with_output();

        CmdResult {
            tmpd: self.tmpd.clone(),
            success: prog.success,
            stdout: prog.stdout,
            stderr: prog.stderr,
        }
    }

    /// Spawns the command, feeding the passed in stdin, waits for the result
    /// and returns a command result.
    /// It is recommended that, instead of this, you use a combination of pipe_in()
    /// with succeeds() or fails()
    pub fn run_piped_stdin<T: Into<Vec<u8>>>(&mut self, input: T) -> CmdResult {
        self.pipe_in(input).run()
    }

    /// Spawns the command, feeds the stdin if any, waits for the result,
    /// asserts success, and returns a command result.
    pub fn succeeds(&mut self) -> CmdResult {
        let cmd_result = self.run();
        cmd_result.success();
        cmd_result
    }

    /// Spawns the command, feeds the stdin if any, waits for the result,
    /// asserts success, and returns a command result.
    pub fn fails(&mut self) -> CmdResult {
        let cmd_result = self.run();
        cmd_result.failure();
        cmd_result
    }

    pub fn command_str(&self) -> String {
        let mut res = self.args.iter().fold(String::new(), |mut acc, s| {
            acc.push_str(&s.to_string_lossy());
            acc.push(' ');
            acc
        });
        res.pop();
        res
    }
}

pub struct UChild {
    handle: JoinHandle<mesabox::Result<()>>,
    stdout: Arc<Mutex<Vec<u8>>>,
    stderr: Arc<Mutex<Vec<u8>>>,
}

impl UChild {
    pub fn new(
        handle: JoinHandle<mesabox::Result<()>>,
        stdout: Arc<Mutex<Vec<u8>>>,
        stderr: Arc<Mutex<Vec<u8>>>,
    ) -> Self {
        Self {
            handle: handle,
            stdout: stdout,
            stderr: stderr,
        }
    }

    pub fn wait_with_output(self) -> UOutput {
        let res = self.handle.join().unwrap();

        let stdout = Arc::try_unwrap(self.stdout).unwrap().into_inner().unwrap();
        let mut stderr = Arc::try_unwrap(self.stderr).unwrap().into_inner().unwrap();

        // FIXME: what to do with res?  print error to stderr first?  should this just be done in library?
        if let Err(ref f) = res {
            write!(stderr, "{}", f).unwrap();
        }

        UOutput {
            success: res.is_ok(),
            stdout: stdout,
            stderr: stderr,
        }
    }
}

pub struct UOutput {
    success: bool,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}

// TODO: figure out a good way to do this
/*
pub fn read_size_string(child: &mut UChild, size: usize) -> String {
    let output = read_size(child, size);
    String::from_utf8(output).unwrap()
}

pub fn read_size(child: &mut UChild, size: usize) -> Vec<u8> {
    let mut output = vec![0; size];

    // FIXME: if output is too slow this will cause issues
    sleep(Duration::from_secs(1));

    child
        .stdout
        .as_mut()
        .unwrap()
        .read_to_end(&mut output)
        .unwrap();
    
    output
}
*/
