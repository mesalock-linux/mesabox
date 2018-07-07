use either::Either;
use libc;
use glob::{self, MatchOptions};
use glob::Pattern as GlobPattern;
use walkdir::WalkDir;

use std::borrow::Cow;
use std::cell::{RefCell, Ref};
use std::ffi::{OsString, OsStr};
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::iter::FromIterator;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::RawFd;
use std::process::{Child, Stdio};
use std::rc::Rc;
use std::result::Result as StdResult;

use super::{NAME, UtilSetup, Result};
use super::command::{CommandEnv, CommandWrapper, ExecData, ExecEnv, InProcessCommand};
use super::env::{EnvFd, Environment};
use util;

pub type ExitCode = libc::c_int;

#[derive(Debug)]
pub struct Script {
    commands: Vec<CompleteCommand>
}

impl Script {
    pub fn new(commands: Vec<CompleteCommand>) -> Self {
        Self {
            commands: commands
        }
    }
}

#[derive(Debug)]
pub struct CompleteCommand {
    lists: Vec<Vec<Vec<AndOr>>>,
}

impl CompleteCommand {
    pub fn new(lists: Vec<Vec<Vec<AndOr>>>) -> Self {
        Self {
            lists: lists
        }
    }

    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment) -> ExitCode
    where
        S: UtilSetup,
    {
        // TODO: set envs and stuff
        let mut code = 0;
        for list in &self.lists {
            code = exec_list(setup, env, list);
        }
        code
    }
}

pub type VarName = OsString;

//pub type Word = OsString;
pub type Name = OsString;
pub type CommandName = Word;

// split into the two types to avoid allocating an extra vector for every word
// XXX: maybe store ParamExpand (at least) too?
// FIXME: needs to store more than just text (text works fine without globbing, but once you add that single quotes no longer
//        function the same as just straight up text (single quotes won't evaluate the glob, whereas the glob needs to be
//        evaluated if given like `echo *`))
//        according to the standard, quote removal is supposed to be last, so this is why
// Order of word expansion:
//    1. tilde expansion, parameter expansion, command substitution, arithmetic expansion
//    2. field splitting (i.e. IFS)
//    3. pathname expansion (i.e. globbing)
//    4. quote removal
#[derive(Debug)]
pub enum Word {
    Text(OsString),
    Complex(Vec<WordPart>),
}

impl Word {
    // XXX: this should likely be something like "eval_simple" and the eval_glob_fs method should
    //      be the standard version
    pub fn eval<S>(&self, setup: &mut S, env: &mut Environment) -> OsString
    where
        S: UtilSetup,
    {
        use self::Word::*;

        match self {
            Text(ref s) => s.clone(),
            Complex(ref parts) => {
                parts.iter().fold(OsString::new(), |mut acc, item| {
                    acc.push(&item.eval(setup, env));
                    acc
                })
            }
        }
    }

    pub fn matches_glob<'a, S>(&self, setup: &mut S, env: &mut Environment, value: &OsStr) -> bool
    where
        S: UtilSetup,
    {
        let text = self.eval(setup, env);

        // XXX: realized these likely are not needed as this method is used by Pattern (for case
        //      statements)
        let mut options = MatchOptions::new();
        options.require_literal_separator = true;
        options.require_literal_leading_dot = true;

        // FIXME: what to do here, error out?
        let matcher = match GlobPattern::new(&text) {
            Ok(m) => m,
            _ => return false
        };

        if matcher.matches_with(value, &options) {
            true
        } else {
            false
        }
    }

    fn eval_tilde<S>(&self, setup: &mut S, env: &mut Environment) -> OsString
    where
        S: UtilSetup,
    {
        use self::Word::*;

        let expand = |s: &OsStr, env: &mut Environment| {
            if s.as_bytes().starts_with(b"~/") {
                match env.get_var("HOME") {
                    Some(dir) if dir.len() > 0 => {
                        let mut result = dir.clone();
                        result.push("/");
                        result.push(OsStr::from_bytes(&s.as_bytes()[2..]));
                        result
                    }
                    _ => s.to_owned()
                }
            } else {
                s.to_owned()
            }
        };

        match self {
            Text(ref s) => expand(s, env),
            Complex(ref parts) => {
                let mut iter = parts.iter();

                if let Some(part) = iter.next() {
                    let start_val = match part {
                        WordPart::Text(ref s) => expand(s, env),
                        part => part.eval(setup, env),
                    };
                    iter.fold(start_val, |mut acc, item| {
                        acc.push(&item.eval(setup, env));
                        acc
                    })
                } else {
                    OsString::new()
                }
            }
        }
    }

    // NOTE: globset seems to implement filename{a,b} syntax, which is not valid for posix shell technically
    // NOTE: the output does not seem to be quite the same as dash (especially with stuff like src/**/*)
    // TODO: need to glob everything (including the command name, which we are not doing right now)
    pub fn eval_glob_fs<'a, S>(&self, setup: &mut S, env: &mut Environment) -> WordEval
    where
        S: UtilSetup,
    {
        let text = self.eval_tilde(setup, env);

        let mut options = MatchOptions::new();
        options.require_literal_separator = true;
        options.require_literal_leading_dot = true;

        match glob::glob_with(&text, &options) {
            Ok(paths) => {
                // FIXME: this should use the current_dir in setup (or whatever it has been changed
                //        to during the course of the program's lifetime)
                let mut res = paths.fold(vec![], |mut acc, entry| {
                    // FIXME: not sure what to do on entry failure (do we bail or just report an error?)
                    if let Ok(entry) = entry {
                        acc.push(entry.as_os_str().to_owned());
                    }
                    acc
                });

                if res.is_empty() {
                    WordEval::Text(text)
                } else {
                    WordEval::Globbed(res)
                }
            }
            Err(_) => {
                // in this case, we just assume that the "glob" is actual meant to be a literal
                WordEval::Text(text)
            }
        }
    }
}

#[derive(Debug)]
pub enum WordPart {
    Text(OsString),
    Param(ParamExpr),
    CommandSubst,       // TODO
}

impl WordPart {
    pub fn eval<S>(&self, setup: &mut S, env: &mut Environment) -> OsString
    where
        S: UtilSetup,
    {
        use self::WordPart::*;

        match self {
            Text(ref s) => s.clone(),
            Param(ref param) => param.eval(setup, env),
            _ => unimplemented!()
        }
    }

    pub fn is_text(&self) -> bool {
        match self {
            WordPart::Text(_) => true,
            _ => false
        }
    }
}

// FIXME: this is actually totally unnecessary (the result of the glob should be one big text string which gets
//        split according to IFS in the next step)
#[derive(Debug)]
pub enum WordEval {
    Text(OsString),
    Globbed(Vec<OsString>),
}

#[derive(Debug)]
pub struct AndOr {
    pipeline: Pipeline,
    separator: SepKind,
}

impl AndOr {
    pub fn new(pipeline: Pipeline, sep: SepKind) -> Self {
        Self {
            pipeline: pipeline,
            separator: sep,
        }
    }

    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment, prev_res: ExitCode) -> ExitCode
    where
        S: UtilSetup,
    {
        match (self.separator, prev_res) {
            (SepKind::First, _) | (SepKind::And, 0) => self.pipeline.execute(setup, env),
            (SepKind::Or, res) if res != 0 => self.pipeline.execute(setup, env),
            (_, prev_res) => prev_res
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum SepKind {
    First,
    And,
    Or,
}

#[derive(Debug)]
pub struct VarAssign {
    pub varname: VarName,
    pub value: Option<Word>,
}

impl VarAssign {
    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment)
    where
        S: UtilSetup,
    {
        let value = self.value.as_ref().map(|w| w.eval(setup, env)).unwrap_or_default();
        env.set_var(Cow::Borrowed(&self.varname), value);
    }

    pub fn eval<S>(&self, setup: &mut S, env: &mut Environment) -> (&OsStr, OsString)
    where
        S: UtilSetup,
    {
        (&self.varname, self.value.as_ref().map(|w| w.eval(setup, env)).unwrap_or_default())
    }
}

#[derive(Debug)]
pub struct Pipeline {
    commands: Vec<Command>,
    pub bang: bool,
}

impl Pipeline {
    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment) -> ExitCode
    where
        S: UtilSetup,
    {
        // FIXME: what this should actually do is set up commands where output of a commands is piped to the input of the next command and then start them all
        let mut code = 0;
        for cmd in &self.commands {
            code = cmd.execute(setup, env);
        }

        if self.bang {
            (code == 0) as ExitCode
        } else {
            code
        }
    }
}

impl FromIterator<Command> for Pipeline {
    fn from_iter<I: IntoIterator<Item = Command>>(iter: I) -> Self {
        Pipeline {
            commands: iter.into_iter().collect(),
            bang: false,
        }
    }
}

#[derive(Debug)]
pub struct Command {
    inner: CommandInner,
    pub redirect_list: Option<Vec<IoRedirect>>,
}

impl Command {
    pub fn with_inner(inner: CommandInner) -> Self {
        Self {
            inner: inner,
            redirect_list: None
        }
    }

    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment) -> ExitCode
    where
        S: UtilSetup,
    {
        // FIXME: what this should actually do is setup a command with stdin/stdout/stderr redirected to whatever is specified in redirect_list and return it
        self.inner.execute(setup, env)
    }
}

#[derive(Debug)]
pub enum CommandInner {
    If(Box<IfClause>),
    While(Box<WhileClause>),
    For(Box<ForClause>),
    Case(Box<CaseClause>),
    FunctionDef(Box<FunctionDef>),
    AndOr(Vec<Vec<AndOr>>),
    Simple(SimpleCommand),
}

impl CommandInner {
    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment) -> ExitCode
    where
        S: UtilSetup,
    {
        use self::CommandInner::*;

        // FIXME: needs to set up redirects somehow
        match self {
            If(ref clause) => clause.execute(setup, env),
            While(ref clause) => clause.execute(setup, env),
            For(ref clause) => clause.execute(setup, env),
            Case(ref clause) => clause.execute(setup, env),
            FunctionDef(ref def) => def.execute(setup, env),
            AndOr(ref and_ors) => exec_list(setup, env, and_ors),
            Simple(ref cmd) => cmd.execute(setup, env),
        }
    }
}

impl From<IfClause> for Command {
    fn from(value: IfClause) -> Self {
        Command::with_inner(CommandInner::If(Box::new(value)))
    }
}

impl From<WhileClause> for Command {
    fn from(value: WhileClause) -> Self {
        Command::with_inner(CommandInner::While(Box::new(value)))
    }
}

impl From<ForClause> for Command {
    fn from(value: ForClause) -> Self {
        Command::with_inner(CommandInner::For(Box::new(value)))
    }
}

impl From<CaseClause> for Command {
    fn from(value: CaseClause) -> Self {
        Command::with_inner(CommandInner::Case(Box::new(value)))
    }
}

impl From<FunctionDef> for Command {
    fn from(value: FunctionDef) -> Self {
        Command::with_inner(CommandInner::FunctionDef(Box::new(value)))
    }
}

impl From<SimpleCommand> for Command {
    fn from(value: SimpleCommand) -> Self {
        Command::with_inner(CommandInner::Simple(value))
    }
}

#[derive(Debug)]
pub struct IfClause {
    cond: Command,
    body: Command,
    else_stmt: Option<ElseClause>,
}

impl IfClause {
    pub fn new(cond: Command, body: Command, else_stmt: Option<ElseClause>) -> Self {
        Self {
            cond: cond,
            body: body,
            else_stmt: else_stmt,
        }
    }

    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment) -> ExitCode
    where
        S: UtilSetup,
    {
        // TODO: redirects
        if self.cond.execute(setup, env) == 0 {
            self.body.execute(setup, env)
        } else if let Some(ref clause) = self.else_stmt {
            clause.execute(setup, env)
        } else {
            0
        }
    }
}

#[derive(Debug)]
pub struct ElseClause {
    cond: Option<Command>,
    body: Command,
    else_stmt: Option<Box<ElseClause>>,
}

impl ElseClause {
    pub fn new(cond: Option<Command>, body: Command, else_stmt: Option<Box<Self>>) -> Self {
        Self {
            cond: cond,
            body: body,
            else_stmt: else_stmt,
        }
    }

    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment) -> ExitCode
    where
        S: UtilSetup,
    {
        // TOD: redirects
        if let Some(ref cmd) = self.cond {
            if cmd.execute(setup, env) != 0 {
                return match self.else_stmt {
                    Some(ref clause) => clause.execute(setup, env),
                    None => 0
                };
            }
        }
        self.body.execute(setup, env)
    }
}

#[derive(Debug)]
pub struct WhileClause {
    cond: Command,
    desired: bool,
    body: Command,
}

impl WhileClause {
    pub fn new(cond: Command, desired: bool, body: Command) -> Self {
        Self {
            cond: cond,
            desired: desired,
            body: body
        }
    }

    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment) -> ExitCode
    where
        S: UtilSetup,
    {
        // TODO: redirects
        let mut code = 0;
        while (self.cond.execute(setup, env) == 0) == self.desired {
            code = self.body.execute(setup, env);
        }
        code
    }
}

#[derive(Debug)]
pub struct ForClause {
    name: Name,
    words: Option<Vec<Word>>,
    body: Command,
}

impl ForClause {
    pub fn new(name: Name, words: Option<Vec<Word>>, body: Command) -> Self {
        Self {
            name: name,
            words: words,
            body: body
        }
    }

    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment) -> ExitCode
    where
        S: UtilSetup,
    {
        // TODO: redirects
        // TODO: when self.words is None it should act as if it were the value in $@ (retrieve from env)
        let words = match self.words {
            Some(ref words) => &words[..],
            None => unimplemented!()
        };

        let mut code = 0;
        for word in words {
            let value = word.eval(setup, env);
            env.set_var(Cow::Borrowed(&self.name), value);
            code = self.body.execute(setup, env);
        }
        code
    }
}

#[derive(Debug)]
pub struct CaseClause {
    word: Word,
    case_list: CaseList
}

impl CaseClause {
    pub fn new(word: Word, case_list: CaseList) -> Self {
        Self {
            word: word,
            case_list: case_list
        }
    }

    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment) -> ExitCode
    where
        S: UtilSetup,
    {
        // TODO: redirects
        self.case_list.execute(setup, env, &self.word)
    }
}

#[derive(Debug)]
pub struct CaseList {
    items: Vec<CaseItem>,
}

impl CaseList {
    pub fn new(items: Vec<CaseItem>) -> Self {
        Self {
            items: items
        }
    }

    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment, word: &Word) -> ExitCode
    where
        S: UtilSetup,
    {
        let word_str = word.eval(setup, env);
        for item in &self.items {
            if let Some(code) = item.execute(setup, env, &word_str) {
                return code;
            }
        }
        0
    }
}

#[derive(Debug)]
pub struct CaseItem {
    pattern: Pattern,
    actions: Option<Command>
}

impl CaseItem {
    pub fn new(pattern: Pattern, actions: Option<Command>) -> Self {
        Self {
            pattern: pattern,
            actions: actions
        }
    }

    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment, word: &OsStr) -> Option<ExitCode>
    where
        S: UtilSetup,
    {
        if self.pattern.matches(setup, env, word) {
            Some(if let Some(ref cmd) = self.actions {
                cmd.execute(setup, env)
            } else {
                0
            })
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Pattern {
    items: Vec<Word>,
}

impl Pattern {
    pub fn new(items: Vec<Word>) -> Self {
        Self {
            items: items
        }
    }

    pub fn matches<'a, S>(&self, setup: &mut S, env: &mut Environment, word: &OsStr) -> bool
    where
        S: UtilSetup,
    {
        for item in &self.items {
            if item.matches_glob(setup, env, word) {
                return true;
            }
        }
        false
    }
}

#[derive(Debug)]
pub struct FunctionDef {
    name: Name,
    body: Rc<FunctionBody>
}

impl FunctionDef {
    pub fn new(name: Name, body: Rc<FunctionBody>) -> Self {
        Self {
            name: name,
            body: body
        }
    }

    pub fn execute<S>(&self, _setup: &mut S, env: &mut Environment) -> ExitCode
    where
        S: UtilSetup,
    {
        env.set_func(&self.name, self.body.clone());
        // XXX: is there actually a way for this to not be 0?  spec says non-zero on failure, so is this just in parsing?
        //      from experimenting i think the only way for this to fail is if we try to define a
        //      function using a reserved name
        0
    }
}

#[derive(Debug)]
pub struct FunctionBody {
    command: Command,
    redirect_list: Option<Vec<IoRedirect>>,
}

impl FunctionBody {
    pub fn new(command: Command, redirect_list: Option<Vec<IoRedirect>>) -> Self {
        Self {
            command: command,
            redirect_list: redirect_list
        }
    }
}

impl InProcessCommand for FunctionBody {
    fn execute<S>(&self, setup: &mut S, env: &mut Environment, _data: ExecData) -> ExitCode
    where
        S: UtilSetup,
    {
        // TODO: set positional parameters using data.args
        // TODO: redirects
        //       the below should redirect whatever is written to whatever stdout is at the time of
        //       the function call to /dev/null (if a command inside the function redirects its
        //       stdout it goes wherever the command tells the output to go though):
        //
        //       test() {
        //           echo hi
        //       } >/dev/null
        //
        //       UNLIKE FUNCTIONS, SUBSHELLS WILL NEED A CLONED COPY OF THE ENVIRONMENT (which gets
        //       thrown away when the subshell finishes)
        self.command.execute(setup, env)
    }
}

#[derive(Debug)]
pub struct SimpleCommand {
    pre_actions: Option<Vec<PreAction>>,
    post_actions: Option<Vec<PostAction>>,
    name: Option<Word>,
}

impl SimpleCommand {
    pub fn new(name: Option<Word>, pre_actions: Option<Vec<PreAction>>, post_actions: Option<Vec<PostAction>>) -> Self {
        Self {
            name: name,
            pre_actions: pre_actions,
            post_actions: post_actions
        }
    }

    pub fn execute<S>(&self, setup: &mut S, env: &mut Environment) -> ExitCode
    where
        S: UtilSetup,
    {
        use std::process::Command as RealCommand;

        if let Some(ref name) = self.name {
            // FIXME: any extra things that were globbed should be passed as arguments (need way to
            //        extend post_actions)
            let cmdname = match name.eval_glob_fs(setup, env) {
                WordEval::Text(text) => text,
                WordEval::Globbed(files) => files.into_iter().next().unwrap(),
            };

            return {
                // NOTE: needed to make functions return Rcs rather than borrowed
                //       pointers for this to work (it is possible that an execution of a
                //       function could remove something that is being executed, which
                //       would then cause that function to be freed (if the borrow checker
                //       didn't catch it, that is))
                if let Some(builtin) = env.get_builtin(&cmdname) {
                    self.run_command(setup, env, ExecEnv::new(builtin))
                } else if let Some(func) = env.get_func(&cmdname) {
                    self.run_command(setup, env, ExecEnv::new(&*func))
                } else {
                    let cmd = CommandWrapper::new(RealCommand::new(cmdname));
                    self.run_command(setup, env, cmd)
                }
            };
        } else if let Some(ref actions) = self.pre_actions {
            for act in actions.iter() {
                // XXX: i believe we are just supposed to ignore redirects here, but not certain
                if let Either::Right(ref assign) = act {
                    assign.execute(setup, env);
                }
            }
        }

        0
    }

    fn run_command<S, E>(&self, setup: &mut S, env: &mut Environment, mut cmd: E) -> ExitCode
    where
        S: UtilSetup,
        E: CommandEnv,
    {
        env.enter_scope();

        cmd.envs(env.export_iter().map(|(k, v)| (Cow::Borrowed(k), Cow::Borrowed(v))));

        if let Some(ref actions) = self.pre_actions {
            for act in actions.iter() {
                match act {
                    Either::Left(ref redirect) => {
                        redirect.setup(setup, env, &mut cmd);
                    }
                    Either::Right(ref assign) => {
                        let (k, v) = assign.eval(setup, env);
                        cmd.env(Cow::Borrowed(k), Cow::Owned(v));
                    }
                }
            }
        }
        if let Some(ref actions) = self.post_actions {
            for act in actions.iter() {
                match act {
                    Either::Left(ref redirect) => {
                        redirect.setup(setup, env, &mut cmd);
                    }
                    Either::Right(ref word) => {
                        match word.eval_glob_fs(setup, env) {
                            WordEval::Text(text) => cmd.arg(Cow::Owned(text)),
                            WordEval::Globbed(glob) => cmd.args(glob.into_iter().map(|v| Cow::Owned(v))),
                        };
                    }
                }
            }
        }

        let res = cmd.status(setup, env).unwrap();

        env.exit_scope();

        res
    }
}

pub type PreAction = Either<IoRedirect, VarAssign>;
pub type PostAction = Either<IoRedirect, Word>;

#[derive(Debug)]
pub enum IoRedirect {
    File(Option<RawFd>, IoRedirectFile),
    Heredoc(Option<RawFd>, Rc<RefCell<HereDoc>>)
}

impl IoRedirect {
    // FIXME: this should be able to error
    pub fn setup<S, E>(&self, setup: &mut S, env: &mut Environment, cmd: &mut E)
    where
        S: UtilSetup,
        E: CommandEnv,
    {
        use self::IoRedirect::*;

        match self {
            File(fd, ref file) => {
                // FIXME: don't unwrap
                file.setup(setup, env, cmd, *fd).unwrap();
            }
            Heredoc(fd, ref doc) => {
                let fd = fd.unwrap_or(0);

                let heredoc = doc.borrow();
                let data = heredoc.data.clone();

                env.set_local_fd(fd as _, EnvFd::Piped(data));
            }
        }
    }
}

#[derive(Debug)]
pub struct IoRedirectFile {
    filename: Word,
    kind: IoRedirectKind
}

impl IoRedirectFile {
    pub fn new(kind: IoRedirectKind, filename: Word) -> Self {
        Self {
            kind: kind,
            filename: filename,
        }
    }

    pub fn setup<S, E>(&self, setup: &mut S, env: &mut Environment, cmd: &mut E, fd: Option<RawFd>) -> Result<()>
    where
        S: UtilSetup,
        E: CommandEnv,
    {
        use self::IoRedirectKind::*;

        let name = self.filename.eval(setup, env);

        match self.kind {
            Input => {
                let file = File::open(name)?;
                env.set_local_fd(fd.unwrap_or(0) as _, EnvFd::File(file))
            }
            Output => {
                // TODO: fail if noclobber option is set (by set -C), so if that option is present
                //       use OpenOptions with create_new() instead of File::create()
                let file = File::create(name)?;
                env.set_local_fd(fd.unwrap_or(1) as _, EnvFd::File(file))
            }
            Clobber => {
                let file = File::create(name)?;
                env.set_local_fd(fd.unwrap_or(1) as _, EnvFd::File(file))
            }
            ReadWrite => {
                let file = OpenOptions::new().create(true).read(true).write(true).open(name)?;

                env.set_local_fd(fd.unwrap_or(0) as _, EnvFd::File(file))
            }
            Append => {
                let file = OpenOptions::new().create(true).append(true).open(name)?;
                env.set_local_fd(fd.unwrap_or(1) as _, EnvFd::File(file))
            }
            DupInput if name.len() == 1 => {
                match name.to_string_lossy().chars().next().unwrap() {
                    '-' => {
                        // TODO: close file descriptor specified by fd (or 0 by default)
                        //env.get_fd()
                        unimplemented!()
                    }
                    ch @ '0'...'9' => {
                        // TODO: duplicate descriptor specified by name as that specified by fd (using dup2)
                        let digit = ch.to_digit(10).unwrap();
                        //cmd.fd_alias(fd.unwrap_or(0), digit as RawFd)
                        let fd = fd.unwrap_or(0) as _;
                        let value = env.get_fd(digit as _).current_val().try_clone()?;
                        env.set_local_fd(fd, value)
                    }
                    _ => {
                        util::string_to_err(Err("bad fd number".to_string()))?
                    }
                }
            }
            DupOutput if name.len() == 1 => {
                match name.to_string_lossy().chars().next().unwrap() {
                    '-' => {
                        // TODO: close file descriptor specified by fd (or 1 by default)
                        unimplemented!()
                    }
                    ch @ '0'...'9' => {
                        // TODO: duplicate descriptor specified by name as that specified by fd (using dup2)
                        let digit = ch.to_digit(10).unwrap();
                        //cmd.fd_alias(fd.unwrap_or(1), digit as RawFd)
                        let fd = fd.unwrap_or(1) as _;
                        let value = env.get_fd(digit as _).current_val().try_clone()?;
                        env.set_local_fd(fd, value)
                    }
                    _ => {
                        util::string_to_err(Err("bad fd number".to_string()))?
                    }
                }
            }
            _ => {
                // TODO: return error message stating invalid direction
                unimplemented!()
            }
        };

        Ok(())
    }
}

#[derive(Debug)]
pub enum IoRedirectKind {
    Input,
    DupInput,
    Output,
    DupOutput,
    ReadWrite,
    Append,
    Clobber
}

#[derive(Debug)]
pub struct HereDoc {
    pub data: Vec<u8>,
}

impl HereDoc {
    pub fn new(data: Vec<u8>) -> Self {
        Self {
            data: data
        }
    }
}

impl Default for HereDoc {
    fn default() -> Self {
        Self::new(vec![])
    }
}

// TODO: figure out how to get this to work (pass the child up the call chain?)
#[derive(Debug)]
pub struct CommandSubst {
    command: Command,
}

impl CommandSubst {
    pub fn new(cmd: Command) -> Self {
        Self {
            command: cmd,
        }
    }

    pub fn eval<S>(&self, setup: &mut S, env: &mut Environment) -> OsString
    where
        S: UtilSetup,
    {
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct DoubleQuote {
    items: Vec<Quotable>
}

impl DoubleQuote {
    pub fn eval<S>(&self, setup: &mut S, env: &mut Environment) -> OsString
    where
        S: UtilSetup,
    {
        self.items.iter().fold(OsString::new(), |mut acc, item| {
            acc.push(&item.eval(setup, env));
            acc
        })
    }
}

#[derive(Debug)]
pub enum Quotable {
    CommandSubst(CommandSubst),
    ArithExpr,      // TODO: actually add support for this
    ParamExpand,    // TODO: add support
    Text(OsString),
}

impl Quotable {
    pub fn eval<S>(&self, setup: &mut S, env: &mut Environment) -> OsString
    where
        S: UtilSetup,
    {
        use self::Quotable::*;

        match self {
            CommandSubst(ref sub) => sub.eval(setup, env),
            Text(ref s) => s.clone(),
            _ => unimplemented!()
        }
    }
}

#[derive(Debug)]
pub enum Param {
    Var(OsString),
    Star,
    Question,
    At
}

impl Param {
    pub fn eval<'a: 'b, 'b, S>(&self, setup: &mut S, env: &'a mut Environment) -> Option<&'b OsString>
    where
        S: UtilSetup,
    {
        use self::Param::*;

        match self {
            Var(ref s) => env.get_var(s),
            // TODO
            _ => unimplemented!()
        }
    }

    pub fn to_os_string(&self) -> OsString {
        use self::Param::*;

        match self {
            Var(ref s) => s.clone(),
            Star => OsString::from("*"),
            Question => OsString::from("?"),
            At => OsString::from("@"),
        }
    }
}

#[derive(Debug)]
pub struct ParamExpr {
    param: Param,
    kind: ParamExprKind,
}

#[derive(Debug)]
pub enum ParamExprKind {
    Assign(Word),
    AssignNull(Word),

    Use(Word),
    UseNull(Word),

    Error(Option<Word>),
    ErrorNull(Option<Word>),

    Alternate(Word),
    AlternateNull(Word),

    SmallPrefix(Word),
    LargePrefix(Word),

    SmallSuffix(Word),
    LargeSuffix(Word),

    Value,
    Length,
}

impl ParamExpr {
    pub fn new(name: Param, kind: ParamExprKind) -> Self {
        Self {
            param: name,
            kind: kind,
        }
    }

    pub fn eval<S>(&self, setup: &mut S, env: &mut Environment) -> OsString
    where
        S: UtilSetup,
    {
        use self::ParamExprKind::*;

        let pval_valid;
        let not_null;

        // NOTE: this entire setup is to get around the borrow checker
        loop {
            let pval = self.param.eval(setup, env);
            pval_valid = pval.is_some();
            not_null = pval.as_ref().map(|v| v.len() > 0).unwrap_or(false);
            return match self.kind {
                Assign(_) | Use(_) | Error(_) if not_null => pval.unwrap().clone(),
                AssignNull(_) | UseNull(_) | ErrorNull(_) if pval_valid => pval.unwrap().clone(),

                Length => pval.map(|v| OsString::from(v.len().to_string())).unwrap_or_else(|| OsString::from("0")),
                Value => pval.map(|v| v.clone()).unwrap_or_default(),

                _ => break,
            };
        }

        match self.kind {
            Alternate(ref word) if not_null => word.eval(setup, env),
            AlternateNull(ref word) if pval_valid => word.eval(setup, env),
            Alternate(_) | AlternateNull(_) => OsString::from(""),

            Assign(ref word) | AssignNull(ref word) => {
                let new_val = word.eval(setup, env);
                // TODO: figure out what to do when not Var (e.g. $*)
                if let Param::Var(ref name) = self.param {
                    env.set_var(Cow::Borrowed(name), new_val.clone());
                }
                new_val
            }

            Use(ref word) | UseNull(ref word) => {
                word.eval(setup, env)
            }

            Error(ref word) | ErrorNull(ref word) => {
                // TODO: figure out how to cleanly exit for scripts
                // FIXME: we should probably just return a Result (all the way up to
                //        CompleteCommand) and then print to stderr there
                let msg = word.as_ref().map(|w| w.eval(setup, env));
                display_err!(
                    setup.error(),
                    "{}: {}",
                    self.param.to_os_string().to_string_lossy(),
                    msg.as_ref().map(|m| m.to_string_lossy()).unwrap_or(Cow::from("parameter not set"))
                );
                // TODO: i assume this is where the result would go
                unimplemented!()
            }

            // TODO: prefixes and suffixes

            _ => unimplemented!()
        }
    }
}

fn exec_andor_chain<'a, S>(setup: &mut S, env: &mut Environment, chain: &'a [AndOr]) -> ExitCode
where
    S: UtilSetup,
{
    let mut code = 0;
    for and_or in chain {
        code = and_or.execute(setup, env, code);
    }
    code
}

fn exec_list<'a, S>(setup: &mut S, env: &mut Environment, list: &'a [Vec<AndOr>]) -> ExitCode
where
    S: UtilSetup,
{
    let mut code = 0;
    for chain in list {
        code = exec_andor_chain(setup, env, chain);
    }
    code
}
