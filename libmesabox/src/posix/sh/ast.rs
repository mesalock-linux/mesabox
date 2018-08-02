use failure::Fail;
use glob::Pattern as GlobPattern;
use glob::{self, MatchOptions};

use std::borrow::Cow;
use std::cell::RefCell;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::iter::FromIterator;
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::os::unix::io::RawFd;
use std::process;
use std::rc::Rc;
use std::result::Result as StdResult;

use super::command::{
    CommandEnv, CommandEnvContainer, CommandWrapper, ExecData, ExecEnv, InProcessChild,
    InProcessCommand, ShellChild,
};
use super::env::{CheckBreak, EnvFd, Environment};
use super::error::{CmdResult, CommandError, Result, ShellError};
use super::types::{Scoped, TryClone};
use super::{UtilSetup, NAME};
use util::{ExitCode, Pipe};

pub struct RuntimeData<'a: 'b, 'b, S: UtilSetup + 'a> {
    pub setup: &'a mut S,
    pub env: &'b mut Environment,
}

#[derive(Debug)]
pub struct Script {
    commands: Vec<CompleteCommand>,
}

impl Script {
    pub fn new(commands: Vec<CompleteCommand>) -> Self {
        Self { commands: commands }
    }
}

#[derive(Debug)]
pub struct CompleteCommand {
    lists: Vec<Vec<Vec<AndOr>>>,
}

impl CompleteCommand {
    pub fn new(lists: Vec<Vec<Vec<AndOr>>>) -> Self {
        Self { lists: lists }
    }

    pub fn execute<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> ExitCode
    where
        S: UtilSetup + 'a,
    {
        // TODO: set envs and stuff
        let mut code = 0;
        for list in &self.lists {
            code = exec_list(data, list);
        }
        code
    }
}

pub type VarName = OsString;

pub type Name = OsString;
pub type CommandName = Word;

// Order of word expansion:
//    1. tilde expansion (done), parameter expansion (done), command substitution (done), arithmetic expansion
//    2. field splitting (i.e. IFS) (TODO)
//    3. pathname expansion (i.e. globbing) (done)
//    4. quote removal (automatically handled?  at least should be when everything is set up correctly)
#[derive(Debug)]
pub enum Word {
    Parameter(ParamExpr),
    CommandSubst(CommandSubst),
    SingleQuote(OsString),
    DoubleQuote(DoubleQuote),
    Simple(OsString),
    Complex(Vec<Word>),
}

impl Word {
    // XXX: this should likely be something like "eval_simple" and the eval_glob_fs method should
    //      be the standard version
    // FIXME: like stated above, we need to retain the quote until the very end (maybe instead of
    //        returning OsString, return some enum { SingleQuote, DoubleQuote, Text }?)
    pub fn eval<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> OsString
    where
        S: UtilSetup + 'a,
    {
        use self::Word::*;

        match self {
            Parameter(ref param) => param.eval(data),
            CommandSubst(ref subst) => subst.eval(data),
            SingleQuote(ref quote) => quote.clone(),
            DoubleQuote(ref quote) => quote.eval(data),
            Simple(ref s) => s.clone(),
            Complex(ref parts) => parts.iter().fold(OsString::new(), |mut acc, item| {
                acc.push(&item.eval(data));
                acc
            }),
        }
    }

    pub fn matches_glob<'a: 'b, 'b, S>(
        &self,
        data: &mut RuntimeData<'a, 'b, S>,
        value: &OsStr,
    ) -> bool
    where
        S: UtilSetup + 'a,
    {
        let text = self.eval(data);

        // XXX: realized these likely are not needed as this method is used by Pattern (for case
        //      statements)
        let mut options = MatchOptions::new();
        options.require_literal_separator = true;
        options.require_literal_leading_dot = true;

        // FIXME: what to do here, error out?
        let matcher = match GlobPattern::new(&text) {
            Ok(m) => m,
            _ => return false,
        };

        if matcher.matches_with(value, &options) {
            true
        } else {
            false
        }
    }

    fn eval_tilde<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> OsString
    where
        S: UtilSetup + 'a,
    {
        use self::Word::*;

        let expand = |s: &OsStr, env: &mut Environment| {
            if s.as_bytes().starts_with(b"~/") {
                match env.get_var("HOME") {
                    Some(dir) if dir.len() > 0 => {
                        let mut result = dir.to_owned();
                        result.push("/");
                        result.push(OsStr::from_bytes(&s.as_bytes()[2..]));
                        result
                    }
                    _ => s.to_owned(),
                }
            } else {
                s.to_owned()
            }
        };

        match self {
            Simple(ref s) => expand(s, data.env),
            Complex(ref parts) => {
                let mut iter = parts.iter();

                if let Some(part) = iter.next() {
                    let start_val = match part {
                        Word::Simple(ref s) => expand(s, data.env),
                        part => part.eval(data),
                    };
                    iter.fold(start_val, |mut acc, item| {
                        acc.push(&item.eval(data));
                        acc
                    })
                } else {
                    OsString::new()
                }
            }
            other => other.eval(data),
        }
    }

    // NOTE: the output does not seem to be quite the same as dash (especially with stuff like src/**/*)
    pub fn eval_glob_fs<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> WordEval
    where
        S: UtilSetup + 'a,
    {
        use std::path::{Component, Path};

        let text = self.eval_tilde(data);

        let mut options = MatchOptions::new();
        options.require_literal_separator = true;
        options.require_literal_leading_dot = true;

        match glob::glob_with(&text, &options) {
            Ok(paths) => {
                // FIXME: this should use the current_dir in setup (or whatever it has been changed
                //        to during the course of the program's lifetime)

                // FIXME: this is a hack around an issue in glob where it removes
                //        Component::CurDir from globbed paths.  this fix only works if the given
                //        path is like ./filepath.  if the path is like path/./otherstuff the inner
                //        ./ will be removed
                let prefix = {
                    let mut components = Path::new(&text).components();

                    if let Some(Component::CurDir) = components.next() {
                        if components.next().is_some() {
                            "./"
                        } else {
                            ""
                        }
                    } else {
                        ""
                    }
                };
                let mut res = paths.fold(vec![], |mut acc, entry| {
                    // FIXME: not sure what to do on entry failure (do we bail or just report an error?)
                    if let Ok(entry) = entry {
                        let mut item = OsString::from(prefix);
                        item.push(entry.as_os_str());
                        acc.push(item);
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

    pub fn execute<'a: 'b, 'b, S>(
        &self,
        data: &mut RuntimeData<'a, 'b, S>,
        prev_res: ExitCode,
    ) -> ExitCode
    where
        S: UtilSetup + 'a,
    {
        let mut execute = || {
            let res = self.pipeline.execute(data);
            data.env.special_vars().set_last_exitcode(res);
            res
        };

        match (self.separator, prev_res) {
            (SepKind::First, _) | (SepKind::And, 0) => execute(),
            (SepKind::Or, res) if res != 0 => execute(),
            (_, prev_res) => prev_res,
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
    pub fn execute<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>)
    where
        S: UtilSetup + 'a,
    {
        let value = self.value
            .as_ref()
            .map(|w| w.eval(data))
            .unwrap_or_default();
        data.env.set_var(Cow::Borrowed(&self.varname), value);
    }

    pub fn eval<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> (&OsStr, OsString)
    where
        S: UtilSetup + 'a,
    {
        (
            &self.varname,
            self.value
                .as_ref()
                .map(|w| w.eval(data))
                .unwrap_or_default(),
        )
    }
}

#[derive(Debug)]
pub struct Pipeline {
    commands: Vec<Command>,
    pub bang: bool,
}

impl Pipeline {
    pub fn execute<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> ExitCode
    where
        S: UtilSetup + 'a,
    {
        assert!(self.commands.len() > 0);

        let last_cmd = self.commands.last().unwrap();

        let code = if self.commands.len() == 1 {
            last_cmd.execute(data)
        } else {
            // NOTE: according to POSIX, we can provide an extension where any (or all) of the
            //       commands executed here are executed in the current shell environment rather
            //       than in subshells
            let mut children = Vec::with_capacity(self.commands.len() - 1);

            for cmd in self.commands.iter().take(self.commands.len() - 1) {
                // XXX: if env adds enter_scope_fds() and enter_scope_vars(), this would be a
                //      scenario in which enter_scope() is still called
                data.env.enter_scope();
                let child = cmd.spawn(data, children.last_mut());
                data.env.exit_scope();
                if let Some(child) = child {
                    children.push(child);
                }
            }

            let code = fake_subshell(data, |data| {
                if let Some(child) = children.last_mut() {
                    data.env.set_fd(0, child.output());
                }
                last_cmd.execute(data)
            });

            // make sure all the children exit to avoid zombies
            // XXX: currently, this will cause the last command's error (if any) first and then the
            //      rest will be dumped in the order specified, not *when* the errors occur
            for mut child in children {
                if let Err(f) = child.wait() {
                    // TODO: print error
                }
            }

            code
        };

        let res = if self.bang {
            (code == 0) as ExitCode
        } else {
            code
        };

        data.env.special_vars().set_last_exitcode(res);

        res
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
    pub redirect_list: Vec<IoRedirect>,
}

impl Command {
    pub fn with_inner(inner: CommandInner) -> Self {
        Self {
            inner: inner,
            redirect_list: Vec::with_capacity(0),
        }
    }

    pub fn execute<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> ExitCode
    where
        S: UtilSetup + 'a,
    {
        // FIXME: what this should actually do is setup a command with stdin/stdout/stderr redirected to whatever is specified in redirect_list and return it
        match self.inner.execute(data) {
            Ok(code) => code,
            Err(f) => {
                // FIXME: needs to print out line number
                // XXX: should we ignore any I/O errors?
                let _ = display_msg!(data.setup.error(), "{}", f);
                127
            }
        }
    }

    pub fn spawn<'a: 'b, 'b, S>(
        &self,
        data: &mut RuntimeData<'a, 'b, S>,
        prev_child: Option<&mut ShellChild>,
    ) -> Option<ShellChild>
    where
        S: UtilSetup + 'a,
    {
        match self.inner.spawn(data, prev_child) {
            Ok(child) => Some(child),
            Err(f) => {
                // TODO: print error stuff
                None
            }
        }
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
    SubShell(Vec<Vec<AndOr>>),
    Simple(SimpleCommand),
}

impl CommandInner {
    pub fn execute<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> Result<ExitCode>
    where
        S: UtilSetup + 'a,
    {
        use self::CommandInner::*;

        // FIXME: needs to set up redirects somehow
        match self {
            If(ref clause) => Ok(clause.execute(data)),
            While(ref clause) => Ok(clause.execute(data)),
            For(ref clause) => Ok(clause.execute(data)),
            Case(ref clause) => Ok(clause.execute(data)),
            FunctionDef(ref def) => Ok(def.execute(data)),
            AndOr(ref and_ors) => Ok(exec_list(data, and_ors)),
            SubShell(ref and_ors) => {
                // XXX: this would be a use case for variable locality, but for now fork
                InProcessChild::spawn(data, |data| {
                    Ok(exec_list(data, and_ors))
                })
                    .map_err(|e| ShellError::Spawn(e))
                    .and_then(|mut child| child.wait().map_err(|e| ShellError::SubShell(e)))
            }
            Simple(ref cmd) => cmd.execute(data),
        }
    }

    pub fn spawn<'a: 'b, 'b, S>(
        &self,
        data: &mut RuntimeData<'a, 'b, S>,
        prev_child: Option<&mut ShellChild>,
    ) -> Result<ShellChild>
    where
        S: UtilSetup + 'a,
    {
        use self::CommandInner::*;

        // FIXME: see above
        match self {
            SubShell(ref and_ors) => {
                // XXX: maybe make a Block node to share code between SubShell, AndOr (in Command, so compound_list) and FunctionBody?
                // FIXME: should this use fake_subshell() or is this unnecessary?
                data.env.enter_scope();

                data.env.set_fd(1, EnvFd::Pipeline);
                let inproc_child = InProcessChild::spawn(data, |data| {
                    if let Some(prev) = prev_child {
                        data.env.set_fd(0, prev.output());
                    }
                    Ok(exec_list(data, and_ors))
                })
                    .map(|child| ShellChild::InProcess(child))
                    .map_err(|e| ShellError::Spawn(e));

                // XXX: fake_subshell????
                data.env.exit_scope();

                inproc_child
            }
            Simple(ref cmd) => cmd.spawn(data, prev_child),
            _ => InProcessChild::spawn(data, |data| {
                self.execute(data)
                    .map_err(|e| CommandError::Shell(Box::new(e.compat())))
            }).map(|child| ShellChild::InProcess(child))
                .map_err(|e| ShellError::Spawn(e)),
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

    pub fn execute<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> ExitCode
    where
        S: UtilSetup + 'a,
    {
        // TODO: redirects
        if self.cond.execute(data) == 0 {
            if check_break(data) {
                0
            } else {
                self.body.execute(data)
            }
        } else if let Some(ref clause) = self.else_stmt {
            clause.execute(data)
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

    pub fn execute<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> ExitCode
    where
        S: UtilSetup + 'a,
    {
        // TOD: redirects
        if let Some(ref cmd) = self.cond {
            if cmd.execute(data) != 0 {
                return match self.else_stmt {
                    Some(ref clause) => clause.execute(data),
                    None => 0,
                };
            } else if check_break(data) {
                return 0;
            }
        }
        self.body.execute(data)
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
            body: body,
        }
    }

    pub fn execute<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> ExitCode
    where
        S: UtilSetup + 'a,
    {
        // TODO: redirects
        data.env.inc_loop_depth();

        let mut code = 0;
        loop {
            // we can't just use a while loop as continue/break work in the condition as well
            let condres = self.cond.execute(data) == 0;
            if condres {
                match check_break_loop(data) {
                    CheckBreak::Continue => continue,
                    CheckBreak::Break => break,
                    _ => {}
                }
            }

            // check the actual condition (i.e. if the command was 0 for while or !0 for until)
            if condres == self.desired {
                code = self.body.execute(data);

                if check_break_loop(data) == CheckBreak::Break {
                    break;
                }
            } else {
                break;
            }
        }

        data.env.dec_loop_depth();

        code
    }
}

#[derive(Debug)]
pub struct ForClause {
    name: Name,
    words: Vec<Word>,
    body: Command,
}

impl ForClause {
    pub fn new(name: Name, words: Vec<Word>, body: Command) -> Self {
        Self {
            name: name,
            words: words,
            body: body,
        }
    }

    pub fn execute<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> ExitCode
    where
        S: UtilSetup + 'a,
    {
        // TODO: redirects
        data.env.inc_loop_depth();

        // TODO: when self.words is empty it should act as if it were the value in $@ (retrieve from env)
        let words = if self.words.is_empty() {
            unimplemented!()
        } else {
            &self.words[..]
        };

        let mut code = 0;
        for word in words {
            let value = word.eval(data);
            data.env.set_var(Cow::Borrowed(&self.name), value);
            code = self.body.execute(data);

            if check_break_loop(data) == CheckBreak::Break {
                break;
            }
        }

        data.env.dec_loop_depth();

        code
    }
}

#[derive(Debug)]
pub struct CaseClause {
    word: Word,
    case_list: CaseList,
}

impl CaseClause {
    pub fn new(word: Word, case_list: CaseList) -> Self {
        Self {
            word: word,
            case_list: case_list,
        }
    }

    pub fn execute<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> ExitCode
    where
        S: UtilSetup + 'a,
    {
        // TODO: redirects
        self.case_list.execute(data, &self.word)
    }
}

#[derive(Debug)]
pub struct CaseList {
    items: Vec<CaseItem>,
}

impl CaseList {
    pub fn new(items: Vec<CaseItem>) -> Self {
        Self { items: items }
    }

    pub fn execute<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>, word: &Word) -> ExitCode
    where
        S: UtilSetup + 'a,
    {
        let word_str = word.eval(data);
        for item in &self.items {
            if let Some(code) = item.execute(data, &word_str) {
                return code;
            }
        }
        0
    }
}

#[derive(Debug)]
pub struct CaseItem {
    pattern: Pattern,
    actions: Option<Command>,
}

impl CaseItem {
    pub fn new(pattern: Pattern, actions: Option<Command>) -> Self {
        Self {
            pattern: pattern,
            actions: actions,
        }
    }

    pub fn execute<'a: 'b, 'b, S>(
        &self,
        data: &mut RuntimeData<'a, 'b, S>,
        word: &OsStr,
    ) -> Option<ExitCode>
    where
        S: UtilSetup + 'a,
    {
        if self.pattern.matches(data, word) {
            Some(if let Some(ref cmd) = self.actions {
                cmd.execute(data)
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
        Self { items: items }
    }

    pub fn matches<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>, word: &OsStr) -> bool
    where
        S: UtilSetup + 'a,
    {
        for item in &self.items {
            if item.matches_glob(data, word) {
                return true;
            }
        }
        false
    }
}

#[derive(Debug)]
pub struct FunctionDef {
    name: Name,
    body: Rc<FunctionBody>,
}

impl FunctionDef {
    pub fn new(name: Name, body: Rc<FunctionBody>) -> Self {
        Self {
            name: name,
            body: body,
        }
    }

    pub fn execute<'a, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> ExitCode
    where
        S: UtilSetup,
    {
        data.env.set_func(&self.name, self.body.clone());
        // XXX: is there actually a way for this to not be 0?  spec says non-zero on failure, so is this just in parsing?
        //      from experimenting i think the only way for this to fail is if we try to define a
        //      function using a reserved name
        0
    }
}

#[derive(Debug)]
pub struct FunctionBody {
    command: Command,
    redirect_list: Vec<IoRedirect>,
}

impl FunctionBody {
    pub fn new(command: Command, redirect_list: Vec<IoRedirect>) -> Self {
        Self {
            command: command,
            redirect_list: redirect_list,
        }
    }
}

impl InProcessCommand for FunctionBody {
    fn execute<'a: 'b, 'b, S>(
        &self,
        rt_data: &mut RuntimeData<'a, 'b, S>,
        data: ExecData,
    ) -> CmdResult<ExitCode>
    where
        S: UtilSetup + 'a,
    {
        rt_data.env.special_vars().set_positionals(data.args);

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
        Ok(self.command.execute(rt_data))
    }

    fn spawn<'a: 'b, 'b, S>(
        &self,
        rt_data: &mut RuntimeData<'a, 'b, S>,
        data: ExecData,
    ) -> CmdResult<ShellChild>
    where
        S: UtilSetup + 'a,
    {
        let child = InProcessChild::spawn(rt_data, |rt_data| self.execute(rt_data, data))?;
        Ok(ShellChild::InProcess(child))
    }
}

#[derive(Debug)]
pub struct SimpleCommand {
    pre_actions: Vec<PreAction>,
    post_actions: Vec<PostAction>,
    name: Option<Word>,
}

impl SimpleCommand {
    pub fn new(
        name: Option<Word>,
        pre_actions: Vec<PreAction>,
        post_actions: Vec<PostAction>,
    ) -> Self {
        Self {
            name: name,
            pre_actions: pre_actions,
            post_actions: post_actions,
        }
    }

    pub fn execute<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> Result<ExitCode>
    where
        S: UtilSetup + 'a,
    {
        self.perform_action(data, |_env| Ok(()), move |cmd, data| cmd.status(data))
            .map(|res| if let Some(code) = res { code } else { 0 })
    }

    pub fn spawn<'a: 'b, 'b, S>(
        &self,
        data: &mut RuntimeData<'a, 'b, S>,
        prev_child: Option<&mut ShellChild>,
    ) -> Result<ShellChild>
    where
        S: UtilSetup + 'a,
    {
        self.perform_action(
            data,
            |env| {
                if let Some(child) = prev_child {
                    // FIXME: should probably be passing Option<Option<&mut ShellChild>>, so we can set stdin to EnvFd::Null if
                    //        the previous command failed or something
                    env.set_fd(0, child.output());
                }
                env.set_fd(1, EnvFd::Pipeline);
                Ok(())
            },
            move |cmd, data| cmd.spawn(data),
        ).map(|res| {
            if let Some(child) = res {
                child
            } else {
                ShellChild::Empty
            }
        })
    }

    fn perform_action<'a: 'b, 'b, S, I, F, R>(
        &self,
        data: &mut RuntimeData<'a, 'b, S>,
        init_fn: I,
        func: F,
    ) -> Result<Option<R>>
    where
        S: UtilSetup + 'a,
        I: FnOnce(&mut Environment) -> CmdResult<()>,
        F: FnOnce(CommandEnvContainer, &mut RuntimeData<'a, 'b, S>) -> CmdResult<R>,
    {
        use std::process::Command as RealCommand;

        if let Some(ref name) = self.name {
            // FIXME: any extra things that were globbed should be passed as arguments (need way to
            //        extend post_actions)
            let cmdname = match name.eval_glob_fs(data) {
                WordEval::Text(text) => text,
                WordEval::Globbed(files) => files.into_iter().next().unwrap(),
            };

            // store the newly created fds so we don't accidentally destroy them if the user does
            // something like 1>&2 2>&1 1>&2
            let mut new_fds = Vec::with_capacity(0);

            // NOTE: if child.output() were to return something like EnvFd::Pipe rather than
            //       EnvFd::Fd, we'd need to do two enter_scope() here instead of one to ensure
            //       any and all EnvFds created by init_fn() remain valid
            data.env.enter_scope();
            init_fn(data.env).map_err(|e| ShellError::Command {
                cmdname: cmdname.to_string_lossy().into_owned(),
                err: e,
            })?;
            let (cmdenv, res) = {
                // NOTE: needed to make functions return Rcs rather than borrowed
                //       pointers for this to work (it is possible that an execution of a
                //       function could remove something that is being executed, which
                //       would then cause that function to be freed (if the borrow checker
                //       didn't catch it, that is))
                if let Some(builtin) = data.env.get_builtin(&cmdname) {
                    let mut cmd = ExecEnv::new(builtin);
                    let res = self.setup_command(data, &mut cmd, &mut new_fds);
                    (CommandEnvContainer::Builtin(cmd), res)
                } else if let Some(func) = data.env.get_func(&cmdname) {
                    let mut cmd = ExecEnv::new(func);
                    let res = self.setup_command(data, &mut cmd, &mut new_fds);
                    (CommandEnvContainer::Function(cmd), res)
                } else {
                    let mut cmd = CommandWrapper::new(RealCommand::new(&cmdname));
                    let res = self.setup_command(data, &mut cmd, &mut new_fds);
                    (CommandEnvContainer::RealCommand(cmd), res)
                }
            };
            if let Err(f) = res {
                // make sure the scope is destroyed on error
                data.env.exit_scope();
                return Err(ShellError::Command {
                    cmdname: cmdname.to_string_lossy().into_owned(),
                    err: f,
                });
            }

            let res = func(cmdenv, data).map_err(|e| ShellError::Command {
                cmdname: cmdname.to_string_lossy().into_owned(),
                err: e,
            });
            data.env.exit_scope();
            return res.map(|v| Some(v));
        } else {
            for act in self.pre_actions.iter() {
                // XXX: i believe we are just supposed to ignore redirects here, but not certain
                //      (this is not correct as the user could be trying to close file descriptors)
                if let PreAction::VarAssign(ref assign) = act {
                    assign.execute(data);
                }
            }
        }

        Ok(None)
    }

    fn setup_command<'a: 'b, 'b, S, E>(
        &self,
        data: &mut RuntimeData<'a, 'b, S>,
        cmd: &mut E,
        new_fds: &mut Vec<EnvFd>,
    ) -> CmdResult<()>
    where
        S: UtilSetup + 'a,
        E: CommandEnv,
    {
        cmd.envs(
            data.env
                .export_iter()
                .map(|(k, v)| (Cow::Borrowed(k), Cow::Borrowed(v))),
        );

        for act in self.pre_actions.iter() {
            match act {
                PreAction::IoRedirect(ref redirect) => {
                    redirect.setup(data, new_fds)?;
                }
                PreAction::VarAssign(ref assign) => {
                    let (k, v) = assign.eval(data);
                    cmd.env(Cow::Borrowed(k), Cow::Owned(v));
                }
            }
        }
        for act in self.post_actions.iter() {
            match act {
                PostAction::IoRedirect(ref redirect) => {
                    redirect.setup(data, new_fds)?;
                }
                PostAction::Word(ref word) => {
                    match word.eval_glob_fs(data) {
                        WordEval::Text(text) => cmd.arg(Cow::Owned(text)),
                        WordEval::Globbed(glob) => {
                            cmd.args(glob.into_iter().map(|v| Cow::Owned(v)))
                        }
                    };
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum PreAction {
    IoRedirect(IoRedirect),
    VarAssign(VarAssign),
}

#[derive(Debug)]
pub enum PostAction {
    IoRedirect(IoRedirect),
    Word(Word),
}

#[derive(Debug)]
pub enum IoRedirect {
    File(Option<RawFd>, IoRedirectFile),
    Heredoc(Option<RawFd>, Rc<RefCell<HereDoc>>),
}

impl IoRedirect {
    pub fn setup<'a: 'b, 'b, S>(
        &self,
        data: &mut RuntimeData<'a, 'b, S>,
        new_fds: &mut Vec<EnvFd>,
    ) -> CmdResult<()>
    where
        S: UtilSetup + 'a,
    {
        use self::IoRedirect::*;

        match self {
            File(fd, ref file) => {
                file.setup(data, *fd, new_fds)?;
            }
            Heredoc(fd, ref doc) => {
                let fd = fd.unwrap_or(0);

                let heredoc = doc.borrow();
                let heredoc_data = heredoc.data.clone();

                data.env.set_fd(fd as _, EnvFd::Piped(heredoc_data));
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct IoRedirectFile {
    filename: Word,
    kind: IoRedirectKind,
}

impl IoRedirectFile {
    pub fn new(kind: IoRedirectKind, filename: Word) -> Self {
        Self {
            kind: kind,
            filename: filename,
        }
    }

    // FIXME: this doesn't work correctly as if the user does something like 2>&1 1>&4,
    //        stderr (fd 2) will be set to whatever stdout (fd 1) is and then stdout will be set to
    //        whatever fd 4 is, but stderr will still be whatever value stdout was (unless this is
    //        the correct behavior, need to verify)
    pub fn setup<'a: 'b, 'b, S>(
        &self,
        data: &mut RuntimeData<'a, 'b, S>,
        fd: Option<RawFd>,
        new_fds: &mut Vec<EnvFd>,
    ) -> CmdResult<()>
    where
        S: UtilSetup + 'a,
    {
        use self::IoRedirectKind::*;
        use std::io;

        let name = self.filename.eval(data);

        let file_err = |fd: RawFd, filename: OsString, err: io::Error| CommandError::FdAsFile {
            fd: fd,
            filename: filename.to_string_lossy().into_owned(),
            err: err,
        };

        let mut create_file_fd = |data: &mut RuntimeData<'a, 'b, S>, fd, file| {
            let new_fd = EnvFd::File(file);
            // FIXME: this really shouldn't be at risk of failing as we can directly create the
            //        RawObjectWrapper (we know the read/write status)
            data.env.set_fd(fd as _, new_fd.try_clone()?);
            new_fds.push(new_fd);
            Ok(())
        };

        match self.kind {
            Input => {
                let fd = fd.unwrap_or(0);
                let file = File::open(&name).map_err(|e| file_err(fd, name, e))?;

                create_file_fd(data, fd, file)?;
            }
            Output => {
                // TODO: fail if noclobber option is set (by set -C), so if that option is present
                //       use OpenOptions with create_new() instead of File::create()
                let fd = fd.unwrap_or(1);
                let file = File::create(&name).map_err(|e| file_err(fd, name, e))?;

                create_file_fd(data, fd, file)?;
            }
            Clobber => {
                let fd = fd.unwrap_or(1);
                let file = File::create(&name).map_err(|e| file_err(fd, name, e))?;

                create_file_fd(data, fd, file)?;
            }
            ReadWrite => {
                let fd = fd.unwrap_or(0);
                let file = OpenOptions::new()
                    .create(true)
                    .read(true)
                    .write(true)
                    .open(&name)
                    .map_err(|e| file_err(fd, name, e))?;

                create_file_fd(data, fd, file)?;
            }
            Append => {
                let fd = fd.unwrap_or(1);
                let file = OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(&name)
                    .map_err(|e| file_err(fd, name, e))?;

                create_file_fd(data, fd, file)?;
            }
            DupInput if name.len() == 1 => {
                match name.as_bytes()[0] {
                    b'-' => {
                        // TODO: close file descriptor specified by fd (or 0 by default)
                        // TODO: anything that closes file descriptors will need to check if the
                        //       file descriptor is still being used in another scope (i.e. it will
                        //       need to check if the count for Locality::Local is > 0)
                        //env.get_fd()
                        unimplemented!()
                    }
                    ch @ b'0'...b'9' => {
                        // TODO: duplicate descriptor specified by name as that specified by fd (using dup2)
                        let digit = (ch as char).to_digit(10).unwrap();
                        //cmd.fd_alias(fd.unwrap_or(0), digit as RawFd)
                        let fd = fd.unwrap_or(0) as _;
                        let value = data.env.get_fd(digit as _).try_clone()?;
                        data.env.set_fd(fd, value)
                    }
                    ch => Err(CommandError::InvalidFd(ch))?,
                }
            }
            DupOutput if name.len() == 1 => {
                match name.as_bytes()[0] {
                    b'-' => {
                        // TODO: close file descriptor specified by fd (or 1 by default)
                        unimplemented!()
                    }
                    ch @ b'0'...b'9' => {
                        // TODO: duplicate descriptor specified by name as that specified by fd (using dup2)
                        // unwrap here is fine as we verified that ch is valid above
                        let digit = (ch as char).to_digit(10).unwrap();
                        let fd = fd.unwrap_or(1) as _;
                        let value = data.env.get_fd(digit as _).try_clone()?;
                        data.env.set_fd(fd, value)
                    }
                    ch => Err(CommandError::InvalidFd(ch))?,
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

#[derive(Clone, Copy, Debug)]
pub enum IoRedirectKind {
    Input,
    DupInput,
    Output,
    DupOutput,
    ReadWrite,
    Append,
    Clobber,
}

#[derive(Debug)]
pub struct HereDoc {
    pub data: Vec<u8>,
}

impl HereDoc {
    pub fn new(data: Vec<u8>) -> Self {
        Self { data: data }
    }
}

impl Default for HereDoc {
    fn default() -> Self {
        Self::new(vec![])
    }
}

#[derive(Debug)]
pub struct CommandSubst {
    command: Box<Command>,
}

impl CommandSubst {
    pub fn new(cmd: Box<Command>) -> Self {
        Self { command: cmd }
    }

    pub fn eval<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> OsString
    where
        S: UtilSetup + 'a,
    {
        // set stdout for the command to an anonymous pipe so we can retrieve the output and return
        // it later (NOTE: we might want to just do this in general if an output EnvFd is
        // EnvFd::Piped, in which case we would just set this EnvFd to EnvFd::Piped instead of
        // manually creating a pipe here)
        let (mut read, write) = match self.write_error(data.setup, data.env, Pipe::create()) {
            Ok(m) => m,
            Err(f) => return f,
        };

        let code = fake_subshell(data, |data| {
            data.env.set_fd(1, EnvFd::Pipe(write));
            self.command.execute(data)
        });
        data.env.special_vars().set_last_exitcode(code);

        // read the output from the pipe into a vector
        let mut output = vec![];
        let res = self.write_error(data.setup, data.env, read.read_to_end(&mut output));

        if let Err(f) = res {
            return f;
        }

        let size = {
            let mut iter = output.rsplitn(2, |&byte| byte != b'\n');
            let last = iter.next().unwrap();
            if iter.next().is_some() {
                output.len() - last.len()
            } else {
                output.len()
            }
        };
        output.truncate(size);
        OsString::from_vec(output)
    }

    fn write_error<S, T, U>(
        &self,
        setup: &mut S,
        env: &mut Environment,
        res: StdResult<T, U>,
    ) -> StdResult<T, OsString>
    where
        S: UtilSetup,
        U: fmt::Display,
    {
        write_error(setup, res).map_err(|code| {
            env.special_vars().set_last_exitcode(code);
            OsString::new()
        })
    }
}

#[derive(Debug)]
pub struct DoubleQuote {
    items: Vec<Word>,
}

impl DoubleQuote {
    pub fn new(items: Vec<Word>) -> Self {
        Self { items: items }
    }

    pub fn eval<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> OsString
    where
        S: UtilSetup + 'a,
    {
        self.items.iter().fold(OsString::new(), |mut acc, item| {
            acc.push(&item.eval(data));
            acc
        })
    }
}

#[derive(Debug)]
pub enum Param {
    Var(OsString),
    Star,
    Question,
    At,
    NumParams,
    ShellPid,
    BackgroundPid,
    Positional(usize),
}

impl Param {
    pub fn eval<'a: 'b, 'b>(&self, env: &'a mut Environment) -> Option<Cow<'b, OsStr>> {
        use self::Param::*;

        match self {
            Var(ref s) => env.get_var(s).map(|v| Cow::Borrowed(v.as_os_str())),
            Question => Some(Cow::Owned(OsString::from(format!(
                "{}",
                env.special_vars().get_last_exitcode()
            )))),
            // TODO: expand/split according to IFS (this may need to return a word)
            Star => {
                // FIXME: this is wrong (shouldn't be combining into one field here)
                let mut res = env.special_vars().get_positionals().iter().fold(
                    OsString::new(),
                    |mut acc, item| {
                        acc.push(item);
                        acc.push(" ");
                        acc
                    },
                );
                Some(Cow::Owned(res))
            }
            At => {
                // FIXME: same issue as above
                let mut res = env.special_vars().get_positionals().iter().fold(
                    OsString::new(),
                    |mut acc, item| {
                        acc.push(item);
                        acc.push(" ");
                        acc
                    },
                );
                Some(Cow::Owned(res))
            }
            NumParams => Some(Cow::Owned(OsString::from(format!(
                "{}",
                env.special_vars().get_positionals().len()
            )))),
            ShellPid => Some(Cow::Owned(OsString::from(format!("{}", process::id())))),
            // TODO: should print out name of shell binary
            Positional(0) => unimplemented!(),
            Positional(num) => env.special_vars()
                .get_positionals()
                .get(*num - 1)
                .map(|item| Cow::Borrowed(item.as_os_str())),
            // TODO (background pid)
            _ => unimplemented!(),
        }
    }

    pub fn to_os_string(&self) -> Cow<OsStr> {
        use self::Param::*;

        match self {
            Var(ref s) => Cow::Borrowed(s),
            Star => Cow::Borrowed(OsStr::new("*")),
            Question => Cow::Borrowed(OsStr::new("?")),
            At => Cow::Borrowed(OsStr::new("@")),
            NumParams => Cow::Borrowed(OsStr::new("#")),
            ShellPid => Cow::Borrowed(OsStr::new("$")),
            BackgroundPid => Cow::Borrowed(OsStr::new("!")),
            Positional(num) => Cow::Owned(OsString::from(format!("{}", num))),
        }
    }
}

#[derive(Debug)]
pub struct ParamExpr {
    param: Param,
    // box everything within ParamExprKind rather than boxing ParamExprKind itself to avoid an
    // allocation in the case of Error/ErrorNull with no argument or Value/Length
    kind: ParamExprKind,
}

#[derive(Debug)]
pub enum ParamExprKind {
    Assign(Box<Word>),
    AssignNull(Box<Word>),

    Use(Box<Word>),
    UseNull(Box<Word>),

    Error(Option<Box<Word>>),
    ErrorNull(Option<Box<Word>>),

    Alternate(Box<Word>),
    AlternateNull(Box<Word>),

    SmallPrefix(Box<Word>),
    LargePrefix(Box<Word>),

    SmallSuffix(Box<Word>),
    LargeSuffix(Box<Word>),

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

    pub fn eval<'a: 'b, 'b, S>(&self, data: &mut RuntimeData<'a, 'b, S>) -> OsString
    where
        S: UtilSetup + 'a,
    {
        use self::ParamExprKind::*;

        let pval_valid;
        let not_null;

        // NOTE: this entire setup is to get around the borrow checker
        loop {
            let pval = self.param.eval(data.env);
            pval_valid = pval.is_some();
            not_null = pval.as_ref().map(|v| v.len() > 0).unwrap_or(false);
            return match self.kind {
                Assign(_) | Use(_) | Error(_) if not_null => pval.unwrap().into_owned(),
                AssignNull(_) | UseNull(_) | ErrorNull(_) if pval_valid => {
                    pval.unwrap().into_owned()
                }

                Length => pval.map(|v| OsString::from(v.len().to_string()))
                    .unwrap_or_else(|| OsString::from("0")),
                Value => pval.map(|v| v.into_owned()).unwrap_or_default(),

                _ => break,
            };
        }

        match self.kind {
            Alternate(ref word) if pval_valid => word.eval(data),
            AlternateNull(ref word) if not_null => word.eval(data),
            Alternate(_) | AlternateNull(_) => OsString::from(""),

            Assign(ref word) | AssignNull(ref word) => {
                let new_val = word.eval(data);
                // TODO: figure out what to do when not Var (e.g. $*)
                if let Param::Var(ref name) = self.param {
                    data.env.set_var(Cow::Borrowed(name), new_val.clone());
                }
                new_val
            }

            Use(ref word) | UseNull(ref word) => word.eval(data),

            Error(ref word) | ErrorNull(ref word) => {
                // TODO: figure out how to cleanly exit for scripts
                // FIXME: we should probably just return a Result (all the way up to
                //        CompleteCommand) and then print to stderr there
                let msg = word.as_ref().map(|w| w.eval(data));
                display_err!(
                    data.setup.error(),
                    "{}: {}",
                    self.param.to_os_string().to_string_lossy(),
                    msg.as_ref()
                        .map(|m| m.to_string_lossy())
                        .unwrap_or(Cow::from("parameter not set"))
                );
                // TODO: i assume this is where the result would go
                unimplemented!()
            }

            // TODO: prefixes and suffixes
            _ => unimplemented!(),
        }
    }
}

fn exec_andor_chain<'a: 'b, 'b, 'c, S>(
    data: &mut RuntimeData<'a, 'b, S>,
    chain: &'c [AndOr],
) -> ExitCode
where
    S: UtilSetup + 'a,
{
    let mut code = 0;
    for and_or in chain {
        code = and_or.execute(data, code);
        if check_break(data) {
            break;
        }
    }
    code
}

fn exec_list<'a: 'b, 'b, 'c, S>(
    data: &mut RuntimeData<'a, 'b, S>,
    list: &'c [Vec<AndOr>],
) -> ExitCode
where
    S: UtilSetup + 'a,
{
    let mut code = 0;
    for chain in list {
        code = exec_andor_chain(data, chain);
        if check_break(data) {
            break;
        }
    }
    code
}

fn write_error<S, T, U>(setup: &mut S, result: StdResult<T, U>) -> StdResult<T, ExitCode>
where
    S: UtilSetup,
    U: fmt::Display,
{
    match result {
        Ok(m) => Ok(m),
        Err(f) => {
            // FIXME: needs to print out line number
            // XXX: should we ignore any I/O errors?
            let _ = display_msg!(setup.error(), "{}", f);
            // FIXME: this could be different depending on the type of error
            Err(127)
        }
    }
}

/// Set up a "subshell" that actually executes in the current process.  This function takes care of
/// saving various information that could get destroyed/modified in the executed commands and
/// restoring it afterwards.
fn fake_subshell<'a: 'b, 'b, S, F>(data: &mut RuntimeData<'a, 'b, S>, func: F) -> ExitCode
where
    S: UtilSetup + 'a,
    F: FnOnce(&mut RuntimeData<'a, 'b, S>) -> ExitCode,
{
    let loop_depth = data.env.loop_depth();

    // TODO: enter_scope() needs to protect variables somehow, so I guess they need Locality as
    //       well?  Maybe clone env?
    data.env.enter_scope();
    let code = func(data);
    data.env.exit_scope();

    // fix loop depth
    data.env.set_loop_depth(loop_depth);

    // reset break counter in case it got set in the "subshell"
    data.env.set_break_counter(0);

    code
}

fn check_break_loop<'a: 'b, 'b, S>(data: &mut RuntimeData<'a, 'b, S>) -> CheckBreak
where
    S: UtilSetup + 'a,
{
    if check_break(data) {
        data.env.dec_break_counter();
        data.env.break_type()
    } else {
        CheckBreak::None
    }
}

fn check_break<'a: 'b, 'b, S>(data: &mut RuntimeData<'a, 'b, S>) -> bool
where
    S: UtilSetup + 'a,
{
    data.env.break_counter() != 0
}
