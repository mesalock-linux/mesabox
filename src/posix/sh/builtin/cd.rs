use clap::{App, AppSettings, Arg};
use libc::c_long;
use nix::unistd::{self, PathconfVar};

use std::borrow::Cow;
use std::env;
use std::ffi::OsStr;
use std::io::Write;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use super::{BuiltinSetup, BuiltinError, UtilSetup, UtilWrite, Environment, ExecData, ExitCode, Result};

#[derive(Clone, Copy)]
pub struct CdBuiltin;

// FIXME: this is fine if we are getting rid of util::actual_path(), but we need to be careful when
//        making fake "subshells" to not do so when "cd" is involved as it sets global state (we
//        could potentially just reset the directory if we detect it has changed, but that could
//        always fail, which makes it safer to just use real subshells with stuff involving "cd")
impl BuiltinSetup for CdBuiltin {
    fn run<S>(&self, setup: &mut S, env: &mut Environment, data: ExecData) -> Result<ExitCode>
    where
        S: UtilSetup,
    {
        // TODO: suppress --help/--version
        let matches = App::new("cd")
            .setting(AppSettings::NoBinaryName)
            .arg(Arg::with_name("logical")
                .short("L")
                .overrides_with("physical"))
            .arg(Arg::with_name("physical")
                .short("P"))
            .arg(Arg::with_name("DIRECTORY")
                .index(1))
            .get_matches_from_safe(data.args)?;

        let mut should_print = false;

        let dir = if let Some(dir) = matches.value_of_os("DIRECTORY") {
            if dir == OsStr::new("-") {
                // command should be equivalent to: cd "$OLDPWD" && pwd
                should_print = true;
                if let Some(oldpwd) = env.get_var("OLDPWD") {
                    Cow::Owned(oldpwd.clone())
                } else {
                    Cow::Borrowed(OsStr::new(""))
                }
            } else {
                Cow::Borrowed(dir)
            }
        } else {
            match env.get_var_nonempty("HOME") {
                Some(dir) => Cow::Owned(dir.clone()),
                None => return Ok(0),
            }
        };

        let dirlen = dir.len();
        let physical = matches.is_present("physical");

        let dir = Path::new(&dir);
        if dir.is_absolute() || dir.starts_with("..") {
            resolve_dot(env, dir.into(), physical, dirlen)
        } else {
            let mut curpath = dir.into();
            if let Some(cdpath) = env.get_var("CDPATH") {
                for path in cdpath.as_bytes().split(|&byte| byte == b':') {
                    let newpath = Path::new(OsStr::from_bytes(path)).join(dir);
                    if newpath.is_dir() {
                        curpath = newpath.into();
                        should_print = true;
                        break;
                    }
                }
            }
            resolve_dot(env, curpath, physical, dirlen)
        }.map(|code| {
            if should_print {
                // XXX: ignore errors?
                let output = setup.output();
                if let Ok(mut output) = output.lock_writer() {
                    let _ = output.write_all(env.get_var("PWD").unwrap().as_bytes());
                    let _ = writeln!(output);
                }
            }
            code
        })
    }
}

fn resolve_dot(env: &mut Environment, curpath: Cow<Path>, physical: bool, dirlen: usize) -> Result<ExitCode> {
    if physical {
        set_cwd(env, curpath, physical, dirlen)
    } else {
        let curpath = if curpath.is_absolute() {
            curpath
        } else {
            if let Some(pwd) = env.get_var_nonempty("PWD") {
                Path::new(pwd).join(curpath).into()
            } else {
                curpath
            }
        };
        let canonical = curpath.canonicalize().map_err(|e| BuiltinError::Io(e))?;

        // NOTE: technically, we are supposed to do the PATH_MAX stuff here, but as it is an edge
        //       case we are going to try to change the directory first and only do the path
        //       shortening if changing directories fails
        set_cwd(env, canonical.into(), physical, dirlen)
    }
}

fn set_cwd(env: &mut Environment, curpath: Cow<Path>, physical: bool, dirlen: usize) -> Result<ExitCode> {
    if let Err(f) = env::set_current_dir(&curpath) {
        if !physical {
            let res = fixup_path(env, &curpath, dirlen)
                .and_then(|relpath| env::set_current_dir(relpath).ok());
            if res.is_some() {
                return set_pwd_var(env, curpath);
            }
        }
        Err(BuiltinError::SetCurrentDir { dir: curpath.into_owned().into_os_string(), err: f })
    } else {
        set_pwd_var(env, curpath)
    }
}

fn fixup_path<'a>(env: &Environment, curpath: &'a Path, dirlen: usize) -> Option<&'a Path> {
    // we don't care about error cases as we can just say that setting the directory failed
    if let Ok(pathmax) = unistd::pathconf(curpath, PathconfVar::PATH_MAX) {
        let pathmax = pathmax.unwrap_or(c_long::max_value()) as usize;

        // note that we need to account for terminating null
        if curpath.as_os_str().len() >= pathmax && dirlen < pathmax {
            // try to strip PWD from curpath (if this fails we just assume that we can't fix
            // the path as this is the only thing mandated by POSIX)
            if let Some(pwd) = env.get_var_nonempty("PWD") {
                return curpath.strip_prefix(pwd).ok();
            }
        }
    }

    None
}

fn set_pwd_var(env: &mut Environment, curpath: Cow<Path>) -> Result<ExitCode> {
    let oldpwd = env.set_export_var(Cow::Borrowed(OsStr::new("PWD")), curpath.into_owned().into_os_string());
    match oldpwd {
        Some(Some(dir)) => {
            env.set_export_var(Cow::Borrowed(OsStr::new("OLDPWD")), dir);
        }
        Some(None) => {
            env.remove_var("OLDPWD");
            env.export_var(Cow::Borrowed(OsStr::new("OLDPWD")));
        }
        None => { /* XXX: do nothing or remove the variable? */ }
    }

    Ok(0)
}
