//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

extern crate clap;
extern crate libc;
extern crate nix;
use clap::Arg;
use nix::mount::MsFlags;
use std::collections::HashMap;
use std::ffi::OsString;
use std::fs;
use std::io::{self, BufRead, BufReader, Write};
use std::path::PathBuf;
use std::thread;
use {ArgsIter, LockError, Result, UtilSetup, UtilWrite};

pub(crate) const NAME: &str = "mount";
pub(crate) const DESCRIPTION: &str = "Mount file systems";

#[derive(Debug, Fail)]
enum MountError {
    #[fail(display = "{}", _0)]
    Output(#[cause] io::Error),
    #[fail(display = "Cannot open file: {}", _0)]
    OpenFileError(String),
    #[fail(display = "Bad format while reading {}", _0)]
    FSDescFileFormatError(String),
    #[fail(display = "Unsupported filesystem type")]
    UnsupportedFSType,
    #[fail(display = "unknown filesystem type '{}'.", _0)]
    UnknownFSType(String),
    #[fail(display = "Unsupported option")]
    UnsupportedOption,
    #[fail(display = "Only root can do that")]
    PermissionDenied,
    #[fail(display = "Invalid argument")]
    InvalidArgument,
    #[fail(display = "Cannot support UUID on your system")]
    UuidSupportError,
    #[fail(display = "Cannot support Label on your system")]
    LabelSupportError,
    #[fail(display = "Cannot find UUID=\"{}\"", _0)]
    UuidNotFoundError(String),
    #[fail(display = "Cannot find Label=\"{}\"", _0)]
    LabelNotFoundError(String),
    #[fail(display = "{}: mount point does not exist.", _0)]
    MountPointNotExist(String),
    #[fail(display = "{}: special device {} does not exist.", _0, _1)]
    DeviceNotExist(String, String),
    // Denotes an error caused by one of stdin, stdout, or stderr failing to lock
    #[fail(display = "{}", _0)]
    Lock(#[cause] LockError),
}

impl From<LockError> for MountError {
    fn from(err: LockError) -> Self {
        MountError::Lock(err)
    }
}

impl From<io::Error> for MountError {
    fn from(err: io::Error) -> Self {
        MountError::Output(err)
    }
}

// There are several types of mount, all of them implement Mount trait
trait Mount {
    fn run(&mut self) -> MountResult<()>;
}

struct MountCommand {
    multi_thread: bool,
    mount_list: Vec<Box<Mount + Send>>,
}

struct Uuid {
    // map UUID to actual path
    path_map: HashMap<OsString, PathBuf>,
}

impl Uuid {
    fn new() -> MountResult<Self> {
        let mut path_map: HashMap<OsString, PathBuf> = HashMap::new();
        let dir = fs::read_dir("/dev/disk/by-uuid").or(Err(MountError::UuidSupportError))?;
        for symlink in dir {
            let link = symlink.or(Err(MountError::UuidSupportError))?;
            path_map.insert(
                link.file_name(),
                link.path()
                    .canonicalize()
                    .or(Err(MountError::UuidSupportError))?,
            );
        }
        Ok(Self { path_map: path_map })
    }

    fn to_dev(&self, input: OsString) -> MountResult<PathBuf> {
        // OsString and OsStr lacks "starts_with" function
        // FIXME Maybe find a better way to test if input starts with "UUID="
        let dir;
        let s = input.clone();
        let s = s.to_string_lossy();
        if s.starts_with("UUID=") {
            dir = OsString::from(&s[5..]);
        } else {
            dir = input;
        }
        Ok(self
            .path_map
            .get(&dir)
            .ok_or(MountError::UuidNotFoundError(String::from(&s[5..])))?
            .clone())
    }
}

struct Label {
    // map Label to actual path
    path_map: HashMap<OsString, PathBuf>,
}

impl Label {
    fn new() -> MountResult<Self> {
        let mut path_map: HashMap<OsString, PathBuf> = HashMap::new();
        let dir = fs::read_dir("/dev/disk/by-label").or(Err(MountError::LabelSupportError))?;
        for symlink in dir {
            let link = symlink.or(Err(MountError::LabelSupportError))?;
            path_map.insert(
                link.file_name(),
                link.path()
                    .canonicalize()
                    .or(Err(MountError::LabelSupportError))?,
            );
        }
        Ok(Self { path_map: path_map })
    }

    fn to_dev(&self, input: OsString) -> MountResult<PathBuf> {
        // OsString and OsStr lacks "starts_with" function
        // FIXME Maybe find a better way to test if input starts with "Label="
        let dir;
        let s = input.clone();
        let s = s.to_string_lossy();
        if s.starts_with("Label=") {
            dir = OsString::from(&s[6..]);
        } else {
            dir = input;
        }
        Ok(self
            .path_map
            .get(&dir)
            .ok_or(MountError::LabelNotFoundError(String::from(&s[5..])))?
            .clone())
    }
}

struct Flag {
    option_map: HashMap<&'static str, MsFlags>,
    flag: MsFlags,
}

impl Default for Flag {
    fn default() -> Self {
        Self {
            option_map: Self::get_option_map(),
            flag: MsFlags::MS_SILENT,
        }
    }
}

impl Flag {
    fn get_option_map() -> HashMap<&'static str, MsFlags> {
        let mut option_map = HashMap::new();
        //option_map.insert("auto",MsFlags{bits: 0,}); // ignored
        //option_map.insert("noauto", MsFlags{bits: 0,}); // ignored
        //option_map.insert("defaults", MsFlags{bits: 0,}); // ignored
        //option_map.insert("nouser", MsFlags{bits: 0,}); // ignored
        //option_map.insert("user", MsFlags{bits: 0,}); // ignored
        option_map.insert("async", MsFlags::MS_SYNCHRONOUS);
        option_map.insert("atime", MsFlags::MS_NOATIME);
        option_map.insert("dev", MsFlags::MS_NODEV);
        option_map.insert("exec", MsFlags::MS_NOEXEC);
        option_map.insert("noatime", MsFlags::MS_NOATIME);
        option_map.insert("nodev", MsFlags::MS_NODEV);
        option_map.insert("noexec", MsFlags::MS_NOEXEC);
        option_map.insert("nosuid", MsFlags::MS_NOSUID);
        option_map.insert("remount", MsFlags::MS_REMOUNT);
        option_map.insert("ro", MsFlags::MS_RDONLY);
        option_map.insert("rw", MsFlags::MS_RDONLY);
        option_map.insert("suid", MsFlags::MS_NOSUID);
        option_map.insert("sync", MsFlags::MS_SYNCHRONOUS);
        option_map
    }

    fn add_flag(&mut self, f: &str) -> MountResult<()> {
        let flg = self.option_map.get(f).ok_or(MountError::UnsupportedOption)?;
        self.flag.insert(*flg);
        Ok(())
    }

    fn get_flag(options: &OsString) -> MountResult<MsFlags> {
        let s = options.clone();
        let s = s.to_string_lossy();
        let mut options: Vec<&str> = s.split(",").collect();
        let mut flag = Flag::default();
        if options.contains(&"default") {
            options.extend_from_slice(&["rw", "suid", "dev", "exec", "auto", "nouser", "async"]);
        }
        for opt in options {
            if opt == "" {
                continue;
            }
            flag.add_flag(opt)?;
        }
        Ok(flag.flag)
    }
}

struct MntEnt {
    mnt_fsname: OsString,
    mnt_dir: OsString,
    mnt_type: OsString,
    mnt_opts: OsString,
}

// This is used to read Filesystem Description File
// e.g. /etc/fstab and /etc/mtab
struct FSDescFile {
    list: Vec<MntEnt>,
}

impl FSDescFile {
    fn new(path: &str) -> MountResult<Self> {
        // all of these files should exist and can be read, but just in case
        let file = fs::File::open(path).or(Err(MountError::OpenFileError(String::from(path))))?;
        let mut list = Vec::new();
        for line in BufReader::new(file).lines() {
            let line = line.or(Err(MountError::FSDescFileFormatError(String::from(path))))?;
            match line.chars().next() {
                None | Some('#') => {
                    continue;
                }
                Some(_) => {}
            }
            let mut iter = line.split_whitespace();
            let mnt_fsname = iter
                .next()
                .ok_or(MountError::FSDescFileFormatError(String::from(path)))?;
            let mnt_dir = iter
                .next()
                .ok_or(MountError::FSDescFileFormatError(String::from(path)))?;
            let mnt_type = iter
                .next()
                .ok_or(MountError::FSDescFileFormatError(String::from(path)))?;
            let mnt_opts = iter
                .next()
                .ok_or(MountError::FSDescFileFormatError(String::from(path)))?;
            let mnt = MntEnt {
                mnt_fsname: OsString::from(mnt_fsname),
                mnt_dir: OsString::from(mnt_dir),
                mnt_type: OsString::from(mnt_type),
                mnt_opts: OsString::from(mnt_opts),
            };
            list.push(mnt)
        }
        Ok(Self { list: list })
    }
    fn print<O: Write>(&self, fs_type: OsString, output: &mut O) -> MountResult<()> {
        for item in &self.list {
            if fs_type != *"" {
                if fs_type != item.mnt_type {
                    continue;
                }
            }
            writeln!(
                output,
                "{} on {} type {} ({})",
                item.mnt_fsname.to_string_lossy(),
                item.mnt_dir.to_string_lossy(),
                item.mnt_type.to_string_lossy(),
                item.mnt_opts.to_string_lossy(),
            )?;
        }
        Ok(())
    }
}

// Define some special requirements
struct Property {
    fake: bool,
    // TODO user mountable devices
}

impl Default for Property {
    fn default() -> Self {
        Self { fake: false }
    }
}

// OsString has limited methods, implement them
trait OsStringExtend {
    fn starts_with(&self, pat: &str) -> bool;
    fn contains(&self, pat: &str) -> bool;
}

impl OsStringExtend for OsString {
    fn starts_with(&self, pat: &str) -> bool {
        self.to_string_lossy().starts_with(pat)
    }
    fn contains(&self, pat: &str) -> bool {
        self.to_string_lossy().contains(pat)
    }
}

// -a and -F are parsed at the very beginning
impl<'a> MountCommand {
    fn new<S: UtilSetup>(setup: &'a mut S, matches: &clap::ArgMatches) -> MountResult<Self> {
        let mut mount_list: Vec<Box<Mount + Send>> = Vec::new();
        let multi_thread = if matches.is_present("F") { true } else { false };
        // If -a exists, mount all the entries in /etc/fstab, except for those who contain "noauto"
        if matches.is_present("a") {
            let fstab = FSDescFile::new("/etc/fstab")?;
            for item in fstab.list {
                if item.mnt_opts.contains("noauto") {
                    continue;
                }
                // In this case, all the mounts are of type "CreateMountPoint"
                let m = CreateMountPoint::new(
                    // TODO detect user mountable devices
                    Property::default(),
                    item.mnt_fsname,
                    PathBuf::from(item.mnt_dir),
                    item.mnt_type,
                    item.mnt_opts,
                    OsString::from(""),
                )?;
                mount_list.push(Box::new(m));
            }
        }
        // If -a dosn't exist, read arguments from command line, and find out the mount type
        else {
            let (_, output, _) = setup.stdio();
            let mut stdout = output.lock()?;
            let mut arg1 = match matches.value_of("arg1") {
                Some(arg1) => OsString::from(arg1),
                None => OsString::from(""),
            };
            let mut arg2 = match matches.value_of("arg2") {
                Some(arg2) => OsString::from(arg2),
                None => OsString::from(""),
            };
            let fs_type = match matches.value_of("t") {
                Some(t) => OsString::from(t),
                None => OsString::from(""),
            };
            let options: Vec<&str> = match matches.values_of("o") {
                Some(t) => t.collect(),
                None => Vec::new(),
            };
            let fake = if matches.is_present("f") { true } else { false };
            let property = Property { fake: fake };
            // We can use UUID as source
            if let Some(uuid) = matches.value_of("U") {
                arg2 = arg1;
                arg1 = OsString::from(String::from("UUID=") + uuid);
            }
            // We can use Label as source
            if let Some(uuid) = matches.value_of("L") {
                arg2 = arg1;
                arg1 = OsString::from(String::from("Label=") + uuid);
            }
            // no argument
            if arg1 == *"" {
                let mut m = ShowMountPoints::new(fs_type, &mut stdout);
                m.run()?;
            }
            // one argument
            else if arg1 != *"" && arg2 == *"" {
                if options.contains(&"remount") {
                    let m = Remount::new(
                        property,
                        arg1,
                        OsString::from(options.join(",")),
                        OsString::from(""),
                    );
                    mount_list.push(Box::new(m));
                } else {
                    let fstab = FSDescFile::new("/etc/fstab")?;
                    for item in fstab.list {
                        if arg1 == item.mnt_fsname || arg1 == item.mnt_dir {
                            let m = CreateMountPoint::new(
                                property,
                                item.mnt_fsname,
                                PathBuf::from(item.mnt_dir),
                                item.mnt_type,
                                item.mnt_opts,
                                OsString::from(""),
                            )?;
                            mount_list.push(Box::new(m));
                            break;
                        }
                    }
                    if mount_list.len() == 0 {
                        return Err(MountError::InvalidArgument);
                    }
                }
            }
            // two argument
            else {
                let m = CreateMountPoint::new(
                    property,
                    arg1,
                    PathBuf::from(arg2),
                    fs_type,
                    OsString::from(options.join(",")),
                    OsString::from(""),
                )?;
                mount_list.push(Box::new(m));
            }
        }
        Ok(Self {
            multi_thread: multi_thread,
            mount_list: mount_list,
        })
    }
    fn run(mut self) -> MountResult<()> {
        if self.multi_thread {
            let mut handle = vec![];
            for mut m in self.mount_list {
                handle.push(thread::spawn(move || m.run()));
            }
            for h in handle {
                if let Err(_) = h.join() {
                    return Err(MountError::InvalidArgument);
                }
            }
        } else {
            for m in &mut self.mount_list {
                m.run()?;
            }
        }
        Ok(())
    }
}

/*
 * Show mount points from /proc/mounts
 * If -t is specified, only output mount points of this file system type
 * Usage examples: "mount", "mount -t ext4"
 */
struct ShowMountPoints<'a, O: Write + 'a> {
    stdout: &'a mut O,
    filesystem_type: OsString,
}

impl<'a, O: Write> ShowMountPoints<'a, O> {
    fn new(filesystem_type: OsString, stdout: &'a mut O) -> Self {
        Self {
            stdout: stdout,
            filesystem_type: filesystem_type,
        }
    }
}

impl<'a, O: Write> Mount for ShowMountPoints<'a, O> {
    fn run(&mut self) -> MountResult<()> {
        FSDescFile::new("/proc/mounts")?.print(self.filesystem_type.clone(), self.stdout)?;
        Ok(())
    }
}

/*
 * Create a new mount point
 * Usage examples: "mount /dev/sda1 /home/username/mnt"
 */
struct CreateMountPoint {
    property: Property,
    source: PathBuf,
    target: PathBuf,
    filesystem_type: OsString,
    mountflags: OsString,
    data: OsString,
}

impl CreateMountPoint {
    fn new(
        property: Property,
        source: OsString,
        target: PathBuf,
        filesystem_type: OsString,
        mountflags: OsString,
        data: OsString,
    ) -> MountResult<Self> {
        Ok(Self {
            property,
            source: Self::parse_source(source)?,
            target,
            filesystem_type,
            mountflags,
            data,
        })
    }
    fn parse_source(source: OsString) -> MountResult<PathBuf> {
        if source.starts_with("UUID=") {
            let uuid = Uuid::new()?;
            Ok(PathBuf::from(uuid.to_dev(source)?))
        } else if source.starts_with("Label=") {
            let label = Label::new()?;
            Ok(PathBuf::from(label.to_dev(source)?))
        } else {
            Ok(PathBuf::from(source))
        }
    }
}

impl Mount for CreateMountPoint {
    fn run(&mut self) -> MountResult<()> {
        // check privilege
        // getuid() is always successful, so it's ok to use it
        if unsafe { libc::getuid() } != 0 {
            return Err(MountError::PermissionDenied);
        }
        // check if mount point exists
        if !self.target.exists() {
            return Err(MountError::MountPointNotExist(String::from(
                self.target.to_string_lossy(),
            )));
        }
        // check if device exists
        if !self.source.exists() {
            let s = String::from(self.source.to_string_lossy());
            let t = String::from(self.target.to_string_lossy());
            return Err(MountError::DeviceNotExist(t, s));
        }
        // if type is not specified, auto detect filesystem type
        if self.filesystem_type == *"" || self.filesystem_type == *"auto" {
            let file_name = "/proc/filesystems";
            let file = match fs::File::open(file_name) {
                Ok(f) => f,
                Err(_) => {
                    // this file should always exist, but just in case
                    return Err(MountError::OpenFileError(String::from(file_name)));
                }
            };
            for line in BufReader::new(file).lines() {
                let line = line?;
                match line.chars().next() {
                    Some(line) => {
                        if !line.is_whitespace() {
                            continue; // skip nodev devices
                        }
                    }
                    None => {
                        continue; // skip empty lines
                    }
                }
                let try_fs_type = &line[1..];
                let mountflags = Flag::get_flag(&self.mountflags)?;
                match nix::mount::mount(
                    Some(self.source.as_os_str()),
                    self.target.as_os_str(),
                    Some(try_fs_type),
                    mountflags,
                    Some(self.data.as_os_str()),
                ).or(Err(MountError::from(io::Error::last_os_error())))
                {
                    Ok(_) => return Ok(()),
                    Err(_) => { /*println!("get error number: {}", nix::errno::errno());*/ }
                }
            }
            return Err(MountError::UnsupportedFSType);
        } else {
            // if type is specified
            let mountflags = Flag::get_flag(&self.mountflags)?;
            if !self.property.fake {
                match nix::mount::mount(
                    Some(self.source.as_os_str()),
                    self.target.as_os_str(),
                    Some(self.filesystem_type.as_os_str()),
                    mountflags,
                    Some(self.data.as_os_str()),
                ).or(Err(MountError::from(io::Error::last_os_error())))
                {
                    Ok(_) => return Ok(()),
                    Err(_) => {
                        // errno == 19 means "No such device"
                        // this happens if you provide a wrong filesystem type
                        if nix::errno::errno() == 19 {
                            return Err(MountError::UnknownFSType(String::from(
                                self.filesystem_type.to_string_lossy(),
                            )));
                        }
                        return Err(MountError::from(io::Error::last_os_error()));
                    }
                }
            }
            Ok(())
        }
    }
}

/*
 * Remount an existing mount
 * Usage examples: "mount -o remount /home/username/mnt"
 */
struct Remount {
    property: Property,
    target: PathBuf,
    mountflags: OsString,
    data: OsString,
}

impl Remount {
    fn new(property: Property, target: OsString, mountflags: OsString, data: OsString) -> Self {
        Self {
            property,
            target: PathBuf::from(target),
            mountflags,
            data,
        }
    }
}

impl Mount for Remount {
    fn run(&mut self) -> MountResult<()> {
        // check privilege
        // getuid() is always successful, so it's ok to use it
        if unsafe { libc::getuid() } != 0 {
            return Err(MountError::PermissionDenied);
        }
        let mounts = FSDescFile::new("/proc/mounts")?;
        let mut source = OsString::new();
        let mut target = OsString::new();
        let mut filesystem_type = OsString::new();
        for item in mounts.list.iter().rev() {
            if self.target == item.mnt_fsname || self.target == item.mnt_dir {
                source = item.mnt_fsname.clone();
                target = item.mnt_dir.clone();
                filesystem_type = item.mnt_type.clone();
                break;
            }
        }
        let mountflags = Flag::get_flag(&self.mountflags)?;
        if !self.property.fake {
            nix::mount::mount(
                Some(source.as_os_str()),
                target.as_os_str(),
                Some(filesystem_type.as_os_str()),
                mountflags,
                Some(self.data.as_os_str()),
            ).or(Err(MountError::from(io::Error::last_os_error())))?
        }
        Ok(())
    }
}

type MountResult<T> = ::std::result::Result<T, MountError>;

pub fn execute<S, T>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    let matches = {
        let app = util_app!(NAME)
            .author("Zhuohua Li <zhuohuali@baidu.com>")
            .arg(Arg::with_name("arg1")
                .index(1))
            .arg(Arg::with_name("arg2")
                .index(2))
            .arg(Arg::with_name("v")
                .short("v")
                .help("invoke verbose mode. The mount command shall provide diagnostic messages on stdout."))
            .arg(Arg::with_name("a")
                .short("a")
                .help("mount all file systems (of the given types) mentioned in /etc/fstab."))
            .arg(Arg::with_name("F")
                .short("F")
                .requires("a")
                .help("If the -a option is also present, fork a new incarnation of mount for each device to be mounted. This will do the mounts on different devices or different NFS servers in parallel."))
            .arg(Arg::with_name("f")
                .short("f")
                .help("cause everything to be done except for the actual system call; if it's not obvious, this `fakes' mounting the file system."))
            // FIXME not supported yet
            .arg(Arg::with_name("n")
                .short("n")
                .help("mount without writing in /etc/mtab. This is necessary for example when /etc is on a read-only file system."))
            // FIXME not supported yet
            .arg(Arg::with_name("s")
                .short("s")
                .help("ignore mount options not supported by a file system type. Not all file systems support this option."))
            .arg(Arg::with_name("r")
                .short("r")
                .conflicts_with("w")
                .help("mount the file system read-only. A synonym is -o ro."))
            .arg(Arg::with_name("w")
                .short("w")
                .conflicts_with("r")
                .help("mount the file system read/write. (default) A synonym is -o rw."))
            .arg(Arg::with_name("L")
                .short("L")
                .help("If the file /proc/partitions is supported, mount the partition that has the specified label.")
                .takes_value(true)
                .conflicts_with("U")
                .value_name("label"))
            .arg(Arg::with_name("U")
                .short("U")
                .help("If the file /proc/partitions is supported, mount the partition that has the specified uuid.")
                .takes_value(true)
                .conflicts_with("L")
                .value_name("uuid"))
            .arg(Arg::with_name("t")
                .short("t")
                .help("indicate a file system type of vfstype.{n}{n}More than one type may be specified in a comma separated list. The list of file system types can be prefixed with no to specify the file system types on which no action should be taken.")
                .takes_value(true)
                .value_name("vfstype"))
            .arg(Arg::with_name("o")
                .short("o")
                .help("options are specified with a -o flag followed by a comma-separated string of options. Some of these options are only useful when they appear in the /etc/fstab file. The following options apply to any file system that is being mounted:{n}{n}async{n}\tperform all I/O to the file system asynchronously.{n}{n}atime{n}\tupdate inode access time for each access. (default){n}{n}auto{n}\tin /etc/fstab, indicate the device is mountable with -a.{n}{n}defaults{n}\tuse default options: rw, suid, dev, exec, auto, nouser, async.{n}{n}dev{n}\tinterpret character or block special devices on the file system.{n}{n}exec{n}\tpermit execution of binaries.{n}{n}noatime{n}\tdo not update file access times on this file system.{n}{n}noauto{n}\tin /etc/fstab, indicates the device is only explicitly mountable.{n}{n}nodev{n}\tdo not interpret character or block special devices on the file system.{n}{n}noexec{n}\tdo not allow execution of any binaries on the mounted file system.{n}{n}nosuid{n}\tdo not allow set-user-identifier or set-group-identifier bits to take effect.{n}{n}nouser{n}\tforbid an unprivileged user to mount the file system. (default){n}{n}remount{n}\tremount an already-mounted file system. This is commonly used to change the mount options for a file system, especially to make a read-only file system writable.{n}{n}ro{n}\tmount the file system read-only.{n}{n}rw{n}\tmount the file system read-write.{n}{n}suid{n}\tallow set-user-identifier or set-group-identifier bits to take effect.{n}{n}sync{n}\tdo all I/O to the file system synchronously.{n}{n}user{n}\tallow an unprivilieged user to mount the file system. This option implies the options noexec, nosuid, nodev unless overridden by subsequent options.")
                .takes_value(true)
                .value_name("options")
                .use_delimiter(true)
                .possible_values(&["async", "atime", "defaults", "dev", "exec", "noatime", "nodev", "noexec", "nosuid", "nouser", "remount", "ro", "rw", "suid", "sync", "user"])
                .hide_possible_values(true));

        app.get_matches_from_safe(args)?
    };

    let mount_command = MountCommand::new(setup, &matches)?;
    Ok(mount_command.run()?)
}
