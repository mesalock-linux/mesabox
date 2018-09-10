//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

extern crate clap;
extern crate lazy_static;
extern crate libc;
extern crate nix;
use clap::{App, Arg};
use nix::mount::MsFlags;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io::{self, BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::thread;
use {ArgsIter, LockError, Result, UtilSetup, UtilWrite};

pub(crate) const NAME: &str = "mount";
pub(crate) const DESCRIPTION: &str = "Mount file systems";

type MountResult<T> = ::std::result::Result<T, MountError>;

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
    #[fail(display = "{}: mount point not mounted or bad option.", _0)]
    MountPointNotMounted(String),
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

/// There are several types of mount task, all of them implement Mountable trait
trait Mountable {
    // Sometimes mount prints messages, so it returns a string
    fn run(&mut self) -> MountResult<String>;
}

/// Store information that we need to execute a mount command
struct MountOptions {
    multi_thread: bool,
    // Mount command might mount a lot of devices at the same time, so we cache them in a list
    mount_list: Vec<Box<Mountable + Send>>,
}

/// Translate UUID into corresponding device path
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

    fn get_device_path(&self, input: &OsString) -> MountResult<&Path> {
        let input_string = input.to_string_lossy();
        let dir;
        if input_string.starts_with("UUID=") {
            dir = OsStr::new(&input_string[5..]);
        } else {
            dir = input;
        }
        Ok(self
            .path_map
            .get(dir)
            .ok_or(MountError::UuidNotFoundError(
                dir.to_string_lossy().to_string(),
            ))?.as_path())
    }
}

/// Translate Label into corresponding device path
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

    fn get_device_path(&self, input: &OsString) -> MountResult<&Path> {
        let input_string = input.to_string_lossy();
        let dir;
        if input_string.starts_with("Label=") {
            dir = OsStr::new(&input_string[6..]);
        } else {
            dir = input;
        }
        Ok(self
            .path_map
            .get(dir)
            .ok_or(MountError::LabelNotFoundError(dir.to_string_lossy().to_string()))?
            .as_path())
    }
}

lazy_static! {
    //static ref OPTION_MAP: HashMap<&'static str, MsFlags> = {
    static ref OPTION_MAP: HashMap<OsString, MsFlags> = {
        let mut option_map = HashMap::new();
        option_map.insert(OsString::from("async"), MsFlags::MS_SYNCHRONOUS);
        option_map.insert(OsString::from("atime"), MsFlags::MS_NOATIME);
        option_map.insert(OsString::from("dev"), MsFlags::MS_NODEV);
        option_map.insert(OsString::from("exec"), MsFlags::MS_NOEXEC);
        option_map.insert(OsString::from("noatime"), MsFlags::MS_NOATIME);
        option_map.insert(OsString::from("nodev"), MsFlags::MS_NODEV);
        option_map.insert(OsString::from("noexec"), MsFlags::MS_NOEXEC);
        option_map.insert(OsString::from("nosuid"), MsFlags::MS_NOSUID);
        option_map.insert(OsString::from("remount"), MsFlags::MS_REMOUNT);
        option_map.insert(OsString::from("ro"), MsFlags::MS_RDONLY);
        option_map.insert(OsString::from("rw"), MsFlags::MS_RDONLY);
        option_map.insert(OsString::from("suid"), MsFlags::MS_NOSUID);
        option_map.insert(OsString::from("sync"), MsFlags::MS_SYNCHRONOUS);
        option_map
    };
}

struct Flag {
    flag: MsFlags,
}

impl Default for Flag {
    fn default() -> Self {
        Self {
            flag: MsFlags::MS_SILENT,
        }
    }
}

impl Flag {
    fn from(options: &mut Vec<OsString>) -> MountResult<Flag> {
        let mut flag = Flag::default();
        if options.contains(&OsString::from("default")) {
            options.extend_from_slice(&[
                OsString::from("rw"),
                OsString::from("suid"),
                OsString::from("dev"),
                OsString::from("exec"),
                OsString::from("auto"),
                OsString::from("nouser"),
                OsString::from("async"),
            ]);
        }
        for opt in options {
            flag.flag
                .insert(*OPTION_MAP.get(opt).ok_or(MountError::UnsupportedOption)?);
        }
        Ok(flag)
    }
}

struct MntEnt {
    mnt_fsname: OsString,
    mnt_dir: OsString,
    mnt_type: OsString,
    mnt_opts: OsString,
}

/// This is used to read Filesystem Description File
/// e.g. /etc/fstab and /etc/mtab
struct FSDescFile {
    entries: Vec<MntEnt>,
}

impl FSDescFile {
    fn new(path: &str) -> MountResult<Self> {
        // all of these files should exist and can be read, but just in case
        let file = fs::File::open(path).or(Err(MountError::OpenFileError(String::from(path))))?;
        let mut entries = Vec::new();
        for line in BufReader::new(file).lines() {
            let line = line.or(Err(MountError::FSDescFileFormatError(String::from(path))))?;
            match line.chars().next() {
                None | Some('#') => {
                    continue;
                }
                Some(_) => {}
            }
            let a: Vec<&str> = line.split_whitespace().collect();
            // There should be 6 columns in FileSystem Description Files
            if a.len() != 6 {
                return Err(MountError::FSDescFileFormatError(String::from(path)));
            }
            // We only need the first 4 columns
            let mnt = MntEnt {
                mnt_fsname: OsString::from(a[0]),
                mnt_dir: OsString::from(a[1]),
                mnt_type: OsString::from(a[2]),
                mnt_opts: OsString::from(a[3]),
            };
            entries.push(mnt)
        }
        Ok(Self { entries: entries })
    }

    fn get_output(&self, fs_type: &OsString) -> String {
        let mut ret = String::new();
        for item in &self.entries {
            if *fs_type != *"" {
                if *fs_type != item.mnt_type {
                    continue;
                }
            }
            ret.push_str(
                format!(
                    "{} on {} type {} ({})\n",
                    item.mnt_fsname.to_string_lossy(),
                    item.mnt_dir.to_string_lossy(),
                    item.mnt_type.to_string_lossy(),
                    item.mnt_opts.to_string_lossy(),
                ).as_str(),
            );
        }
        ret
    }
}

/// Define some special requirements
struct Property {
    fake: bool,
    use_uuid: bool,
    use_label: bool,
    // TODO user mountable devices
}

impl Default for Property {
    fn default() -> Self {
        Self {
            fake: false,
            use_uuid: false,
            use_label: false,
        }
    }
}

/// OsString has limited methods, implement them
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

impl MountOptions {
    fn from_matches(matches: &clap::ArgMatches) -> MountResult<Self> {
        let mut mount_list: Vec<Box<Mountable + Send>> = Vec::new();

        // If -a exists, mount all the entries in /etc/fstab, except for those who contain "noauto"
        if matches.is_present("a") {
            let fstab = FSDescFile::new("/etc/fstab")?;
            for item in fstab.entries {
                if item.mnt_opts.contains("noauto") {
                    continue;
                }
                // Split the comma separated option string into a vector, also convert &str into OsString
                let opts: Vec<OsString> = item
                    .mnt_opts
                    .to_string_lossy()
                    .split(",")
                    .collect::<Vec<&str>>()
                    .into_iter()
                    .map(|i| OsString::from(i))
                    .collect();
                // In this case, all the mounts are of type "CreateMountPoint"
                let m = CreateMountPoint::new(
                    Property::default(),
                    item.mnt_fsname,
                    PathBuf::from(item.mnt_dir),
                    Some(item.mnt_type),
                    Some(opts),
                    OsString::new(),
                )?;
                mount_list.push(Box::new(m));
            }
        }
        // If -a doesn't exist, read arguments from command line, and find out the mount type
        else {
            let mut arg1 = matches.value_of("arg1");
            let mut arg2 = matches.value_of("arg2");
            let fs_type = matches.value_of("t");

            let options: Option<Vec<OsString>> = matches
                .values_of("o")
                .map(|i| i.collect())
                .map(|i: Vec<&str>| i.into_iter().map(|s| OsString::from(s)).collect());

            let property = Property {
                fake: matches.is_present("f"),
                use_uuid: matches.is_present("U"),
                use_label: matches.is_present("L"),
            };

            // If -U exists, then use UUID as source
            if let Some(uuid) = matches.value_of("U") {
                arg2 = arg1;
                arg1 = Some(uuid);
            }

            // If -L exists, then use Label as source
            if let Some(label) = matches.value_of("L") {
                arg2 = arg1;
                arg1 = Some(label);
            }

            match arg1 {
                Some(arg1) => {
                    match arg2 {
                        // Two arguments
                        Some(arg2) => {
                            let m = CreateMountPoint::new(
                                property,
                                OsString::from(arg1),
                                PathBuf::from(arg2),
                                fs_type.map(|t| OsString::from(t)),
                                options,
                                OsString::new(),
                            )?;
                            mount_list.push(Box::new(m));
                        }
                        // One argument
                        None => match options {
                            Some(ref opts) if opts.contains(&OsString::from("remount")) => {
                                let m = Remount::new(
                                    property,
                                    OsString::from(arg1),
                                    opts.to_vec(),
                                    OsString::new(),
                                );
                                mount_list.push(Box::new(m));
                            }
                            _ => {
                                let fstab = FSDescFile::new("/etc/fstab")?;
                                for item in fstab.entries {
                                    if arg1 == item.mnt_fsname || arg1 == item.mnt_dir {
                                        let m = CreateMountPoint::new(
                                            property,
                                            item.mnt_fsname,
                                            PathBuf::from(item.mnt_dir),
                                            Some(item.mnt_type),
                                            options,
                                            OsString::new(),
                                        )?;
                                        mount_list.push(Box::new(m));
                                        break;
                                    }
                                }
                                if mount_list.len() == 0 {
                                    return Err(MountError::InvalidArgument);
                                }
                            }
                        },
                    }
                }
                // no argument
                None => {
                    let m = ShowMountPoints::new(fs_type);
                    mount_list.push(Box::new(m));
                }
            }
        }
        Ok(Self {
            multi_thread: matches.is_present("F"),
            mount_list: mount_list,
        })
    }
}

struct Mounter<O>
where
    O: Write,
{
    output: O,
}

impl<O> Mounter<O>
where
    O: Write,
{
    fn new(output: O) -> Self {
        Mounter { output }
    }

    fn mount(&mut self, mut options: MountOptions) -> MountResult<()> {
        if options.multi_thread {
            let mut handle = vec![];
            for mut m in options.mount_list {
                handle.push(thread::spawn(move || m.run()));
            }
            for h in handle {
                if let Err(_) = h.join() {
                    return Err(MountError::from(io::Error::last_os_error()));
                }
            }
        } else {
            for m in &mut options.mount_list {
                write!(self.output, "{}", m.run()?);
            }
        }
        Ok(())
    }
}

/// Show mount points from /proc/mounts
/// If -t is specified, only output mount points of this file system type
/// Usage examples: "mount", "mount -t ext4"
struct ShowMountPoints {
    filesystem_type: OsString,
}

impl ShowMountPoints {
    fn new(filesystem_type: Option<&str>) -> Self {
        let t = match filesystem_type {
            Some(t) => OsString::from(t),
            None => OsString::new(),
        };
        Self { filesystem_type: t }
    }
}

impl Mountable for ShowMountPoints {
    fn run(&mut self) -> MountResult<String> {
        Ok(FSDescFile::new("/proc/mounts")?.get_output(&self.filesystem_type))
    }
}

/// Create a new mount point
/// Usage examples: "mount /dev/sda1 /home/username/mnt"
struct CreateMountPoint {
    property: Property,
    source: PathBuf,
    target: PathBuf,
    filesystem_type: Option<OsString>,
    mountflags: Option<Vec<OsString>>,
    data: OsString,
}

impl CreateMountPoint {
    fn new(
        property: Property,
        source: OsString,
        target: PathBuf,
        filesystem_type: Option<OsString>,
        mountflags: Option<Vec<OsString>>,
        data: OsString,
    ) -> MountResult<Self> {
        // If source is an UUID or LABEL, get the corresponding device path
        // If source is read from /etc/fstab, check its prefix
        // If source is read from command line, check its property
        let device_path;
        if source.starts_with("UUID=") || property.use_uuid {
            let uuid = Uuid::new()?;
            device_path = PathBuf::from(uuid.get_device_path(&source)?);
        } else if source.starts_with("Label=") || property.use_label {
            let label = Label::new()?;
            device_path = PathBuf::from(label.get_device_path(&source)?);
        } else {
            device_path = PathBuf::from(source);
        }
        Ok(Self {
            property,
            source: device_path,
            target,
            filesystem_type,
            mountflags,
            data,
        })
    }
}

impl Mountable for CreateMountPoint {
    fn run(&mut self) -> MountResult<String> {
        // check privilege
        // getuid() is always successful, so it's ok to use it
        if unsafe { libc::getuid() } != 0 {
            return Err(MountError::PermissionDenied);
        }

        // check if mount point exists
        if !self.target.exists() {
            return Err(MountError::MountPointNotExist(
                self.target.to_string_lossy().to_string(),
            ));
        }

        // check if device exists
        if !self.source.exists() {
            let s = self.source.to_string_lossy().to_string();
            let t = self.target.to_string_lossy().to_string();
            return Err(MountError::DeviceNotExist(t, s));
        }

        // Get mountflags
        let mountflags = match self.mountflags {
            Some(ref mut mountflags) => Flag::from(mountflags)?.flag,
            None => Flag::default().flag,
        };

        // If type is not specified or "auto", automatically detect filesystem type
        if Some(OsString::from("auto")) == self.filesystem_type {
            self.filesystem_type = None;
        }
        match self.filesystem_type {
            None => {
                // Read all the filesystem types that we support
                let file_name = "/proc/filesystems";
                let file = fs::File::open(file_name)
                    .or(Err(MountError::OpenFileError(String::from(file_name))))?;
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
                    // Empty lines are already skipped, so it is ok to read array from index 1
                    let try_fs_type = &line[1..];
                    if let Ok(_) = nix::mount::mount(
                        Some(self.source.as_os_str()),
                        self.target.as_os_str(),
                        Some(try_fs_type),
                        mountflags,
                        Some(self.data.as_os_str()),
                    ).or(Err(MountError::from(io::Error::last_os_error())))
                    {
                        return Ok(String::new());
                    }
                }
                // Now we tried all the types that we support and none of them succeed
                return Err(MountError::UnsupportedFSType);
            }

            // If type is specified
            Some(ref fs_type) => {
                if !self.property.fake {
                    match nix::mount::mount(
                        Some(self.source.as_os_str()),
                        self.target.as_os_str(),
                        Some(fs_type.as_os_str()),
                        mountflags,
                        Some(self.data.as_os_str()),
                    ).or(Err(MountError::from(io::Error::last_os_error())))
                    {
                        Ok(_) => return Ok(String::new()),
                        Err(e) => {
                            // Error number 19 means "No such device"
                            // This happens if you provide a wrong filesystem type
                            if nix::errno::errno() == 19 {
                                return Err(MountError::UnknownFSType(String::from(
                                    fs_type.to_string_lossy(),
                                )));
                            }
                            // TODO: It's not known if there are other possible error numbers
                            return Err(e);
                        }
                    }
                }
                return Ok(String::new());
            }
        }
    }
}

/// Remount an existing mount point
/// Usage examples: "mount -o remount /home/username/mnt"
struct Remount {
    property: Property,
    target: PathBuf,
    mountflags: Vec<OsString>,
    data: OsString,
}

impl Remount {
    fn new(
        property: Property,
        target: OsString,
        mountflags: Vec<OsString>,
        data: OsString,
    ) -> Self {
        Self {
            property,
            target: PathBuf::from(target),
            mountflags,
            data,
        }
    }
}

impl Mountable for Remount {
    fn run(&mut self) -> MountResult<String> {
        // check privilege
        // getuid() is always successful, so it's ok to use it
        if unsafe { libc::getuid() } != 0 {
            return Err(MountError::PermissionDenied);
        }

        // Go through all the existing mount points, find the appropriate source & target
        let existing_mounts = FSDescFile::new("/proc/mounts")?;
        let mut source = OsStr::new("");
        let mut target = OsStr::new("");
        let mut filesystem_type = OsStr::new("");
        // We need to do this in reverse order
        for item in existing_mounts.entries.iter().rev() {
            if self.target == item.mnt_fsname || self.target == item.mnt_dir {
                source = &item.mnt_fsname;
                target = &item.mnt_dir;
                filesystem_type = &item.mnt_type;
                break;
            }
        }

        // If not found, the mount point is not mounted
        if source == "" || target == "" {
            return Err(MountError::MountPointNotMounted(
                self.target.to_string_lossy().to_string(),
            ));
        }

        let mountflags = Flag::from(&mut self.mountflags)?.flag;

        if !self.property.fake {
            nix::mount::mount(
                Some(source),
                target,
                Some(filesystem_type),
                mountflags,
                Some(self.data.as_os_str()),
            ).or(Err(MountError::from(io::Error::last_os_error())))?
        }

        Ok(String::new())
    }
}

fn create_app() -> App<'static, 'static> {
    util_app!(NAME)
            .author("Zhuohua Li <zhuohuali@baidu.com>")
            .arg(Arg::with_name("arg1")
                .index(1))
            .arg(Arg::with_name("arg2")
                .index(2))
            .arg(Arg::with_name("v") // TODO: not supported yet
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
            .arg(Arg::with_name("n") // TODO: not supported yet
                .short("n")
                .help("mount without writing in /etc/mtab. This is necessary for example when /etc is on a read-only file system."))
            .arg(Arg::with_name("s") // TODO: not supported yet
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
                .hide_possible_values(true))
}

pub fn execute<S, T>(setup: &mut S, args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    let app = create_app();
    let matches = app.get_matches_from_safe(args)?;
    let options = MountOptions::from_matches(&matches)?;

    let output = setup.output();
    let output = output.lock()?;

    let mut mounter = Mounter::new(output);
    mounter.mount(options)?;
    Ok(())
}