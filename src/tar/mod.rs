//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
// 
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

extern crate tar;

use super::{ArgsIter, UtilSetup, Result, UtilRead, UtilWrite};

use clap::{Arg, ArgGroup, OsValues};
use globset::{GlobSetBuilder, Glob};
//use regex::bytes::RegexSet;
use std::ffi::{OsString, OsStr};
use std::fs::File;
use std::io::{BufReader, BufWriter, Read};
use std::path::Path;

pub(crate) const DESCRIPTION: &str = "Manage archives using the tar format";

#[derive(PartialEq)]
enum Mode {
    None,
    Create,
    Append,
    List,
    Update,
    Extract,
}

impl Default for Mode {
    fn default() -> Mode {
        Mode::None
    }
}

#[derive(PartialEq)]
enum Compression {
    None,
    Bzip2,
    Compress,
    Gzip,
    Lzip,
    Lzma,
    Lzop,
    Xz,
}

impl Default for Compression {
    fn default() -> Compression {
        Compression::None
    }
}

enum ArchiveFormat {
    Gnu,
    V7,
    Ustar,
    // FIXME: only supports reading right now
    Posix,
}

impl Default for ArchiveFormat {
    fn default() -> ArchiveFormat {
        ArchiveFormat::Gnu
    }
}

#[derive(Default)]
struct Options<'a> {
    pub mode: Mode,
    pub compression: Compression,
    pub format: ArchiveFormat,
    pub file: Option<&'a OsStr>,

    // extra parameters passed to the command
    pub values: Option<OsValues<'a>>,
}

struct Tar<'a, 'b, I, O, E>
where
    I: UtilRead + 'a,
    O: UtilWrite + 'a,
    E: UtilWrite + 'a,
{
    setup: &'a mut UtilSetup<I, O, E>,
    pub options: Options<'b>,
}

impl<'a, 'b, I, O, E> Tar<'a, 'b, I, O, E>
where
    I: UtilRead,
    O: UtilWrite,
    E: UtilWrite,
{
    pub fn new(setup: &'a mut UtilSetup<I, O, E>, options: Options<'b>) -> Self {
        Self {
            setup: setup,
            options: options,
        }
    }

    pub fn create_archive(&mut self) -> Result<()> {
        // this is fine because -c requires -f
        let path = Path::new(self.options.file.unwrap());
        let file = File::create(path)?;

        let mut builder = tar::Builder::new(BufWriter::new(file));

        let header_type = match self.options.format {
            ArchiveFormat::Gnu => tar::Header::new_gnu(),
            ArchiveFormat::V7 => tar::Header::new_old(),
            ArchiveFormat::Ustar => tar::Header::new_ustar(),
            // FIXME: somehow implement pax
            _ => unimplemented!()
        };

        // FIXME: this should probably fail if self.options.values is None
        if let Some(ref mut values) = self.options.values {
            for value in values {
                let mut header = header_type.clone();
                // FIXME: probably need to check the paths to make sure they are valid/safe and such
                //let path = Path::new(value);
                //if path.is
                builder.append_path(value)?;
            }
        }

        builder.finish()?;

        Ok(())
    }

    pub fn list_contents(&mut self) -> Result<()> {
        match self.options.file {
            Some(filepath) => {
                let file = File::open(filepath)?;
                let archive = tar::Archive::new(BufReader::new(file));
                Self::list_contents_helper(&mut self.setup.stdout, &mut self.options.values, archive)
            }
            _ => {
                let archive = tar::Archive::new(&mut self.setup.stdin);
                Self::list_contents_helper(&mut self.setup.stdout, &mut self.options.values, archive)
            }
        }
    }

    fn list_contents_helper<R: Read>(output: &mut O, values: &mut Option<OsValues<'b>>, mut archive: tar::Archive<R>) -> Result<()> {
        use std::os::unix::ffi::OsStrExt;
        use std::str;
        // XXX: this is not complete, need to handle options and such

        let globset = match values {
            Some(ref mut vals) => {
                let mut set = GlobSetBuilder::new();
                for val in vals {
                    set.add(Glob::new(str::from_utf8(val.as_bytes())?)?);
                }
                Some(set.build()?)
            }
            None => None
        };
        for entry in archive.entries()? {
            let entry = entry?;
            let path = entry.path()?;
            if globset.is_none() || globset.as_ref().unwrap().is_match(&path) {
                writeln!(output, "{}", path.display())?
            }
        }

        Ok(())
    }

    pub fn append_data(&mut self) -> Result<()> {


        Ok(())
    }
}

pub fn execute<I, O, E, T, U>(setup: &mut UtilSetup<I, O, E>, args: ArgsIter<T, U>) -> super::Result<()>
where
    I: UtilRead,
    O: UtilWrite,
    E: UtilWrite,
    T: Iterator<Item = U>,
    U: Into<OsString> + Clone,
{
    // TODO: figure out how to support bundled syntax
    let mut app = util_app!("tar")
                    // operations
                    .group(ArgGroup::with_name("mode")
                            .args(&["create", "append", "list", "update", "extract"])
                            .required(true))
                    .arg(Arg::with_name("create")
                            .short("c")
                            .help("create a new archive containing the given items"))
                    .arg(Arg::with_name("append")
                            .short("r")
                            .requires("file")
                            .help("append new entries to the archive specified by -f"))
                    .arg(Arg::with_name("list")
                            .short("t")
                            .help("list archive contents to stdout"))
                    .arg(Arg::with_name("update")
                            .short("u")
                            .requires("file")
                            .help("update entries in the archive if they have more recent
                                    modification dates than the corresponding entries in the
                                    archive"))
                    .arg(Arg::with_name("extract")
                            .short("x")
                            .help("extract entries from the archive to the disk"))
                    // SUSv2 options (the above and b, f, l, m, o, v, and w)
                    .arg(Arg::with_name("file")
                            .short("f")
                            .takes_value(true)
                            .value_name("FILE")
                            .help("read or write to FILE"))
                    .arg(Arg::with_name("blocksize")
                            .short("b")
                            .takes_value(true)
                            .value_name("BLOCKSIZE")
                            .help("Specify the block size in 512-byte records"))
                    .arg(Arg::with_name("check-links")
                            .short("l")
                            .long("check-links")
                            .help("Display a warning unless all the hard links referencing a file are archived"))
                    .arg(Arg::with_name("modtime")
                            .short("m")
                            .help("Do not extract modification time"))
                    // FIXME: bsdtar defines two -o options, one for creating archives and one for extracting
                    .arg(Arg::with_name("verbose")
                            .short("v")
                            .help("Produce verbose output"))
                    .arg(Arg::with_name("confirm")
                            .short("w")
                            .help("Ask for confirmation for every action"))
                    // common extras
                    .arg(Arg::with_name("follow-links")
                            .short("H")
                            .help("For any symlinks given on the command-line, archive the file the symlink points to instead of the symlink itself"))
                    .arg(Arg::with_name("gzip")
                            .short("z")
                            .long("gzip")
                            // needs an alias for --ungzip
                            .help("use gzip for compression/decompression"))
                    .arg(Arg::with_name("bzip2")
                            .short("j")
                            // might want an alias for -y
                            .long("bzip2")
                            .help("use bzip2 for compression/decompression"))
                    .arg(Arg::with_name("xz")
                            .short("J")
                            .long("xz")
                            .help("use xz for compression/decompression"))
                    .arg(Arg::with_name("compress")
                            .short("Z")
                            .long("compress")
                            // needs an alias for --uncompress
                            .help("use compress for compression/decompression"))
                    // XXX: not sure if we care enough about these other formats to add the code for them
                    .arg(Arg::with_name("lzip")
                            .long("lzip")
                            .help("use lzip for compression/decompression"))
                    .arg(Arg::with_name("lzma")
                            .long("lzma")
                            .help("use lzma for compression/decompression"))
                    .arg(Arg::with_name("lzop")
                            .long("lzop")
                            .help("use lzop for compression/decompression"))
                    .arg(Arg::with_name("format")
                            .long("format")
                            // XXX: posix only works for read i believe (which unfortunately are the ones that don't this specified)
                            .conflicts_with_all(&["list", "extract"])
                            .possible_values(&["gnu", "v7", "ustar", "posix"])
                            .help("Use the specified format for the archive"))
                    .arg(Arg::with_name("FILES | PATTERNS").index(1).multiple(true));

    let matches = get_matches!(setup, app, args);

    let mut options = Options::default();

    options.file = matches.value_of_os("file");

    options.mode = if matches.is_present("create") {
        Mode::Create
    } else if matches.is_present("append") {
        Mode::Append
    } else if matches.is_present("list") {
        Mode::List
    } else if matches.is_present("update") {
        Mode::Update
    } else if matches.is_present("extract") {
        Mode::Extract
    } else {
        // this should never happen due to the ArgGroup above
        unreachable!();
    };

    options.compression = if matches.is_present("gzip") {
        Compression::Gzip
    } else if matches.is_present("bzip2") {
        Compression::Bzip2
    } else if matches.is_present("xz") {
        Compression::Xz
    } else if matches.is_present("lzip") {
        Compression::Lzip
    } else if matches.is_present("lzma") {
        Compression::Lzma
    } else if matches.is_present("lzop") {
        Compression::Lzop
    } else if matches.is_present("compress") {
        Compression::Compress
    } else {
        Compression::None
    };

    options.format = matches.value_of("format").map(|fmt| match fmt {
        "gnu" => ArchiveFormat::Gnu,
        "v7" => ArchiveFormat::V7,
        "ustar" => ArchiveFormat::Ustar,
        "posix" => ArchiveFormat::Posix,
        // the above are the only possible values
        _ => unreachable!()
    }).unwrap_or(ArchiveFormat::Gnu);

    options.values = matches.values_of_os("FILES | PATTERNS");

    let mut util = Tar::new(setup, options);

    match util.options.mode {
        Mode::Create => {
            util.create_archive()?;
        }
        Mode::List => {
            util.list_contents()?;
        }
        Mode::Append => {
            util.append_data()?;
        }
        _ => {}
    }

    Ok(())
}
