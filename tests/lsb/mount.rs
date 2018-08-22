//
// Copyright (c) 2018, The MesaLock Linux Project Contributors
// All rights reserved.
//
// This work is licensed under the terms of the BSD 3-Clause License.
// For a copy, see the LICENSE file.
//

// NOTE: some ignored tests need root privilege, and you must run these tests in serial
// use command `sudo cargo test root_test_mount -- --ignored --test-threads 1`

use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::fs;
use std::io::{BufRead, BufReader, Read};
use std::process::Command;

const NAME: &str = "mount";

struct MntEnt {
    mnt_fsname: String,
    mnt_dir: String,
    mnt_type: String,
    mnt_opts: String,
}

#[test]
fn test_mount_no_arg() {
    let file = fs::File::open("/proc/mounts").unwrap();
    let mut list = Vec::new();
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        match line.chars().next() {
            None | Some('#') => continue,
            Some(_) => {}
        }
        let mut iter = line.split_whitespace();
        let mnt_fsname = iter.next().unwrap();
        let mnt_dir = iter.next().unwrap();
        let mnt_type = iter.next().unwrap();
        let mnt_opts = iter.next().unwrap();
        let mnt = MntEnt {
            mnt_fsname: String::from(mnt_fsname),
            mnt_dir: String::from(mnt_dir),
            mnt_type: String::from(mnt_type),
            mnt_opts: String::from(mnt_opts),
        };
        list.push(mnt)
    }

    let mut output = String::new();
    for item in &list {
        if item.mnt_type != "ext4" {
            continue;
        }
        let s = format!(
            "{} on {} type {} ({})",
            item.mnt_fsname, item.mnt_dir, item.mnt_type, item.mnt_opts
        );
        output.push_str((s + "\n").as_str());
    }
    new_cmd!()
        .args(&["-t", "ext4"])
        .assert()
        .success()
        .stdout(predicate::str::contains(output).from_utf8())
        .stderr("");
}

#[test]
fn test_mount_without_root() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&["/dev/loop0", "mnt"])
        .assert()
        .failure()
        .stdout("")
        .stderr("mount: Only root can do that\n");
}

#[test]
#[ignore]
fn root_test_mount_nonexistent_dev() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&["/dev/this_device_should_not_exist", "mnt"])
        .assert()
        .failure()
        .stdout("")
        .stderr(
            predicate::str::contains(
                "special device /dev/this_device_should_not_exist does not exist.",
            ).from_utf8(),
        );
}

#[test]
#[ignore]
fn root_test_mount_nonexistent_mount_point() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&["/dev/loop0", "this_target_should_not_exist"])
        .assert()
        .failure()
        .stdout("")
        .stderr(predicate::str::contains("mount point does not exist.").from_utf8());
}

#[test]
#[ignore]
fn root_test_mount_unknown_filesystem_type() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&[
            "-t",
            "this_filesystem_type_should_not_exist",
            "/dev/loop0",
            "mnt",
        ])
        .assert()
        .failure()
        .stdout("")
        .stderr(
            predicate::str::contains(
                "unknown filesystem type 'this_filesystem_type_should_not_exist'.",
            ).from_utf8(),
        );
}

#[test]
#[ignore]
fn root_test_mount_unknown_uuid() {
    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&["-U", "this_uuid_should_not_exist", "mnt"])
        .assert()
        .failure()
        .stdout("")
        .stderr(
            predicate::str::contains("Cannot find UUID=\"this_uuid_should_not_exist\"").from_utf8(),
        );
}

#[test]
#[ignore]
fn root_test_mount_create_mount_point() {
    Command::new("losetup")
        .current_dir(fixtures_dir!())
        .args(&["/dev/loop0", "dev.img"])
        .assert()
        .success()
        .stdout("")
        .stderr("");

    new_cmd!()
        .current_dir(fixtures_dir!())
        .args(&["/dev/loop0", "mnt"])
        .assert()
        .success();

    let mut path = fixtures_dir!();
    path.push("mnt/read_file_from_device.txt");
    let mut file = fs::File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    assert_eq!(contents, "Hello World!\n");
    drop(file);

    Command::new("umount")
        .current_dir(fixtures_dir!())
        .args(&["mnt"])
        .assert()
        .success()
        .stdout("")
        .stderr("");

    Command::new("losetup")
        .current_dir(fixtures_dir!())
        .args(&["-d", "/dev/loop0"])
        .assert()
        .success()
        .stdout("")
        .stderr("");
}

#[test]
#[ignore]
fn root_test_mount_remount() {
    Command::new("losetup")
        .current_dir(fixtures_dir!())
        .args(&["/dev/loop0", "dev.img"])
        .assert()
        .success()
        .stdout("")
        .stderr("");

    new_cmd!()
            .current_dir(fixtures_dir!())
            // mount it as read-write
            .args(&["-o", "rw", "/dev/loop0", "mnt"])
            .assert()
            .success();

    new_cmd!()
            .current_dir(fixtures_dir!())
            // then remount it as read-only
            .args(&["-o", "remount,ro", "/dev/loop0", "mnt"])
            .assert()
            .success();

    let mut path = fixtures_dir!();
    path.push("mnt/create_new_file.txt");
    match fs::File::create(path) {
        // This operation should not succeed because it's read-only
        Ok(_) => assert!(false),
        // OS error 30 means "Read-only file system"
        Err(e) => assert_eq!(e.raw_os_error(), Some(30)),
    }

    Command::new("umount")
        .current_dir(fixtures_dir!())
        .args(&["mnt"])
        .assert()
        .success()
        .stdout("")
        .stderr("");

    Command::new("losetup")
        .current_dir(fixtures_dir!())
        .args(&["-d", "/dev/loop0"])
        .assert()
        .success()
        .stdout("")
        .stderr("");
}
