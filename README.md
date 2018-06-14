MesaBox
=======

[![Build Status](https://ci.mesalock-linux.org/api/badges/mesalock-linux/mesabox/status.svg?branch=master)](https://ci.mesalock-linux.org/mesalock-linux/mesabox)
[![License](https://img.shields.io/badge/license-BSD-blue.svg)](LICENSE)

MesaBox is a collection of core system utilities written in Rust for Unix-like
systems.  

Like the well-known [BusyBox][] and [Toybox][] sets of utilities popular on
embedded devices, MesaBox seeks to provide a fully functioning command-line
environment (unlike [uutils][], which just seeks to reimplement the GNU
coreutils).

Completion Status
-----------------

As the project has just begun many utilities have yet to be implemented.  If
something in the table below seems interesting, feel free to take a stab at it.
If it seems like something that should be in the table is missing, make sure to
open an issue.  Take a look at CONTRIBUTING.md for more details.

Utility | Type | Status
:------:|:----:|:------:
arch    | GNU  | **Complete**
base32  | GNU  | **Complete**
base64  | GNU  | **Complete**
yes     | GNU  | **Complete**
tar     | LSB  | Beginning Stages
ping    | Networking | Simple Version
cat     | POSIX/GNU | **Complete**
chmod   | POSIX/GNU | **Mostly Complete** <br/> (missing `--reference`)
head    | POSIX/GNU | **Complete**
init    | POSIX | Simple Version
sh      | POSIX | TODO
sleep   | POSIX | **Complete**

Maintainer
----------

  - Alex Lyon `<alexlyon@baidu.com>` [@Arcterus](https://github.com/Arcterus)
  - Mingshen Sun `<mssun@mesalock-linux.org>` [@mssun](https://github.com/mssun)

[BusyBox]: https://busybox.net/about
[Toybox]: https://landley.net/toybox/about.html
[uutils]: https://github.com/uutils/coreutils

License
-------

MesaBox is provided under the 3-Clause BSD license (please see LICENSE for more
details).
