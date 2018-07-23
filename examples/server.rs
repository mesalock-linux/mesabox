extern crate mesabox;

use mesabox::UtilData;
use std::io::Write;
use std::iter;

use std::net::TcpListener;

fn main() {
    let listener = TcpListener::bind("127.0.0.1:9876").unwrap();

    for stream in listener.incoming() {
        let mut stream = stream.unwrap();

        let mut stdout = stream.try_clone().unwrap();
        let mut stderr = stream.try_clone().unwrap();

        let res = {
            let mut setup =
                UtilData::new(&mut stream, &mut stdout, &mut stderr, iter::empty(), None);

            mesabox::execute(&mut setup, &mut ["head", "-n", "4"].into_iter())
        };
        if let Err(f) = res {
            let _ = writeln!(stderr, "{}", f);
        }
    }
}
