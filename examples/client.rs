use std::io::{Read, Write};
use std::net::TcpStream;

const FILE_DATA: &str = r#"
            this is a collection of text
this should be printed
    so should this
but this shouldn't
more stuff down here




stuff
"#;

fn main() {
    let mut stream = TcpStream::connect("127.0.0.1:9876").unwrap();

    writeln!(stream, "{}", FILE_DATA).unwrap();

    let mut result = String::new();
    stream.read_to_string(&mut result).unwrap();

    println!("result from server: {}", result);
}
