// NOTE: if this ever changes, io_number in parser.rs must be changed as well
pub const FD_COUNT: usize = 9;

#[derive(Clone, Debug)]
pub enum ShellOption {
    Default,
}
