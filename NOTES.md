### Restrictions

The utilities should _never_ panic as they are meant to be embeddable into
other programs.  If a utility panics, it would cause the parent program to fail
as well unless it explicitly catches panics.  Instead of allowing panics,
always use `Result`s and check for propagate errors up the call stack.

We cannot assume that the current directory of the utility is the directory in
which it should operate.  For example, during testing, we cannot change the
current working directory as doing so would interfere with any tests that are
running in parallel.  As such, a function `util::actual_path()` is provided.
This function accepts the current working directory (as provided in
`UtilSetup`) and prepends the directory to the given path if the given path is
not absolute.  For consistency, even if there are no tests for a given utility,
_this method must be used_.
