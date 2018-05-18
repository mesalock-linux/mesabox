### Restrictions

The utilities should _never_ panic as they are meant to be embeddable into
other programs.  If a utility panics, it would cause the parent program to fail
as well unless it explicitly catches panics.  Instead of allowing panics,
always use `Result`s and check for propagate errors up the call stack.
