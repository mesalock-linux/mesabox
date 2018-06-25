#!/bin/sh

KCOV_OPTIONS="--verify --exclude-pattern=/.cargo --exclude-path=./target"
COV_DIR="target/cov"

last=

find_and_exec() {
    for file in target/debug/deps/"$1"-*.d; do
        name="$(echo "$file" | cut -f 1 -d .)"
        output="$("$name" 2>/dev/null)"
        if [ "$?" -eq 0 -a "x$(echo \"$output\" | cut -f 1 -d ' ')" != "xmesabox" ]; then
            kcov --collect-only $KCOV_OPTIONS $COV_DIR $name
            last="$name"
        fi
    done
}

RUSTFLAGS='-C link-dead-code -C codegen-units=1' cargo test --no-run

find_and_exec mesabox
find_and_exec tests

if [ "x$last" != "x" ]; then
    kcov --report-only --coveralls-id="$COVERALLS_TOKEN" $KCOV_OPTIONS $COV_DIR $last
fi
