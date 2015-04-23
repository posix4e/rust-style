#!/bin/sh

# This is a little script that copys all working examples from rust_by_example

# TODO: Add more test files.

RUST="rustc";
QUIET="-A unused-variables -A dead-code -A unused-assignments";
RUSTC_NT="$RUST -Z no-trans --test $QUIET";
TARGET_DIR=unformatted

# this must be set to the location of the rust by example examples
SRC_DIR=../../../rust-by-example/

for SRC in `find $SRC_DIR -name "*.rs"`
do
    echo $SRC:

    $RUSTC_NT $SRC 2> /dev/null;
    if [ $? -eq 0 ]; then
        cp $SRC $TARGET_DIR
        echo "  compilation ok... copied"
    else
        echo "  compilation failed"
    fi
done