#!/bin/sh

# rustfmt should never ever destroy working source code.
# To test against regressions this shell script tests for various rust source
# files in src/regression_tests/unformatted.

# TODO: Add more test files.

RUST="rustc";
QUIET="-A unused-variables -A dead-code -A unused-assignments";
RUSTC_NT="$RUST -Z no-trans --test $QUIET";
UDIR=src/test_src/unformatted
FDIR=src/test_src/formatted

cargo build;

mkdir $FDIR

for SRC in `ls $UDIR`
do
    echo $SRC:
    target/debug/rustfmt $UDIR/$SRC > "$FDIR/$SRC";

    echo "    unformatted...\c";
    $RUSTC_NT "$UDIR/$SRC" 2> /dev/null;
    if [ $? -eq 0 ]; then
        echo "ok"
    else
        echo "failed"
    fi

    echo "    formatted.....\c";
    $RUSTC_NT "$FDIR/$SRC" 2> /dev/null;
    if [ $? -eq 0 ]; then
        echo "ok"
    else
        echo "failed"
    fi
done

# clean up
rm -rf $FDIR