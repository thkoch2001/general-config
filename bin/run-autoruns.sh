#!/bin/sh

TEMPLATE=xsession-$USER-$PPID-XXX
TEMPFILE=$(mktemp --tmpdir $TEMPLATE)

echo "logging to $TEMPFILE"

( $HOME/bin/dex -v -a 2>&1 1>$TEMPFILE ) &