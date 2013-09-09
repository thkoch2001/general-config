#!/bin/sh

COMMAND="xpra attach tcp:192.168.122.80:2345 --encoding=png --window-layout=border"

exec daemon \
  --noconfig \
  --command="$COMMAND" \
  --respawn \
  --attempts=5 \
  --acceptable=10 \
  --delay=10 \
  --limit=0 \
  --name=xpra_launch_loop \
  --output=/tmp/xpra_launch_loop.log