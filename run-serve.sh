#!/usr/bin/env bash
# Keep the loom raster service up across restarts — mirrors combat/frpc.sh.
# Run this from the persistent context (the same place frpc.sh is started), NOT from
# a one-off shell that gets reaped:  setsid ./run-serve.sh 9393 </dev/null &>/tmp/loom-serve.log &
cd "$(dirname "$0")" || exit 1
PORT="${1:-9393}"
while true; do
  sbcl --dynamic-space-size 4096 --script inspect/serve.lisp "$PORT"
  echo "[run-serve] serve.lisp exited ($?), restarting in 3s" >&2
  sleep 3
done
