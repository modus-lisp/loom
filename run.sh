#!/bin/sh
# run.sh — launch loom's glass UI (weft served over VNC) on a start URL/path.
#   ./run.sh                              # bundled home page, VNC on :5900
#   ./run.sh https://example.com          # a URL
#   ./run.sh path/to/page.html            # a local file
#   ./run.sh https://example.com 5901     # a URL on a chosen VNC port
# Connect a VNC client to localhost:<port> (default 5900) to browse.  Requires a
# Lisp with Quicklisp + loom/weft/glass on the ASDF path — no SDL, no X.
PORT="${2:-5900}"
if [ -n "$1" ]; then
  exec sbcl --control-stack-size 256 --dynamic-space-size 4096 \
            --eval '(ql:quickload :loom/glass)' \
            --eval "(loom.glass:run-glass :start \"$1\" :port $PORT)" \
            --eval '(uiop:quit 0)'
else
  exec sbcl --control-stack-size 256 --dynamic-space-size 4096 \
            --eval '(ql:quickload :loom/glass)' \
            --eval "(loom.glass:run-glass :port $PORT)" \
            --eval '(uiop:quit 0)'
fi
