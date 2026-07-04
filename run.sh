#!/bin/sh
# run.sh — launch loom on a start URL/path (default: the bundled home page).
#   ./run.sh                         # bundled home page
#   ./run.sh https://example.com     # a URL
#   ./run.sh path/to/page.html       # a local file
# Requires: SDL2 (brew install sdl2) + a Lisp with Quicklisp + loom/weft on the
# ASDF path (see README).  Cocoa needs the main thread; this sbcl --eval path runs
# loom:run on the process main thread, which is exactly what's required.
exec sbcl --control-stack-size 256 --dynamic-space-size 4096 --eval '(ql:quickload :loom)' \
          --eval "(loom:run :start ${1:+\"$1\"}${1:-nil})" \
          --eval '(uiop:quit 0)'
