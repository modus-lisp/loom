#!/usr/bin/env bash
# text-audit-run.sh — run the text-layout geometry audit end to end.
#   1. Chromium reference geometry (per-id box + per-line rects) for each test.
#   2. weft geometry for the same tests.
#   3. font-tolerant geometry diff -> ranked bug map.
# Usage: text-audit-run.sh <tests-dir> [width] [--verbose]
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
TESTS="${1:?tests dir}"
WIDTH="${2:-400}"
VERBOSE="${3:-}"
WORK="${TEXT_AUDIT_WORK:-/tmp/text-audit-work}"
rm -rf "$WORK"; mkdir -p "$WORK"

ls "$TESTS"/*.html | sort > "$WORK/files.txt"

echo "== chromium reference =="
node "$HERE/text-audit-chrome.js" "$WORK/files.txt" "$WORK" "$WIDTH" 2>&1 | tail -3

echo "== weft =="
sbcl --dynamic-space-size 4096 --script "$HERE/text-audit.lisp" "$WORK" "$WIDTH" $(cat "$WORK/files.txt") 2>&1 | tail -3

echo "== diff =="
python3 "$HERE/text-audit-diff.py" "$WORK" $VERBOSE
