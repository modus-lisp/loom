#!/usr/bin/env bash
# compare.sh — geometry diff between weft and Chromium for a URL.
#   Usage: inspect/compare.sh <url> [width]
# Dumps every element's box from both engines (keyed by a structural DOM path) and
# reports the divergences.  Chromium is the reference; JS is off in both so the same
# SSR markup is compared.
set -euo pipefail
URL="${1:?usage: compare.sh <url> [width]}"
W="${2:-1024}"
DIR="$(cd "$(dirname "$0")" && pwd)"
CH="$(mktemp /tmp/cmp-chrome.XXXX.tsv)"
WE="$(mktemp /tmp/cmp-weft.XXXX.tsv)"
trap 'rm -f "$CH" "$WE"' EXIT

echo "comparing $URL @ ${W}px" >&2
node "$DIR/cmp-chrome.js" "$URL" "$W" > "$CH"
CMP_URL="$URL" CMP_W="$W" sbcl --noinform --dynamic-space-size 4096 --non-interactive \
  --load "$DIR/cmp-weft.lisp" 2>/dev/null > "$WE"
python3 "$DIR/cmp-diff.py" "$CH" "$WE" "$W"
