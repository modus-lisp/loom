#!/usr/bin/env bash
# hn-audit-cron.sh — cron wrapper for the deterministic HN render-quality audit.
# Renders every HN front-page link in weft and Chromium, diffs the geometry, and
# logs the worst offenders to loom-errors.db (kind='render-audit').  No LLM.
#   Install (daily 08:30):
#     (crontab -l; echo '30 8 * * * bash /path/to/loom/inspect/hn-audit-cron.sh >> /tmp/hn-audit.log 2>&1') | crontab -
export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"
cd "$(cd "$(dirname "$0")/.." && pwd)" || exit 1      # this repo, wherever it is checked out
echo "===== hn-audit $(date -u +%FT%TZ) ====="
exec sbcl --dynamic-space-size 4096 --script inspect/hn-audit.lisp
