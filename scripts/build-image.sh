#!/usr/bin/env bash
# Build a standalone executable with the magic library and the parsed magic
# database baked in, so the CLI starts instantly (no re-parsing the 350+ magic
# fragments per run).
#
# Uses ASDF's program-op (asdf:make) under the hood -- see :build-operation /
# :entry-point in magic.asd -- with a warm-up step so the database is already
# resident in the dumped image.
#
# Output:  bin/magic.bin   (invoked automatically by bin/magic when present)
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

echo ">> Loading system, warming the magic database, and dumping via program-op ..."
sbcl --noinform --disable-ldb --non-interactive \
  --eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system "magic"))' \
  --eval '(progn (format t "~&;; parsing magic database...~%") (magic:default-database) (format t ";; ~A rules resident in image~%" (magic:database-entry-count (magic:default-database))))' \
  --eval '(asdf:operate (quote asdf:program-op) "magic")'

OUT="$ROOT/bin/magic.bin"
chmod +x "$OUT"
echo ">> Built $OUT ($(du -h "$OUT" | cut -f1))"
