#!/usr/bin/env bash
# Refresh the vendored copy of file(1)'s source (used to regenerate the magic
# database).  Re-clones upstream, strips the .git dir, and records the commit.
set -euo pipefail

REPO="https://github.com/file/file.git"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DEST="$ROOT/vendor/file"
TMP="$(mktemp -d)"

echo ">> Cloning $REPO ..."
git clone --depth 1 "$REPO" "$TMP/file"

COMMIT="$(git -C "$TMP/file" rev-parse HEAD)"
DATE="$(git -C "$TMP/file" log -1 --format='%ci' | cut -d' ' -f1)"
rm -rf "$TMP/file/.git"

echo ">> Replacing $DEST ..."
rm -rf "$DEST"
mv "$TMP/file" "$DEST"
rm -rf "$TMP"

cat > "$DEST/VENDORED.txt" <<EOF
Vendored copy of the file(1) / libmagic source tree.

Upstream:  $REPO
Commit:    $COMMIT
Date:      $DATE

Only the magic pattern sources under magic/Magdir/ are consumed by this
project; the rest of the tree is kept so the magic database can be regenerated
/ refreshed straight from upstream.

To refresh this copy, run:  scripts/vendor-file.sh
EOF

echo ">> Done. Vendored file at $COMMIT ($DATE)."
echo "   Magdir fragments: $(ls "$DEST/magic/Magdir" | wc -l | tr -d ' ')"
