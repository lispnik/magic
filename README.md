# magic

[![CI](https://github.com/lispnik/magic/actions/workflows/ci.yml/badge.svg)](https://github.com/lispnik/magic/actions/workflows/ci.yml)

A pure **Common Lisp** reimplementation of [`file(1)`](https://github.com/file/file)'s
magic-based content detection. It reads `file`'s own *magic* pattern database
(the human-readable source fragments in `magic/Magdir/`) and evaluates those
patterns against your data to identify file types — no `libmagic`, no FFI.

```lisp
(magic:file-type "/etc/hosts")          ; => "ASCII text"
(magic:mime-type "photo.png")           ; => "image/png"
(magic:describe-file "archive.tar.gz")  ; prints: archive.tar.gz: gzip compressed data ... [application/gzip]
```

```console
$ bin/magic samples/*.png samples/a.pdf
samples/a.png: PNG image data, 1 x 1, 8-bit/color RGB, non-interlaced
samples/a.pdf: PDF document, version 1.4
```

The upstream `file` source tree is **vendored** under `vendor/file/` so the
magic database can be (re)generated straight from `file`'s magic DSL — see
[Re-vendoring](#re-vendoring).

## Requirements

- [SBCL](http://www.sbcl.org/)
- [ocicl](https://github.com/ocicl/ocicl) for dependency management
- Dependencies (installed via ocicl): `cl-ppcre`, and for tests `fiveam`

The project was scaffolded and its dependencies fetched with:

```console
$ ocicl install cl-ppcre fiveam
```

`ocicl setup` must have been run once (it wires ocicl into your `~/.sbclrc`).

## Usage

### From the REPL

```lisp
(asdf:load-system "magic")

;; File-oriented (reads a bounded prefix of the file)
(magic:file-type   #p"/bin/ls")        ; human-readable description
(magic:mime-type   #p"/bin/ls")        ; MIME type
(magic:describe-file #p"/bin/ls")      ; prints a `file`-style line, returns a result

;; Buffer-oriented (identify bytes already in memory)
(magic:buffer-type       #(#x89 80 78 71 13 10 26 10))  ; "PNG image data, ..."
(magic:buffer-mime-type  "%PDF-1.7")                    ; "application/pdf"

;; Rich result object
(let ((r (magic::buffer-match (coerce '(#x1f #x8b 8) '(vector (unsigned-byte 8))))))
  (magic:result-description r)   ; the full description string
  (magic:result-mime-type   r)   ; MIME type, or NIL
  (magic:result-extensions  r)   ; list of extensions, e.g. ("gz")
  (magic:result-strength    r))  ; the matched rule's strength score
```

The database is loaded lazily on first use from the vendored `Magdir` and
cached (`magic:default-database`). You can also build your own:

```lisp
(let ((db (magic:make-database)))
  (magic:load-magic-directory db #p"/usr/share/file/magic/")   ; a directory of fragments
  ;; or a single fragment file:
  (magic:load-magic-file db #p"my-formats.magic")
  (magic:file-type "something.dat" :database db))
```

### Command line

Build the standalone `file`-like driver. The parsed magic database is baked
into the image (via ASDF's `program-op`), so it starts in ~20 ms instead of
re-parsing the ~350 fragments each run:

```console
$ scripts/build-image.sh       # asdf:make -> bin/magic (~50 MB, gitignored)
```

Then use it like `file`:

```console
$ bin/magic FILE...            # print "path: description"
$ bin/magic --mime FILE...     # print "path: mime/type"
```

To run without building an image, load the system and call the driver directly
(~0.8 s of start-up while the database is parsed):

```console
$ sbcl --eval '(asdf:load-system "magic")' \
       --eval '(uiop:quit (magic::run-cli (uiop:command-line-arguments)))' \
       --end-toplevel-options --mime FILE...
```

## How it works

Each line of a magic fragment is a *test*: `offset  type  test  message`,
with `>`-prefixed continuation lines forming a tree of sub-tests. The pipeline:

| Stage | File | Responsibility |
|-------|------|----------------|
| Byte buffer | `src/buffer.lisp` | endian-aware integer/float reads, bounds checks |
| Escapes/printf | `src/escapes.lisp` | C-string escape decoding, printf-style message formatting |
| Type table | `src/types.lisp` | the ~80 magic base types and their sizes/endianness |
| Parser | `src/parser.lisp` | DSL → `entry` trees (offsets, masks, flags, operators, `!:` annotations) |
| Evaluator | `src/evaluator.lisp` | offset resolution, comparisons, `use`/`name`/`indirect` recursion, strength |
| Database | `src/database.lisp` | loading a `Magdir`, named-entry table, strength-ordered matching |
| API | `src/api.lisp` | `file-type` / `mime-type` / `describe-file`, text fallback, CLI |

The parser and evaluator were written against `MAGIC(5)` and cross-checked
against `file`'s own `apprentice.c` / `softmagic.c` (vendored) for the fiddly
parts — indirect-offset arithmetic, the strength formula, `default`/`clear`
continuation semantics, and the `\b` no-space message join.

### Supported

- Offsets: absolute, negative-from-EOF (level 0), relative (`&`), and indirect
  reads `(( x [.,type][op][( ]y ))` with all the documented type letters and
  `+ - * / % & | ^` arithmetic
- Numeric/date/float types in native/BE/LE/middle endianness, signed and
  unsigned, with `& | ^ + - * / %` masks and `~` inversion
- Operators `= != < > & ^ ~ x`
- `string` (incl. `c`/`C` case-insensitivity), `pstring` (with the
  `B/H/h/L/l/J` length-prefix modifiers), `search`, `regex` (via `cl-ppcre`)
- `name`/`use` subroutines (including `^name` endianness flipping) and
  `indirect` re-scans
- `default`/`clear` continuation logic
- `!:mime`, `!:ext`, `!:apple`, `!:strength` annotations
- printf-style message formatting and the `\b` separator rules
- Strength scoring that mirrors `file`'s rule ordering
- A small text-vs-binary fallback (ASCII / ISO-8859 text)

### Performance

Matching mirrors `file`'s two-phase strategy: **binary** tests run first in
strength order, and the ~340 **text** tests (`search`/`regex` with printable
patterns) are tried only if the data looks textual. Each top-level rule also
carries a precomputed **first-byte fingerprint**, so rules whose fixed offset
can't possibly match are skipped before any evaluation. Together these take an
unknown-binary (no-match) identification from ~26 ms to ~0.6 ms, and a typical
match (PNG) to ~0.6 ms — while producing byte-identical results (there is a
regression test asserting the index never changes the outcome).

### Known limitations

Not a byte-for-byte clone of a specific `file` release. In particular:

- `der` (DER certificate) and `guid` value tests are parsed but not evaluated
- Some string flags (`W`, `w`, `T`, `t`, `b`, full-word `f`) are accepted but
  only partially honoured; matching is otherwise exact/case-insensitive
- Text detection is a lightweight heuristic, not `file`'s full encoding
  analysis (no "with CRLF line terminators", charset sniffing, etc.)
- Results track the **vendored** database version, which may differ from the
  `file` binary installed on your system (e.g. `font/ttf` vs `font/sfnt`)

On a sample of real files, MIME output agrees with the system `file` ~86%,
with the remaining differences attributable to database-version drift rather
than engine behaviour.

## Testing

```console
$ sbcl --eval '(asdf:test-system "magic")' --quit
# or
$ sbcl --eval '(asdf:load-system "magic/tests")' \
       --eval '(fiveam:run! (quote magic/tests:all-tests))' --quit
```

The suite (`tests/`, 140+ checks) covers the low-level parser units
(integer/escape/offset parsing, type lookup, masks), printf formatting, numeric
comparison, and hand-written mini-databases exercising the engine: `pstring`
length prefixes, relative and negative-from-EOF offsets, dates, regex, `use`
endianness flipping, the first-byte fingerprint / binary-vs-text split (with an
invariant check that the index never changes results), and end-to-end detection
of PNG/GIF/JPEG/PDF/gzip/ELF/BMP/ZIP/class plus text. A regression test also
parses every file in the vendored database and asserts >99% of lines parse.

## Re-vendoring

To refresh the vendored `file` sources (and therefore the magic database):

```console
$ scripts/vendor-file.sh
```

This re-clones upstream, strips its `.git`, and records the commit in
`vendor/file/VENDORED.txt`. Only `vendor/file/magic/Magdir/` is consumed at
runtime; the rest is kept for provenance and regeneration.

## Layout

```
magic.asd              system + test-system definitions
src/                   library sources (see table above)
tests/                 fiveam suite + in-memory fixtures
bin/magic              built standalone CLI (via scripts/build-image.sh; gitignored)
scripts/vendor-file.sh refresh the vendored magic database
vendor/file/           vendored upstream file(1) source (magic/Magdir/ is used)
```

## License

BSD-2-Clause (matching `file`'s licensing of the magic database it builds on).
