;;;; tests/tests.lisp

(in-package #:magic/tests)

(def-suite all-tests :description "All magic tests.")
(in-suite all-tests)

;;; ---------------------------------------------------------------------------
;;; Low-level parsing units

(test parse-c-integer
  (is (= 19 (magic::parse-c-integer "19")))
  (is (= 19 (magic::parse-c-integer "0x13")))
  (is (= 11 (magic::parse-c-integer "013")))       ; octal
  (is (= -5 (magic::parse-c-integer "-5")))
  (is (= #xffd8ff00 (magic::parse-c-integer "0xffd8ff00")))
  (is (= 65 (magic::parse-c-integer "'A'"))))

(test decode-string-escapes
  (is (equalp (magic::bytes->buffer '(#x89 #x50 #x4e #x47))
              (magic::decode-string-escapes "\\x89PNG")))
  (is (equalp (magic::bytes->buffer '(0 0 0 13))
              (magic::decode-string-escapes "\\0\\0\\0\\015")))
  (is (equalp (magic::bytes->buffer '(10 9 32))
              (magic::decode-string-escapes "\\n\\t\\ "))))

(test type-lookup
  (is (eq :numeric (magic::mtype-category (magic::lookup-type "belong"))))
  (is (= 4 (magic::mtype-size (magic::lookup-type "belong"))))
  (is (eq :big (magic::mtype-endian (magic::lookup-type "beshort"))))
  (is (eq :string (magic::mtype-category (magic::lookup-type "string"))))
  (is (eq :use (magic::mtype-category (magic::lookup-type "use"))))
  (is (null (magic::lookup-type "nonsense"))))

(test parse-type-token-mask-and-flags
  (let ((e (magic::make-entry)))
    (magic::parse-type-token "belong&0xffffff00" e)
    (is (eq :numeric (magic::mtype-category (magic::ent-type e))))
    (is (= #xffffff00 (magic::ent-mask e)))
    (is (char= #\& (magic::ent-mask-op e))))
  (let ((e (magic::make-entry)))
    (magic::parse-type-token "search/512" e)
    (is (eq :search (magic::mtype-category (magic::ent-type e))))
    (is (= 512 (magic::ent-str-range e))))
  (let ((e (magic::make-entry)))
    (magic::parse-type-token "string/c" e)
    (is (find #\c (magic::ent-str-flags e))))
  (let ((e (magic::make-entry)))
    (magic::parse-type-token "ubelong" e)
    (is (magic::ent-unsigned e))))

(test parse-offset-indirect
  ;; (0x3c.l): indirect, base 0x3c, 4-byte little-endian read
  (let ((o (magic::parse-offset "(0x3c.l)" 1)))
    (is (magic::moff-indirect o))
    (is (= #x3c (magic::moff-base o)))
    (is (= 4 (magic::moff-in-size o)))
    (is (eq :little (magic::moff-in-endian o))))
  ;; relative offset &0
  (let ((o (magic::parse-offset "&0" 1)))
    (is (magic::moff-relative o))
    (is (not (magic::moff-indirect o))))
  ;; (4.s*512): short LE, multiply by 512
  (let ((o (magic::parse-offset "(4.s*512)" 1)))
    (is (magic::moff-indirect o))
    (is (= 2 (magic::moff-in-size o)))
    (is (char= #\* (magic::moff-in-op o)))
    (is (= 512 (magic::moff-disp o)))))

(test parse-magic-source-nesting
  (let ((roots (magic::parse-magic-source
                (format nil "0	string	MZ	DOS executable~%>0x18	leshort	>0x3f	extended~%!:mime	application/x-dosexec"))))
    (is (= 1 (length roots)))
    (let ((root (first roots)))
      (is (= 1 (length (magic::ent-children root))))
      (is (string= "application/x-dosexec"
                   (magic::ent-mime (first (magic::ent-children root))))))))

;;; ---------------------------------------------------------------------------
;;; Formatting and comparison

(test printf-format
  (is (string= "42" (magic::printf-format "%d" 42)))
  (is (string= "007" (magic::printf-format "%03d" 7)))
  (is (string= "ff" (magic::printf-format "%x" 255)))
  (is (string= "1 x 1" (magic::printf-format "%d x 1" 1)))
  (is (string= "hi" (magic::printf-format "%s" "hi")))
  (is (string= "100%" (magic::printf-format "100%%" 0))))

(test numeric-match
  (is-true  (magic::numeric-match-p #\= 5 5 4 nil))
  (is-false (magic::numeric-match-p #\= 5 6 4 nil))
  (is-true  (magic::numeric-match-p #\! 5 6 4 nil))
  (is-true  (magic::numeric-match-p #\> 6 5 4 nil))
  (is-true  (magic::numeric-match-p #\x 0 999 4 nil))
  (is-true  (magic::numeric-match-p #\& #b1110 #b0110 4 nil))
  (is-false (magic::numeric-match-p #\& #b1000 #b0110 4 nil))
  ;; signed comparison: 0xffffffff = -1 < 0
  (is-true  (magic::numeric-match-p #\< #xffffffff 0 4 nil))
  ;; unsigned comparison: 0xffffffff > 0
  (is-true  (magic::numeric-match-p #\> #xffffffff 0 4 t)))

;;; ---------------------------------------------------------------------------
;;; Engine test with a small hand-written database

(test custom-database-roundtrip
  (let ((db (magic::make-database)))
    (magic::database-add-source
     db (format nil "0	string	FOO	Foo container~%!:mime	application/x-foo~%>3	byte	x	version %d"))
    (let* ((buf (bv "FOO" 7))
           (r (magic::acc->result (magic::match-buffer db buf))))
      (is (string= "Foo container version 7" (magic:result-description r)))
      (is (string= "application/x-foo" (magic:result-mime-type r))))
    ;; non-match
    (is (null (magic::match-buffer db (bv "BAR" 1))))))

(test custom-database-indirect-offset
  ;; value at offset 4 (little-endian long) points to a "PE" marker
  (let ((db (magic::make-database)))
    (magic::database-add-source
     db (format nil "0	string	MZ	DOS~%>(4.l)	string	PE	 PE header"))
    (let ((buf (bv "MZ" 0 0 (le32 8) "PE")))  ; offset 4..7 = 8; "PE" at offset 8
      (let ((r (magic::acc->result (magic::match-buffer db buf))))
        (is-true (search "PE header" (magic:result-description r)))))))

;;; ---------------------------------------------------------------------------
;;; End-to-end detection against the vendored file(1) database

(def-suite detection :description "Detection against the real magic database."
  :in all-tests)
(in-suite detection)

(defmacro def-detect-test (name sample &key desc mime)
  `(test ,name
     (let ((r (magic::buffer-match (,sample))))
       (is-true r ,(format nil "~A should be recognised" name))
       (when r
         ,@(when desc
             `((is-true (search ,desc (magic:result-description r) :test #'char-equal)
                        "description ~S should contain ~S"
                        (magic:result-description r) ,desc)))
         ,@(when mime
             `((is (string= ,mime (magic:result-mime-type r)))))))))

(def-detect-test detect-png   sample-png   :desc "PNG image data"  :mime "image/png")
(def-detect-test detect-gif   sample-gif   :desc "GIF image data"  :mime "image/gif")
(def-detect-test detect-jpeg  sample-jpeg  :desc "JPEG image data" :mime "image/jpeg")
(def-detect-test detect-pdf   sample-pdf   :desc "PDF document"    :mime "application/pdf")
(def-detect-test detect-gzip  sample-gzip  :desc "gzip compressed" :mime "application/gzip")
(def-detect-test detect-elf   sample-elf   :desc "ELF")
(def-detect-test detect-bmp   sample-bmp   :desc "PC bitmap")
(def-detect-test detect-zip   sample-zip   :desc "Zip archive")
(def-detect-test detect-class sample-class :desc "class")

(test detect-png-dimensions
  (is (string= "PNG image data, 16 x 16, 8-bit/color RGB, non-interlaced"
               (magic:result-description (magic::buffer-match (sample-png))))))

(test detect-ascii-text
  (let ((r (magic::buffer-match (sample-ascii))))
    (is-true (search "text" (magic:result-description r)))
    (is (string= "text/plain" (magic:result-mime-type r)))))

(test empty-buffer
  (is (null (magic::match-buffer (magic:default-database)
                                 (magic::bytes->buffer #())))))

;;; ---------------------------------------------------------------------------
;;; Pascal strings

(def-suite engine :description "Evaluator features." :in all-tests)
(in-suite engine)

(defun match-desc (source buffer)
  "Parse SOURCE into a fresh database and return the description for BUFFER."
  (let ((db (magic::make-database)))
    (magic::database-add-source db source)
    (let ((acc (magic::match-buffer db buffer)))
      (and acc (magic::acc-description acc)))))

(test pstring-byte-length
  ;; 1-byte length prefix (default B): length 2, content "hi"
  (is (string= "got hi next 9"
               (match-desc (format nil "0	pstring	hi	got %s~%>3	byte	x	next %d")
                           (bv 2 "hi" 9)))))

(test pstring-2byte-be-length
  (is (string= "P ab"
               (match-desc (format nil "0	pstring/H	ab	P %s") (bv 0 2 "ab")))))

(test pstring-length-includes-self
  ;; /J: the length field (4) counts its own 2 prefix bytes, leaving 2 content
  (is (string= "P cd"
               (match-desc (format nil "0	pstring/HJ	cd	P %s") (bv 0 4 "cd")))))

(test pstring-prefix-modifiers
  (multiple-value-bind (size endian incl) (magic::pstring-prefix "H")
    (is (= 2 size)) (is (eq :big endian)) (is (not incl)))
  (multiple-value-bind (size endian incl) (magic::pstring-prefix "lJ")
    (is (= 4 size)) (is (eq :little endian)) (is-true incl))
  (multiple-value-bind (size endian) (magic::pstring-prefix "")
    (is (= 1 size)) (is (eq :big endian))))

;;; ---------------------------------------------------------------------------
;;; String value uses the matched region (the GIF \x01 bug)

(test string-equal-prints-matched-region
  ;; %s must print only the matched bytes, not everything up to the next NUL
  (is (string= "ver 89a!"
               (match-desc (format nil "0	string	9a	ver 8%s!") (bv "9a" 1 0)))))

(test string-relational-and-any
  (is (string= "nonempty: Hi"
               (match-desc (format nil "0	string	>\\0	nonempty: %s") (bv "Hi" 0))))
  (is (null (match-desc (format nil "0	string	>\\0	x") (bv 0 0)))))

;;; ---------------------------------------------------------------------------
;;; Offsets: relative, negative-from-end

(test relative-offset
  (is (string= "ab then cd"
               (match-desc (format nil "0	string	AB	ab~%>&0	string	CD	then cd")
                           (bv "ABCD")))))

(test negative-from-end-offset
  (is (string= "ends with tail"
               (match-desc (format nil "-4	string	TAIL	ends with tail") (bv "xyTAIL")))))

;;; ---------------------------------------------------------------------------
;;; Dates

(test format-magic-date
  (is (string= "1970-01-01 00:00:00" (magic::format-magic-date 0 nil)))
  (is (string= "2009-02-13 23:31:30" (magic::format-magic-date 1234567890 nil))))

(test date-type-end-to-end
  (is (string= "stamp 2009-02-13 23:31:30"
               (match-desc (format nil "0	ledate	x	stamp %s")
                           (le32 1234567890)))))

;;; ---------------------------------------------------------------------------
;;; Regex

(test regex-match
  (is (string= "upper ABC"
               (match-desc (format nil "0	regex	=[A-Z]+	upper %s") (bv "ABCdef")))))

;;; ---------------------------------------------------------------------------
;;; use / name with endianness flip (^name)

(test use-endian-flip
  (let ((buf (bv "MM" #x12 #x34)))
    ;; ^swap flips leshort -> beshort, so 0x1234 matches the big-endian bytes
    (is (search "FLIP"
                (match-desc (format nil "0	name	swap~%>2	leshort	0x1234	FLIP~%0	string	MM	root~%>0	use	^swap")
                            buf)))
    ;; without the flip, the little-endian read is 0x3412 and does not match
    (is (not (search "FLIP"
                     (match-desc (format nil "0	name	swap~%>2	leshort	0x1234	FLIP~%0	string	MM	root~%>0	use	swap")
                                 buf))))))

;;; ---------------------------------------------------------------------------
;;; First-byte fingerprint index

(def-suite indexing :description "Fingerprint index and binary/text split." :in all-tests)
(in-suite indexing)

(defun fp-of (line)
  (magic::entry-fingerprint (magic::parse-magic-line line)))

(test fingerprint-derivation
  (is (equal '(0 . #x46) (fp-of "0	string	FOO	x")))          ; 'F'
  (is (equal '(0 . #x12) (fp-of "0	beshort	0x1234	x")))       ; BE high byte
  (is (equal '(0 . #x34) (fp-of "0	leshort	0x1234	x")))       ; LE low byte
  (is (equal '(4 . #x25) (fp-of "4	string	%PDF	x")))
  ;; no single-byte precondition can be derived from these:
  (is (null (fp-of "0	string/c	foo	x")))                     ; case-insensitive
  (is (null (fp-of "0	belong&0xff	0x12	x")))                 ; masked
  (is (null (fp-of "0	string	>\\0	x")))                       ; relational
  (is (null (fp-of "0	search/10	foo	x")))                    ; search scans a range
  (is (null (fp-of ">(0.l)	string	X	x"))))                   ; indirect offset

(test fingerprint-ok-p
  (let ((buf (bv "FOO")))
    (is-true  (magic::fingerprint-ok-p '(0 . #x46) buf 0))       ; 'F' present
    (is-false (magic::fingerprint-ok-p '(0 . #x47) buf 0))       ; 'G' absent
    (is-false (magic::fingerprint-ok-p '(9 . #x46) buf 0))       ; out of bounds
    (is-true  (magic::fingerprint-ok-p nil buf 0))               ; no precondition
    (is-true  (magic::fingerprint-ok-p '(0 . #x4f) buf 1))))     ; bias shifts 0 -> 'O'

(test text-vs-binary-classification
  (is-true  (magic::entry-text-p (magic::parse-magic-line "0	regex	=[a-z]+	x")))
  (is-true  (magic::entry-text-p (magic::parse-magic-line "0	search/9	hello	x")))
  (is-false (magic::entry-text-p (magic::parse-magic-line "0	search/9	\\x00\\x01	x")))
  (is-false (magic::entry-text-p (magic::parse-magic-line "0	string	hello	x")))
  (is-false (magic::entry-text-p (magic::parse-magic-line "0	belong	1	x"))))

(test buffer-textual-p
  (is-true  (magic::buffer-textual-p (bv "plain text")))
  (is-false (magic::buffer-textual-p (bv "has" 0 "nul")))
  (is-true  (magic::buffer-textual-p (bv 9 10 13 "tabs and newlines"))))

(test database-partition-covers-all
  (let ((db (magic:default-database)))
    (magic::ensure-sorted db)
    (is (= (magic:database-entry-count db)
           (+ (length (magic::database-binary db)) (length (magic::database-text db)))))
    (is (> (length (magic::database-text db)) 100))
    (is (> (length (magic::database-binary db)) 3000))))

(test index-does-not-change-results
  ;; every sample must resolve identically with and without the index
  (let ((db (magic:default-database)))
    (dolist (buf (list (sample-png) (sample-gif) (sample-jpeg) (sample-pdf)
                       (sample-gzip) (sample-elf) (sample-bmp) (sample-zip)
                       (sample-class) (sample-ascii)
                       (bv 1 2 3 4 5 6 7 8 9 10)))
      (let ((with (let ((magic::*fingerprint-index* t)) (magic:buffer-type buf :database db)))
            (without (let ((magic::*fingerprint-index* nil)) (magic:buffer-type buf :database db))))
        (is (string= with without)
            "index changed result: ~S vs ~S" with without)))))

;;; ---------------------------------------------------------------------------
;;; Whole-database parse coverage (regression guard)

(def-suite coverage :description "Parse the entire vendored database." :in all-tests)
(in-suite coverage)

(test parse-all-magdir
  (let ((total 0) (ok 0) (err 0))
    (dolist (f (uiop:directory-files (magic::vendored-magic-directory)))
      (with-open-file (in f :if-does-not-exist nil :external-format :latin-1)
        (when in
          (loop for line = (read-line in nil) while line do
            (let ((tr (string-left-trim '(#\Space #\Tab) line)))
              (when (and (plusp (length tr))
                         (char/= (char tr 0) #\#)
                         (not (and (> (length tr) 1)
                                   (char= (char tr 0) #\!) (char= (char tr 1) #\:))))
                (incf total)
                (handler-case (progn (magic::parse-magic-line line) (incf ok))
                  (error () (incf err)))))))))
    (is (> total 20000) "expected a large database, saw ~A test lines" total)
    (is (< err 30) "too many parse errors: ~A of ~A lines" err total)
    (is (> (/ ok total) 0.99) "parse coverage ~,1F%%" (* 100.0 (/ ok total)))))

(defun run-all () (run! 'all-tests))
