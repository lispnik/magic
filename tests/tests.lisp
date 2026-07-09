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

(defun run-all () (run! 'all-tests))
