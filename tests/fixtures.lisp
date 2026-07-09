;;;; tests/fixtures.lisp --- In-memory sample files for the test-suite.

(in-package #:magic/tests)

(defun bv (&rest parts)
  "Build an (unsigned-byte 8) vector from PARTS, each of which may be an
integer (one byte), a string (its char-codes), or a sequence of bytes."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (labels ((push-byte (b) (vector-push-extend (logand #xff b) out))
             (add (p) (etypecase p
                        (integer (push-byte p))
                        (character (push-byte (char-code p)))
                        (string (loop for c across p do (push-byte (char-code c))))
                        (sequence (map nil #'add p)))))
      (dolist (p parts) (add p)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun be16 (n) (bv (ldb (byte 8 8) n) (ldb (byte 8 0) n)))
(defun be32 (n) (bv (ldb (byte 8 24) n) (ldb (byte 8 16) n)
                    (ldb (byte 8 8) n) (ldb (byte 8 0) n)))
(defun le16 (n) (bv (ldb (byte 8 0) n) (ldb (byte 8 8) n)))
(defun le32 (n) (bv (ldb (byte 8 0) n) (ldb (byte 8 8) n)
                    (ldb (byte 8 16) n) (ldb (byte 8 24) n)))

;;; Minimal but valid-enough headers for each format.

(defun sample-png ()
  (bv #x89 "PNG" #x0d #x0a #x1a #x0a
      (be32 13) "IHDR" (be32 16) (be32 16) 8 2 0 0 0))

(defun sample-gif ()
  (bv "GIF89a" (le16 16) (le16 16) #x00 #x00 #x00 #x3b))

(defun sample-jpeg ()
  (bv #xff #xd8 #xff #xe0 #x00 #x10 "JFIF" #x00 #x01 #x01 #x00
      #x00 #x01 #x00 #x01 #x00 #x00 #xff #xd9))

(defun sample-pdf ()
  (bv "%PDF-1.7" #x0a "1 0 obj" #x0a "<<>>" #x0a "endobj" #x0a))

(defun sample-gzip ()
  ;; 1f 8b, method=deflate(8), no flags, mtime=0, xfl=0, os=3(Unix)
  (bv #x1f #x8b #x08 #x00 #x00 #x00 #x00 #x00 #x00 #x03
      #x2b #x49 #x2d #x2e #x01 #x00 #x00 #x00 #xff #xff))

(defun sample-elf ()
  ;; ELF64 LE executable, x86-64
  (bv #x7f "ELF" 2 1 1 0 0 0 0 0 0 0 0 0 0 0
      (le16 2) (le16 #x3e) (le32 1)))

(defun sample-bmp ()
  ;; Windows 3.x BMP: 40-byte BITMAPINFOHEADER, 16x16, 24bpp
  (bv "BM" (le32 1078) (le16 0) (le16 0) (le32 54)
      (le32 40) (le32 16) (le32 16) (le16 1) (le16 24)
      (le32 0) (le32 0) (le32 0) (le32 0) (le32 0) (le32 0)))

(defun sample-zip ()
  ;; The "empty archive" end-of-central-directory record (PK\5\6 + 18 zeros).
  (bv "PK" #x05 #x06 (make-array 18 :element-type '(unsigned-byte 8) :initial-element 0)))

(defun sample-class ()
  ;; Java class file
  (bv #xca #xfe #xba #xbe (be16 0) (be16 52)))

(defun sample-ascii ()
  (bv "The quick brown fox jumps over the lazy dog." #x0a))
