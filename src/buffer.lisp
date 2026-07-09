;;;; buffer.lisp --- Byte-buffer reading with endian awareness.

(in-package #:magic)

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))

(define-condition magic-error (error)
  ((message :initarg :message :reader magic-error-message :initform nil))
  (:report (lambda (c s) (format s "~A" (magic-error-message c)))))

;;; The native byte order of the machine that would run file(1).  Most magic
;;; entries specify an explicit endianness (be*/le*); the bare types
;;; (short/long/quad) use this.
(defvar *native-endian*
  #+little-endian :little
  #+big-endian :big
  #-(or little-endian big-endian) :little
  "Byte order used for the non-prefixed numeric magic types.")

(declaim (inline buffer-length))
(defun buffer-length (buffer)
  (length (the octet-vector buffer)))

(defun read-file-into-buffer (pathname &key (max-bytes nil))
  "Read PATHNAME into a fresh octet-vector.  When MAX-BYTES is supplied only
that many leading bytes are read (file(1) itself only reads a bounded prefix)."
  (with-open-file (in pathname :element-type 'octet :if-does-not-exist :error)
    (let* ((len (file-length in))
           (n (if max-bytes (min len max-bytes) len))
           (buf (make-array n :element-type 'octet)))
      (let ((read (read-sequence buf in)))
        (if (= read n)
            buf
            (subseq buf 0 read))))))

(defun bytes->buffer (sequence)
  "Coerce SEQUENCE (list, string, or vector) into an octet-vector."
  (etypecase sequence
    (octet-vector sequence)
    (string (map 'octet-vector #'char-code sequence))
    (sequence (map 'octet-vector (lambda (x) (logand #xff (if (characterp x) (char-code x) x)))
                   sequence))))

(declaim (inline in-bounds-p))
(defun in-bounds-p (buffer offset size)
  (and (>= offset 0)
       (<= (+ offset size) (buffer-length buffer))))

(defun read-uint (buffer offset size endian)
  "Read an unsigned integer of SIZE bytes from BUFFER at OFFSET using ENDIAN
byte order (:little, :big, :middle, or :native).  Returns NIL if out of range."
  (when (in-bounds-p buffer offset size)
    (let ((endian (if (eq endian :native) *native-endian* endian)))
      (ecase endian
        (:big
         (let ((v 0))
           (dotimes (i size v)
             (setf v (logior (ash v 8) (aref buffer (+ offset i)))))))
        (:little
         (let ((v 0))
           (dotimes (i size v)
             (setf v (logior v (ash (aref buffer (+ offset i)) (* 8 i)))))))
        (:middle
         ;; PDP-11 middle-endian, only meaningful for 4-byte values: the two
         ;; 16-bit halves are little-endian, stored high half first.
         (if (= size 4)
             (let ((b0 (aref buffer offset))
                   (b1 (aref buffer (+ offset 1)))
                   (b2 (aref buffer (+ offset 2)))
                   (b3 (aref buffer (+ offset 3))))
               (logior (ash b1 24) (ash b0 16) (ash b3 8) b2))
             (read-uint buffer offset size :little)))))))

(defun sign-extend (value size)
  "Interpret the SIZE-byte unsigned VALUE as a two's-complement signed integer."
  (let ((bits (* 8 size)))
    (if (logbitp (1- bits) value)
        (- value (ash 1 bits))
        value)))

(defun read-int (buffer offset size endian &key (signed t))
  "Read an integer of SIZE bytes; sign-extend when SIGNED."
  (let ((u (read-uint buffer offset size endian)))
    (when u
      (if signed (sign-extend u size) u))))

(defun decode-ieee (bits exponent-bits mantissa-bits)
  "Decode an IEEE-754 value from its integer BITS representation."
  (let* ((sign (if (logbitp (+ exponent-bits mantissa-bits) bits) -1 1))
         (exponent (ldb (byte exponent-bits mantissa-bits) bits))
         (mantissa (ldb (byte mantissa-bits 0) bits))
         (bias (1- (ash 1 (1- exponent-bits)))))
    (cond
      ((and (= exponent (1- (ash 1 exponent-bits))) (zerop mantissa))
       (* sign #.(expt 10 30)))            ; +/- infinity (approximate)
      ((= exponent (1- (ash 1 exponent-bits)))
       :nan)
      ((zerop exponent)                     ; subnormal
       (* sign (scale-float (float mantissa 1d0) (- (+ bias mantissa-bits -1)))))
      (t
       (* sign (scale-float (float (logior mantissa (ash 1 mantissa-bits)) 1d0)
                            (- exponent bias mantissa-bits)))))))

(defun read-ieee-float (buffer offset size endian)
  "Read a 4- or 8-byte IEEE float from BUFFER."
  (let ((bits (read-uint buffer offset size endian)))
    (when bits
      (ecase size
        (4 (decode-ieee bits 8 23))
        (8 (decode-ieee bits 11 52))))))
