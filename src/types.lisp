;;;; types.lisp --- The magic type table.

(in-package #:magic)

(defstruct (mtype (:constructor make-mtype (name category size endian signed)))
  "Describes one magic base type.  CATEGORY is one of :numeric :float :date
:string :pstring :search :regex :guid :der :indirect :name :use :default
:clear :offset.  SIZE is the byte width for fixed-size types (NIL otherwise).
ENDIAN is :little :big :middle or :native.  SIGNED is the default signedness."
  name category size endian signed)

(defparameter *type-table* (make-hash-table :test 'equal))

(defun register-type (names category size endian signed)
  (let ((mt (make-mtype (first names) category size endian signed)))
    (dolist (n names)
      (setf (gethash n *type-table*) mt))))

(macrolet ((deftypes (&body specs)
             `(progn ,@(loop for (names category size endian signed) in specs
                             collect `(register-type ',(if (listp names) names (list names))
                                                     ,category ,size ,endian ,signed)))))
  (deftypes
    ;; integers (native)
    (("byte" "dC" "d1" "c")        :numeric 1 :native t)
    (("ubyte" "uC" "u1")           :numeric 1 :native nil)
    (("short" "dS" "d2")           :numeric 2 :native t)
    (("ushort" "uS" "u2")          :numeric 2 :native nil)
    (("long" "dI" "dL" "d4")       :numeric 4 :native t)
    (("ulong" "uI" "uL" "u4")      :numeric 4 :native nil)
    (("quad" "d8" "dQ")            :numeric 8 :native t)
    (("uquad" "u8" "uQ")           :numeric 8 :native nil)
    ;; integers (big endian)
    ("beshort"  :numeric 2 :big t)   ("ubeshort" :numeric 2 :big nil)
    ("belong"   :numeric 4 :big t)   ("ubelong"  :numeric 4 :big nil)
    ("bequad"   :numeric 8 :big t)   ("ubequad"  :numeric 8 :big nil)
    ("beid3"    :numeric 4 :big nil) ("ubeid3"   :numeric 4 :big nil)
    ;; integers (little endian)
    ("leshort"  :numeric 2 :little t)   ("uleshort" :numeric 2 :little nil)
    ("lelong"   :numeric 4 :little t)   ("ulelong"  :numeric 4 :little nil)
    ("lequad"   :numeric 8 :little t)   ("ulequad"  :numeric 8 :little nil)
    ("leid3"    :numeric 4 :little nil) ("uleid3"   :numeric 4 :little nil)
    ;; integers (middle / PDP endian)
    ("melong"   :numeric 4 :middle t)   ("umelong"  :numeric 4 :middle nil)
    ;; floats
    ("float"    :float 4 :native nil)  ("befloat"  :float 4 :big nil)  ("lefloat"  :float 4 :little nil)
    ("double"   :float 8 :native nil)  ("bedouble" :float 8 :big nil)  ("ledouble" :float 8 :little nil)
    ;; dates (4-byte)
    ("date"     :date 4 :native nil)   ("ldate"    :date 4 :native nil)
    ("bedate"   :date 4 :big nil)      ("beldate"  :date 4 :big nil)
    ("ledate"   :date 4 :little nil)   ("leldate"  :date 4 :little nil)
    ("medate"   :date 4 :middle nil)   ("meldate"  :date 4 :middle nil)
    ;; dates (8-byte)
    ("qdate"    :date 8 :native nil)   ("qldate"   :date 8 :native nil)  ("qwdate"   :date 8 :native nil)
    ("beqdate"  :date 8 :big nil)      ("beqldate" :date 8 :big nil)     ("beqwdate" :date 8 :big nil)
    ("leqdate"  :date 8 :little nil)   ("leqldate" :date 8 :little nil)  ("leqwdate" :date 8 :little nil)
    ;; strings and pattern types
    (("string" "s") :string nil nil nil)
    ("pstring"      :pstring nil nil nil)
    ("bestring16"   :string nil :big nil)
    ("lestring16"   :string nil :little nil)
    ("search"       :search nil nil nil)
    ("regex"        :regex nil nil nil)
    ("guid"         :guid 16 :little nil)
    ("leguid"       :guid 16 :little nil)
    ("beguid"       :guid 16 :big nil)
    ("der"          :der nil nil nil)
    ;; control / structural types
    ("indirect"     :indirect nil nil nil)
    ("name"         :name nil nil nil)
    ("use"          :use nil nil nil)
    ("default"      :default nil nil nil)
    ("clear"        :clear nil nil nil)
    ("offset"       :offset 8 :native t)))

(defun lookup-type (name)
  "Return the MTYPE for the base type NAME, or NIL if unknown."
  (gethash name *type-table*))
