; vim: set ft=lisp ts=2 sw=2 et :
; lib1.lisp -- lib.lisp with one accessor `?` for plist,
; hash and struct; settings are a plist so cli is trivial.
; Kit (no reader macros):
;   (fn ...)             lambda; args are $1..$9 as used
;   (? x k [d])          getf / gethash / slot-value; setf-able
;   (let+ ((x 1)                    var
;          ((a b) lst)              destructure
;          (f (z) (* z 2))) ...)    local function
;   o keys               hash-as-record
; General utils:
;   cat prn least most
;   thing split csv path
;   args cli
;   rand rint shuffle few
; (c) 2026 Tim Menzies timm@ieee.org, MIT license.

#+sbcl (declaim (sb-ext:muffle-conditions
                  warning style-warning))
(setf *read-default-float-format* 'double-float)

(defvar *seed* 1234567891)

;;; ----- the kit ----------------------------------------------
(defmacro fn (&body b)
  (let ((a (gensym)))
    `(lambda (&rest ,a)
       (declare (ignorable ,a))
       (symbol-macrolet
           (($1 (nth 0 ,a)) ($2 (nth 1 ,a)) ($3 (nth 2 ,a))
            ($4 (nth 3 ,a)) ($5 (nth 4 ,a)) ($6 (nth 5 ,a))
            ($7 (nth 6 ,a)) ($8 (nth 7 ,a)) ($9 (nth 8 ,a)))
         ,@b))))

(defmacro let+ ((b &rest bs) &body body)
  (let ((tail (if bs `((let+ ,bs ,@body)) body)))
    (cond ((consp (first b))
           `(destructuring-bind ,(first b) ,(second b) ,@tail))
          ((cddr b) `(labels ((,(first b) ,@(rest b))) ,@tail))
          (t        `(let ((,(first b) ,(second b))) ,@tail)))))

(defun o (&rest kvs)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k h) v))
    h))

(defun ? (x k &optional d)
  "Get K from X: plist (getf), hash (gethash), else slot.
D is returned when K is absent (plist and hash only)."
  (cond ((listp x)        (getf x k d))
        ((hash-table-p x) (gethash k x d))
        (t                (slot-value x k))))

(defun (setf ?) (v x k &optional d)
  "Set K in X: hash or struct (plists are treated as
immutable; see `cli`, which returns a new one)."
  (declare (ignore d))
  (if (hash-table-p x)
      (setf (gethash k x) v)
      (setf (slot-value x k) v)))

(defun keys (h)
  (loop for k being the hash-keys of h collect k))

(defmacro end! (place x)      ; append x to list in place
  `(setf ,place (nconc ,place (list ,x))))

;;; ----- little things ----------------------------------------
(defun cat (&rest l) (format nil "~{~a~}" l))

(defun prn (f &rest a) (format t "~?~%" f a))

(defun least (l f)
  (reduce (fn (if (<= (funcall f $1) (funcall f $2)) $1 $2))
          l))

(defun most (l f)
  (reduce (fn (if (>= (funcall f $1) (funcall f $2)) $1 $2))
          l))

;;; ----- strings to things ------------------------------------
(defun thing (s)
  (let ((s (string-trim " " s)))
    (cond ((equal s "?")     '?)
          ((equal s "True")  t)
          ((equal s "False") nil)
          (t (multiple-value-bind (x n)
                 (let ((*read-eval* nil))
                   (read-from-string s nil))
               (if (and (numberp x) (= n (length s)))
                   x
                   s))))))

(defun split (s &optional (sep #\,))
  "Split S on SEP; subseq's nil end handles the last piece."
  (loop for a = 0 then (1+ b)
        for b = (position sep s :start a)
        collect (string-trim " " (subseq s a b))
        while b))

(defun getenv (s)
  #+sbcl (sb-ext:posix-getenv s)
  #+clisp (ext:getenv s))

(defun path (s)
  "Expand a leading $MOOT (env, else HOME/gits/moot)."
  (if (and (> (length s) 5) (string= "$MOOT" s :end2 5))
      (concatenate 'string
        (or (getenv "MOOT")
            (concatenate 'string
              (namestring (user-homedir-pathname)) "gits/moot"))
        (subseq s 5))
      s))

(defun csv (file)
  "Read FILE as CSV; skip blank/#-lines; cells via `thing`."
  (with-open-file (s (path file))
    (loop for line = (read-line s nil) while line
          for l = (string-trim '(#\space #\tab #\return) line)
          unless (or (equal l "") (eql (char l 0) #\#))
            collect (map 'vector #'thing (split l)))))

;;; ----- cli ---------------------------------------------------
(defun args ()
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*)

(defun cli (plist)
  "A new plist: PLIST with -x V from the command line
  applied, where x is the first letter of a key (-f file)."
  (loop for (k v) on plist by #'cddr
    collect k collect 
    (or (loop for (f a) on (args)
          when (equal f (cat "-" (char (string-downcase k) 0)))
          return (thing a))
        v)))

;;; ----- random ------------------------------------------------
(defun rand (&optional (n 1))
  (setf *seed* (mod (* 16807 *seed*) 2147483647))
  (* n (/ *seed* 2147483647.0)))

(defun rint (&optional (n 2)) (floor (rand n)))

(defun shuffle (l)
  (let ((v (coerce l 'vector)))
    (loop for i from (1- (length v)) downto 1
          do (rotatef (aref v i) (aref v (rint (1+ i)))))
    (coerce v 'list)))

(defun few (l n) (subseq (shuffle l) 0 n))
