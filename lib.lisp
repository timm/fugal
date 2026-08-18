; vim: set ft=lisp ts=2 sw=2 et :
; lithp.lisp -- the pay-rent subset of tiny.lisp.
; Kit (no reader macros):
;   (fn ...)             lambda; args are $1..$9 as used
;   (? x a b)            quoted-key chained access
;   (let+ ((x 1)                    var
;          ((a b) lst)              destructure
;          (f (z) (* z 2))) ...)    local function
;   o ats keys           hash-as-record (structs too)
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

(defmacro ? (x k &rest ks)
  (if ks `(? (ats ,x ',k) ,@ks) `(ats ,x ',k)))

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

(defun ats (x k &optional d)
  (if (hash-table-p x) (gethash k x d) (slot-value x k)))

(defun (setf ats) (v x k &optional d)
  (declare (ignore d))
  (if (hash-table-p x)
      (setf (gethash k x) v)
      (setf (slot-value x k) v)))

(defun keys (h)
  (loop for k being the hash-keys of h collect k))

(defmacro end! (place x)        ; append x to tail of list in place
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

(defun cli (lst)
  (loop for (f v) on (args) do
    (dolist (kv lst)
      (when (equal f (cat "-" (char (string-downcase
                                      (car kv)) 0)))
        (setf (cdr kv) (thing v))))))

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
