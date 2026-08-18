; vim: set ft=lisp ts=2 sw=2 et :
; fft.lisp -- small.lisp rebuilt on lithp.lisp:
; plain ANSI CL plus only fn ? let+ o ats, and the
; local (my k) settings. No reader macros, no
; def/def+/!/{} sugar; num/sym split via defmethod.
; (c) 2026 Tim Menzies timm@ieee.org, MIT license.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "lithp.lisp"))           ; macros must exist at compile time too

(defvar *settings*
  '((seed . 1234567891) (p . 2) (bins . 7) (depth . 4)
    (file . "$MOOT/optimize/misc/auto93.csv")))

(defmacro my (k) `(cdr (assoc ',k *settings*)))

(defvar big 1e32)

;;; 1. columns -------------------------------------------------
(defstruct (num (:conc-name) (:constructor num
              (&optional (n 0) (mu 0.0) (m2 0.0))))
  n mu m2)

(defun sym () (o))

(defun sd (i)
  (if (< (n i) 2) 0
      (sqrt (/ (max 0 (m2 i)) (1- (n i))))))

(defun welford (i v &optional (w 1))
  (incf (n i) w)
  (if (< (n i) 1) (num)
      (let ((d (- v (mu i))))
        (incf (mu i) (/ (* w d) (n i)))
        (incf (m2 i) (* w d (- v (mu i)))) i)))

(defun norm (i v)
  (let ((z (/ (- v (mu i)) (+ (sd i) 1e-32))))
    (/ 1 (+ 1 (exp (* -1.7 (max -3 (min 3 z))))))))

(defmethod mix ((i num) j &optional (w 1))
  (let ((m (+ (n i) (* w (n j))))
        (d (- (mu j) (mu i))))
    (if (< m 1) (num)
        (num m
             (/ (+ (* (n i) (mu i)) (* w (n j) (mu j))) m)
             (+ (m2 i) (* w (m2 j))
                (/ (* w d d (n i) (n j)) m))))))

(defmethod mix ((i hash-table) j &optional (w 1))
  (let ((out (o)))
    (maphash (fn (incf (ats out $1 0) $2)) i)
    (maphash (fn (incf (ats out $1 0) (* w $2))) j)
    out))

;;; 2. data ------------------------------------------------------
(defstruct (data (:conc-name) (:constructor %data))
  names x y (goal (o)) (cols (o)) all rows)

(defun col (i at) (ats (cols i) at))

(defmethod add+ ((i num)        v w) (welford i v w))
(defmethod add+ ((i hash-table) v w) (incf (ats i v 0) w) i)

(defun add (i v &optional (w 1)) (if (eq v '?) i (add+ i v w)))

(defun adds (lst &optional (it (num)))
  (dolist (v lst it) (setf it (add it v))))

(defun role (i s at)
  (let ((z (char s (1- (length s)))))
    (setf (ats (cols i) at)
          (if (lower-case-p (char s 0)) (sym) (num)))
    (end! (all i) (col i at))
    (cond ((find z "-+!")
	   (setf (ats (goal i) at) (if (eql z #\+) 1 0))
           (end! (y i) at))
          ((not (eql z #\X)) (end! (x i) at)))))

(defun data (src &aux (i (%data :names (pop src) :rows src)))
  (loop for s across (names i) for at from 0 do (role i s at))
  (dolist (row (rows i) i) (map nil #'add (all i) row)))   ; row is a vector

;;; 3. discretization ------------------------------------------
(defmethod bin ((c num) v) (floor (* (my bins) (norm c v))))
(defmethod bin ((c hash-table) v) v)

(defmethod top ((c num) v old) (max (or old (- big)) v))
(defmethod top ((c hash-table) v old) v)

(defmethod cuts-of ((c num) bins hi at)
  (let ((l (num)))
    (mapcar (fn (setf l (mix l (ats bins $1)))
                (list at (- big) (ats hi $1) l))
            (butlast (sort (keys bins) #'<)))))

(defmethod cuts-of ((c hash-table) bins hi at)
  (mapcar (fn (list at (ats hi $1) (ats hi $1) (ats bins $1)))
          (keys bins)))

(defun cuts-at (c lst ys at)
  (let ((bins (o)) (hi (o)))
    (loop for r in lst for y1 in ys
          for v = (elt r at) unless (eq v '?) do
      (let ((k (bin c v)))
        (setf (ats bins k)
              (add (or (ats bins k) (num)) y1)
              (ats hi k) (top c v (ats hi k)))))
    (cuts-of c bins hi at)))

(defun cuts (i lst y)
  (let ((ys (mapcar y lst)))
    (loop for at in (x i)
          append (cuts-at (col i at) lst ys at))))

;;; 4. grow trees ----------------------------------------------
(defun mink (lst &optional (p (my p)))
  (let ((n (length lst)))
    (expt (/ (loop for x in lst sum (expt (abs x) p)) n)
          (/ 1.0 p))))

(defun disty (i row)
  (mink (mapcar (fn (- (norm (col i $1) (elt row $1))
                       (ats (goal i) $1)))
                (y i))))

(defmethod has ((v symbol) lo hi) t)
(defmethod has ((v string) lo hi) (equal v lo))
(defmethod has ((v number) lo hi) (<= lo v hi))

(defun branch (nd right)
  (o 'at (? nd at) 'lo (? nd lo) 'hi (? nd hi)
     'left (? nd left) 'right right))

(defun splits (i y root)
  (let+ ((enough (expt (length (rows root)) .33))
         (cs (remove-if (fn (<= (n (fourth $1)) enough))
                        (cuts i (rows i) y))))
    (when cs
      (loop for (bit pick) in `((0 ,#'least) (1 ,#'most))
            append
        (let+ (((at lo hi leaf)
                (funcall pick cs (fn (mu (fourth $1)))))
               (no (remove-if (fn (has (elt $1 at) lo hi))
                              (rows i))))
          (when no (list (list bit (o 'at at 'lo lo 'hi hi
                               'left leaf)
                               no))))))))

(defun grows (i y root &optional (d 0))
  (or (when (< d (my depth))
        (loop for (bit nd no) in (splits i y root)
              for kid = (data (cons (names i) no))
              append
          (loop for (bias r) in (grows kid y root (1+ d))
                collect (list (cat bit bias)
                              (branch nd r)))))
      (list (list "" (adds (mapcar y (rows i)))))))

;;; 5. use trees -----------------------------------------------
(defmethod predict ((i num) row) (mu i))

(defmethod predict ((i hash-table) row)
  (predict (if (has (elt row (? i at)) (? i lo) (? i hi))
               (? i left) (? i right))
           row))

(defun err (tr lst y)
  (/ (loop for r in lst sum (abs (- (funcall y r) (predict tr r))))
     (length lst)))

(defun tune (cands lst y)
  (least cands (fn (err $1 lst y))))

(defun rule (i tr)
  (let ((s (elt (names i) (? tr at)))
        (lo (? tr lo)) (hi (? tr hi)))
    (cond ((equal lo hi)  (cat s " == " lo))
          ((= lo (- big)) (cat s " <= " hi))
          (t              (cat s " >= " lo)))))

(defmethod show (i (tr num))
  (prn "~33a leaf  d2h ~,2f n=~d" "" (mu tr) (n tr)))

(defmethod show (i (tr hash-table))
  (let ((l (? tr left)))
    (prn "if ~30a then d2h ~,2f n=~d"
         (rule i tr) (mu l) (n l))
    (show i (? tr right))))

;;; 6. demos ---------------------------------------------------
(defun eg-main ()
  (let+ ((i (data (csv (my file))))
         (y (fn (disty i $1)))
         (ts (mapcar #'second (grows i y i))))
    (show i (tune ts (rows i) y))))

(defun eg-trees ()
  (let+ ((i (data (csv (my file))))
         (y (fn (disty i $1))))
    (loop for (bias tr) in (grows i y i)
          for k from 1 do
      (prn "===== tree ~2d   bias ~5a   err ~,3f ====="
           k bias (err tr (rows i) y))
      (show i tr) (terpri))))

(defun eg-grows (&optional (reps 10) (k 100))
  (let ((all (csv (my file))) (m 0)
        (t0 (get-internal-real-time)))
    (loop repeat reps do
      (let ((i (data (cons (car all) (few (cdr all) k)))))
        (setf m (length (grows i (fn (disty i $1)) i)))))
    (let ((s (/ (- (get-internal-real-time) t0)
                internal-time-units-per-second)))
      (prn "~dx (sample ~d, ~d trees): ~,3f s -> ~,1f ms"
           reps k m s (* 1000 (/ s reps))))))

;;; 7. start ---------------------------------------------------
(cli *settings*)
(setf *seed* (my seed))
(cond ((member "--grows" (args) :test #'equal) (eg-grows))
      ((member "--trees" (args) :test #'equal) (eg-trees))
      (t (eg-main)))
