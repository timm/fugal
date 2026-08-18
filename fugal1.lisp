; vim: set ft=lisp lispwords+=fn,let+ :
;;;; fugal1.lisp : grow many small trees, keep the best one.
;;;; Rows are vectors. Columns make themselves from what they
;;;; hold: `seen` of a number grows a num (a plist n mu m2 sd,
;;;; Welford), `seen` of anything else grows a sym (a hash of
;;;; counts). A data is rows plus one such summary per column.
;;;; Names say the role: trailing +/- = goal (max/min),
;;;; ! = klass, X = skip.
;;;; y(row) = "distance to heaven": Minkowski gap between the
;;;; normalised goals and their ideal 0/1. Each x column is
;;;; cut into bins (numbers: 7 bins of the sigmoid'd z-score,
;;;; cumulative from the left; symbols: one per value); each
;;;; cut carries a num of the ys under it. A split keeps the
;;;; cut with the least (bit 0) or most (bit 1) mean y and
;;;; recurses on the rest, so depth d gives <= 2^d trees named
;;;; by their bits; `tune` keeps the lowest error. Trees and
;;;; nums are both plists -- (at lo hi left right) for a node,
;;;; (n mu m2 sd) for a leaf -- so a node is what has an `at`.
;;;; Settings are a plist; -x V on the command line sets key x...
(load (merge-pathnames "lib1.lisp" *load-truename*))

(defvar *settings* '(seed 1234567891 p 2 bins 7 depth 4
                     file "$MOOT/optimize/misc/auto93.csv"))
(defmacro my (k) `(? *settings* ',k))            ; (my bins)

;;; ----- columns ---------------------------------------------
;;; nil is the empty column and `seen` returns a new one, so
;;; callers store what they get back: (setf c (seen c v)).
(defun n  (i) (? i 'n  0))
(defun mu (i) (? i 'mu 0))
(defun sd (i) (? i 'sd 0))

(defmethod seen ((i null) &optional (v '?))    ; the first v
  (if (eq v '?) i                              ; says which
      (seen (if (numberp v)                    ; column to make
                (list 'n 0 'mu 0.0 'm2 0.0 'sd 0)
                (o))
            v)))

(defmethod seen ((i cons) &optional (v '?))    ; num: Welford
  (if (eq v '?) i
      (let* ((k (1+ (n i)))
             (d (- v (mu i)))
             (m (+ (mu i) (/ d k)))
             (q (+ (? i 'm2) (* d (- v m)))))
        (list 'n k 'mu m 'm2 q
              'sd (if (< k 2) 0 (sqrt (/ (max 0 q) (1- k))))))))

(defmethod seen ((i hash-table) &optional (v '?))   ; sym: counts
  (unless (eq v '?) (incf (? i v 0)))
  i)

(defun seens (l &optional i)          ; seen all of L
  (dolist (v l i) (setf i (seen i v))))

(defun norm (i v &aux (z (/ (- v (mu i)) (+ (sd i) 1e-32))))
  (/ 1 (+ 1 (exp (* -1.7 (max -3 (min 3 z)))))))

;;; ----- data ------------------------------------------------
(defstruct (data (:conc-name) (:constructor %data))
  names cols x y rows)

(defun col (i at) (aref (cols i) at))

(defun data (src &aux (i (%data :names (pop src) :rows src)))
  (setf (cols i)
        (make-array (length (names i)) :initial-element nil))
  (loop for s across (names i) for at from 0
        for z = (char s (1- (length s)))
        if (find z "-+!")
          collect (cons at (if (eql z #\+) 1 0)) into y
        else if (char/= z #\X) collect at into x
        finally (setf (x i) x (y i) y))
  (dolist (row (rows i) i)
    (loop for v across row for at from 0
          do (setf (aref (cols i) at) (seen (col i at) v)))))

(defun mink (l &optional (p (my p)))
  (expt (/ (loop for x in l sum (expt (abs x) p)) (length l))
        (/ 1.0 p)))

(defun disty (i row)                       ; distance to heaven
  (mink (loop for (at . g) in (y i)
              collect (- (norm (col i at) (elt row at)) g))))

;;; ----- cuts: a cut is a tree node with no `right` yet.
;;; nums: lo=nil, "v <= hi"; syms: lo=hi=v, "v == lo". --------
(defmethod bin ((c cons) v)
  (floor (* (my bins) (norm c v))))
(defmethod bin ((c hash-table) v) v)

(defun cut (at lo hi ys) (list 'at at 'lo lo 'hi hi 'left ys))

(defun has (cut row &aux (v  (elt row (? cut 'at)))
                         (lo (? cut 'lo)) (hi (? cut 'hi)))
  (or (eq v '?) (if lo (equal v lo) (<= v hi))))

(defun bins (c at rows ys &aux (h (o)))    ; k -> (hi . ys)
  (loop for r in rows for y in ys for v = (elt r at)
        unless (eq v '?)
          do (let* ((k (bin c v))
                    (b (or (? h k) (setf (? h k) (cons v nil)))))
               (push y (cdr b))
               (setf (car b) (if (consp c) (max (car b) v) v))))
  h)

(defun cuts-at (c at rows ys &aux (h (bins c at rows ys)))
  (if (consp c)                      ; nums: cumulative from low
      (loop for k in (butlast (sort (keys h) #'<))
            append (cdr (? h k)) into all
            collect (cut at nil (car (? h k)) (seens all)))
      (loop for k in (keys h)          ; syms: one cut per value
            collect (cut at k k (seens (cdr (? h k)))))))

(defun cuts (i rows y &aux (ys (mapcar y rows)))
  (loop for at in (x i) append (cuts-at (col i at) at rows ys)))

;;; ----- grow: split on least/most cut; each choice = a tree --
(defun splits (i y root
               &aux (enough (expt (length (rows root)) .33)))
  (let ((cs (remove-if (fn (<= (n (? $1 'left)) enough))
                       (cuts i (rows i) y))))
    (when cs
      (loop for (bit f) in '((0 least) (1 most))
            for cut = (funcall f cs (fn (mu (? $1 'left))))
            for no  = (remove-if (fn (has cut $1)) (rows i))
            when no collect (list bit cut no)))))

(defun grows (i y root &optional (d 0))
  (or (when (< d (my depth))
        (loop for (bit cut no) in (splits i y root)
              for kid = (data (cons (names i) no))
              append
                (loop for (bias r) in (grows kid y root (1+ d))
                      collect (list (cat bit bias)
                                    (list* 'right r cut)))))
      (list (list "" (seens (mapcar y (rows i)))))))

;;; ----- use: predict, score, choose, print -------------------
(defun predict (tr row)              ; leaves are nums: no `at`
  (if (? tr 'at)
      (predict (? tr (if (has tr row) 'left 'right)) row)
      (mu tr)))

(defun err (tr lst y)
  (/ (loop for r in lst
           sum (abs (- (funcall y r) (predict tr r))))
     (length lst)))

(defun tune (trees lst y) (least trees (fn (err $1 lst y))))

(defun rule (i tr &aux (s  (elt (names i) (? tr 'at)))
                       (lo (? tr 'lo)) (hi (? tr 'hi)))
  (if lo (cat s " == " lo) (cat s " <= " hi)))

(defun show (i tr &aux (l (? tr 'left)))
  (if (? tr 'at)
      (progn (prn "if ~30a then d2h ~,2f n=~d"
                  (rule i tr) (mu l) (n l))
             (show i (? tr 'right)))
      (prn "~33a leaf  d2h ~,2f n=~d" "" (mu tr) (n tr))))
