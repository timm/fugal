; vim: set ft=lisp ts=2 sw=2 et :
; fugal-eg.lisp -- demos/tests for fugal1.lisp. Every demo
; reseeds, prints, then asserts. Run one with --tree etc;
; --all runs everything and exits with the failure count.
;   sbcl --script fugal-eg.lisp --tree -f $MOOT/x.csv -b 5
; (c) 2026 Tim Menzies timm@ieee.org, MIT license.

(load (merge-pathnames "fugal1.lisp" *load-truename*))

;;; ----- runner ------------------------------------------------
(defvar *eg* nil)                    ; alist: "--name" -> fn

(defmacro eg (name doc &body body)
  `(push (list ,name ,doc (lambda () ,@body)) *eg*))

(defun run (name)
  "Reseed, run one demo; nil (not error) if it fails."
  (let ((it (assoc name *eg* :test #'equal)))
    (unless it (prn "no demo ~a" name) (return-from run nil))
    (setf *seed* (my seed))
    (handler-case (progn (funcall (third it)) t)
      (error (e) (prn "FAIL ~a: ~a" name e) nil))))

(defun usage ()
  (prn "sbcl --script fugal-eg.lisp [--demo] [-k v ...]~%")
  (loop for (k v) on *settings* by #'cddr do
    (prn "  -~a  ~10a = ~a" (char (string-downcase k) 0) k v))
  (terpri)
  (dolist (it (reverse *eg*))
    (prn "  ~8a ~a" (first it) (second it))))

;;; ----- helpers ----------------------------------------------
(defun the-data ()  (data (csv (my file))))
(defun the-y (i)    (fn (disty i $1)))

;;; ----- demos ------------------------------------------------
(eg "--csv" "rows and columns of the file"
  (let ((rows (csv (my file))))
    (prn "rows ~a, cols ~a, first ~a"
         (length rows) (length (first rows)) (first rows))
    (assert (> (length rows) 10))))

(eg "--num" "welford mean/sd of a column"
  (let ((n (seens (loop for r in (cdr (csv (my file)))
                        collect (elt r 0)))))
    (prn "n ~a mu ~,2f sd ~,2f" (n n) (mu n) (sd n))
    (assert (and (> (n n) 0) (> (sd n) 0)))))

(eg "--sym" "counts of a symbolic column"
  ;; columns type themselves, so a file of all numbers has no
  ;; syms at all; when that happens, make one here and count it.
  (let+ ((i  (the-data))
         (at (position-if #'hash-table-p (cols i)))
         (s  (if at (col i at) (seens '(a b a c a)))))
    (prn "col ~a: ~a" (if at (elt (names i) at) "(made one)")
         (loop for k in (keys s) collect (list k (? s k))))
    (assert (> (hash-table-count s) 0))))

(eg "--data" "x and y column indexes"
  (let ((i (the-data)))
    (prn "x ~a y ~a" (x i) (y i))
    (assert (and (x i) (y i)))))

(eg "--disty" "rows sorted by distance to heaven"
  (let+ ((i (the-data)) (y (the-y i))
         (rows (sort (copy-list (rows i)) #'<
                     :key y)))
    (prn "best ~a ~,2f" (first rows) (funcall y (first rows)))
    (prn "worst ~a ~,2f"
         (car (last rows)) (funcall y (car (last rows))))
    (assert (<= (funcall y (first rows))
                (funcall y (car (last rows)))))))

(eg "--cuts" "candidate cuts over the x columns"
  (let+ ((i (the-data)) (y (the-y i))
         (cs (cuts i (rows i) y)))
    (dolist (c (subseq cs 0 (min 5 (length cs))))
      (prn "~a n=~a" (rule i c) (n (? c 'left))))
    (prn "... ~a cuts" (length cs))
    (assert (> (length cs) 2))))

(eg "--tree" "best of the grown trees, as rules"
  (let+ ((i (the-data)) (y (the-y i))
         (ts (mapcar #'second (grows i y i)))
         (best (tune ts (rows i) y)))
    (show i best)
    (assert (<= (err best (rows i) y)
                (err (seens (mapcar y (rows i))) (rows i) y)))))

(eg "--trees" "every grown tree, with its bias and error"
  (let+ ((i (the-data)) (y (the-y i)) (k 0))
    (loop for (bias tr) in (grows i y i) do
      (incf k)
      (prn "===== tree ~2d   bias ~5a   err ~,3f ====="
           k bias (err tr (rows i) y))
      (show i tr) (terpri))
    (assert (> k 1))))

(eg "--grows" "time: grow all trees on 10 samples of 100"
  (let ((all (csv (my file))) (m 0)
        (t0 (get-internal-real-time)))
    (loop repeat 10 do
      (let ((i (data (cons (car all) (few (cdr all) 100)))))
        (setf m (length (grows i (the-y i) i)))))
    (let ((s (/ (- (get-internal-real-time) t0)
                internal-time-units-per-second)))
      (prn "10x (sample 100, ~d trees): ~,3f s -> ~,1f ms"
           m s (* 100 s))
      (assert (> m 1)))))

(eg "--all" "run every demo; fail if any of them do"
  (let ((bad 0))
    (dolist (it (reverse *eg*))
      (unless (equal (first it) "--all")
        (prn "~%--- ~a" (first it))
        (unless (run (first it)) (incf bad))))
    (prn "~%failures: ~a" bad)
    (assert (= bad 0))))

;;; ----- start ------------------------------------------------
(setf *settings* (cli *settings*))
(setf *seed* (my seed))
(let ((todo (remove-if-not (fn (assoc $1 *eg* :test #'equal))
                           (args))))
  (if todo
      (let ((bad (count nil (mapcar #'run todo))))
        #+sbcl (sb-ext:exit :code bad))
      (usage)))
