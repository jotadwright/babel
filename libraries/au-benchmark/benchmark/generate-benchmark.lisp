(in-package :au-benchmark.benchmark)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate Benchmark ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Benchmark Settings

(defparameter *literals* '(f g h))

(defparameter *arities* '((f . 1) (g . 2) (h . 3)))

(defparameter *variables-1*
  '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

(defparameter *variables-2*
  '(ALPHA BETA GAMMA DELTA EPSILON ZETA ETA THETA IOTA KAPPA
    LAMBDA MU NU XI OMICRON PI RHO SIGMA TAU UPSILON PHI CHI
    PSI OMEGA))

(defparameter *num-literals-per-class*
  '((1 . (5 . 16))
    (2 . (10 . 16))
    (3 . (10 . 16))
    (4 . (15 . 21))
    (5 . (15 . 21))
    (6 . (15 . 23))))

(defparameter *num-variables-per-class*
  '((1 . (5 . 11))
    (2 . (6 . 11))
    (3 . (9 . 16))
    (4 . (9 . 16))
    (5 . (9 . 16))
    (6 . (9 . 19))))

(defparameter *variable-coefficients-per-class*
  '((1 . (0 . 60480))
    (2 . (60480 . 362880))
    (3 . (362880 . 3628799))
    (4 . (3628799 . 17418240))
    (5 . (17418240 . 174182400))
    (6 . (174182400 . 1741824000))))

(defparameter *atom-coefficients-per-class*
  '((1 . (0 . 41472))
    (2 . (41472 . 207360))
    (3 . (207360 . 9072000))
    (4 . (9072000 . 17418240))
    (5 . (17418240 . 174182400))
    (6 . (174182400 . 1741824000))))

;;;; Compute coefficients

;; Compute coefficient
;; C = L2! / (L2 - L1)!
(defun compute-coefficient (L1 L2)
  (assert (>= L2 L1))
  (* (/ (fac! L2)
        (* (fac! (- L2 L1))
           (fac! L1)))
     (fac! L1)))

;; Variable coefficient
;; L1 = set of vars from G1
;; L2 = set of vars from G2
;; constraint: L2 >= L1
(defun variable-coefficient (G1 G2)
  (let ((G1-variables (remove-duplicates (remove-if-not #'variable-p (mappend #'cdr G1))))
        (G2-variables (remove-duplicates (remove-if-not #'variable-p (mappend #'cdr G2)))))
    (if (>= (length G2-variables) (length G1-variables))
      (compute-coefficient (length G1-variables) (length G2-variables))
      (compute-coefficient (length G2-variables) (length G1-variables)))))

;; Atoms coefficient
;; For every configuration Config:
;;   L1 = set of atoms with a configuration Config
;;   L2 = set of atoms with same configuration Config
;; constraint: L2 >= L1

;; Configurations?
;; f(X) -> f-(1)  f has arity 1
;; g(X,Y) -> g-(1,2)  g has arity 2, and the first and second argument are different
;; g(X,X) -> g-(1,1)  g has arity 2, and both arguments are the same
;; h(X,Y,Z) -> h-(1,2,3)   h has arity 3, and all arguments are different
;; h(X,Y,X) -> h-(1,2,1)   h has arity 3, and the first and last argument are the same
;; h(X,Y,Y) -> h-(1,2,2)   h has arity 3, and the second and last argument are the same
;; h(X,X,X) -> h-(1,1,1)   h has arity 3, and all arguments are the same
(defun make-configuration (atom)
  (let* ((unique-args (remove-duplicates (rest atom)))
         (args-indexes
          (loop for arg in unique-args
                for i from 1
                collect (cons arg i)))
         (args-config
          (loop for arg in (rest atom)
                collect (rest (assoc arg args-indexes)))))
    (cons (first atom) args-config)))

(defun get-atoms-with-configuration (G config)
  (loop for atom in G
        for atom-config = (make-configuration atom)
        when (equal atom-config config)
        collect atom))

(defun atoms-coefficient (G1 G2)
  (let* ((unique-configurations-G1
          (remove-duplicates
           (mapcar #'make-configuration G1)
           :test #'equal))
         (coefficients
          ;; what if there are configurations in G2 that do not occur in G1?
          (loop for config in unique-configurations-G1
                for G1-atoms = (get-atoms-with-configuration G1 config)
                for G2-atoms = (get-atoms-with-configuration G2 config)
                if (>= (length G2-atoms) (length G1-atoms))
                collect (compute-coefficient (length G1-atoms) (length G2-atoms))
                else collect (compute-coefficient (length G2-atoms) (length G1-atoms)))))
    (reduce #'* coefficients :initial-value 1)))

;;;; Generate benchmark functions

(defun generate-pair-of-goals-with-constraints (literals-range variables-range var-coeff-range atom-coeff-range)
  "Given the range of literals, range of variables, variable coefficient and
   atom coefficient; generate a pair of goals."
  (let* ((literals-1 (loop repeat (random-from-range (car literals-range) (cdr literals-range))
                           collect (random-elt *literals*)))
         (literals-2 (loop repeat (random-from-range (car literals-range) (cdr literals-range))
                           collect (random-elt *literals*)))
         (number-of-variables (random-from-range (car variables-range) (cdr variables-range)))
         (goal-1
          (remove-duplicates
           (loop with possible-vars = (mapcar #'make-var (subseq *variables-1* 0 number-of-variables))
                 for literal in literals-1
                 for arity = (rest (assoc literal *arities*))
                 for literal-vars = (loop repeat arity collect (random-elt possible-vars))
                 collect (cons literal literal-vars))
           :test #'equal))
         (goal-2
          (remove-duplicates
           (loop with possible-vars = (mapcar #'make-var (subseq *variables-2* 0 number-of-variables))
                 for literal in literals-2
                 for arity = (rest (assoc literal *arities*))
                 for literal-vars = (loop repeat arity collect (random-elt possible-vars))
                 collect (cons literal literal-vars))
           :test #'equal))
         (goal-1-variables (remove-duplicates (mappend #'cdr goal-1)))
         (goal-2-variables (remove-duplicates (mappend #'cdr goal-2)))
         (var-coeff (variable-coefficient goal-1 goal-2))
         (atom-coeff (atoms-coefficient goal-1 goal-2)))
    ;; if the generated goals do not satisfy the variable coefficient
    ;; and the atom coefficient, generate them again...
    (if (and (>= var-coeff (car var-coeff-range))
             (<= var-coeff (cdr var-coeff-range))
             (>= atom-coeff (car atom-coeff-range))
             (<= atom-coeff (cdr atom-coeff-range)))
      (list (sort goal-1 #'< :key #'length) (sort goal-2 #'< :key #'length)
            (length goal-1) (length goal-2)
            (length goal-1-variables) (length goal-2-variables)
            var-coeff atom-coeff)
      (generate-pair-of-goals-with-constraints literals-range variables-range var-coeff-range atom-coeff-range))))

(defun generate-pair-of-goals-from-class (class)
  "Generate a pair of goals for the given class. Use the class to look up the
   constraints on the pair of goals."
  (let* ((literals-range (rest (assoc class *num-literals-per-class*)))
         (variables-range (rest (assoc class *num-variables-per-class*)))
         (var-coeff-range (rest (assoc class *variable-coefficients-per-class*)))
         (atom-coeff-range (rest (assoc class *atom-coefficients-per-class*))))
    (generate-pair-of-goals-with-constraints literals-range variables-range var-coeff-range atom-coeff-range)))

; (generate-pair-of-goals-from-class 1)

(defun generate-goals-from-class (num-pairs-of-goals class)
  (loop repeat num-pairs-of-goals
        for pair-of-goals = (generate-pair-of-goals-from-class class)
        collect pair-of-goals))

(defun generate-benchmark ()
  ;; 1000 pairs of goals, class 1 - 6
  ;; write to a lisp file
  (loop for class from 1 to 6
        for outfile = (merge-pathnames
                       (make-pathname :directory '(:relative :up "data")
                                      :name (format nil "class-~a" class)
                                      :type "lisp")
                       *load-pathname*)
        do (ensure-directories-exist outfile)
           (reset-id-counters)
           (with-open-file (stream outfile :direction :output
                                   :if-exists :supersede)
             (let ((dataset (generate-goals-from-class 1000 class)))
               (loop for pair-of-goals in dataset
                     do (write-line (format nil "~a" pair-of-goals) stream))))))

; (generate-benchmark)

