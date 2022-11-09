(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

(defun test-perfect ()
  "The same network as the simulation environment's solution."
  (let* ((solutions (evaluate "applications\\muhai-cookingbot\\evaluation\\tests\\test-perfect.lisp"))
         (perfection (loop for solution in solutions
                             always (and (= (subgoals-ratio solution) 1)
                                         (= (dish-score solution) 1)
                                         (= (time-ratio solution) 1)))))
    (if perfection
      (print "test-perfect: SUCCESS")
      (error "test-perfect: FAILURE"))))
     
(defun test-permuted-perfect ()
  "The same network as the simulation environment's solution, but with some parts executed in a different order."
  (let ((solution (first (evaluate "applications\\muhai-cookingbot\\evaluation\\tests\\test-permuted-perfect.lisp" (list *almond-crescent-cookies-environment*)))))
    (if (and (= (subgoals-ratio solution) 1)
             (= (dish-score solution) 1)
             (= (time-ratio solution) 1))
      (print "test-permuted-perfect: SUCCESS")
      (error "test-permuted-perfect: FAILURE"))))

(defun test-imperfect ()
  "The same network as the simulation environment's solution, but with some instructions missing at the end."
  (let ((solution (first (evaluate "applications\\muhai-cookingbot\\evaluation\\tests\\test-imperfect.lisp"  (list *almond-crescent-cookies-environment*)))))
    (if (and (= (subgoals-ratio solution) (/ 23 24))
             (< (dish-score solution) 1))
      (print "test-imperfect: SUCCESS")
      (error "test-imperfect: FAILURE"))))

(defun test-extra-operations ()
  "The same network as the simulation environment's solution, but with some extra instructions at the end."
  (let ((solution (first (evaluate "applications\\muhai-cookingbot\\evaluation\\tests\\test-extra-operation.lisp"  (list *almond-crescent-cookies-environment*)))))
    (if (and (= (subgoals-ratio solution) 1)
             (= (dish-score solution) 1)
             (> (time-ratio solution) 1))
      (print "test-extra-operations: SUCCESS")
      (error "test-extra-operations: FAILURE"))))

(defun test-empty ()
  "A solution that only contains get-kitchen."
  (let ((solution (first (evaluate "applications\\muhai-cookingbot\\evaluation\\tests\\test-empty.lisp"  (list *almond-crescent-cookies-environment*)))))
    (if (and (= (subgoals-ratio solution) 0)
             (= (dish-score solution) 0))
      (print "test-empty: SUCCESS")
      (error "test-empty: FAILURE"))))

(defun test-multiple-recipes ()
  "A file that contains two solutions, an imperfect one and a perfect one."
  (let* ((solutions (evaluate "applications\\muhai-cookingbot\\evaluation\\tests\\test-multiple-recipes.lisp"  (list *almond-crescent-cookies-environment* *afghan-biscuits-environment*)))
         (solution-perfect (first solutions))
         (solution-imperfect (second solutions)))
    (if (and (= (subgoals-ratio solution-perfect) 1)
             (= (dish-score solution-perfect) 1)
             (= (subgoals-ratio solution-imperfect) (/ 23 24))
             (< (dish-score solution-imperfect) 1))
      (print "test-multiple-recipes: SUCCESS")
      (error "test-multiple-recipes: FAILURE"))))

(defun execute-all-tests ()
  (test-perfect)
  (test-permuted-perfect)
  (test-imperfect)
  (test-extra-operations)
  (test-empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience Functions (Removable) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defparameter test (evaluate "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\test.lisp"))
;  (evaluate "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\test.lisp")

(defun print-results (solutions)
  "Convenience Function that prints the measurement results of the given solutions."
  (loop for solution in solutions
        do (print "SOLUTION:")
           (print (recipe-id solution))
           (print "Percentage of Reached Subgoals:")
           (print (subgoals-ratio solution))
           (print "Dish Score:")
           (print (dish-score solution))
           (print "Time Ratio:")
           (print (time-ratio solution))))

; TODO RD: get node at maximum depth that was reached if no solution could be found?
; TODO RD: final value zou altijd een container met een ingredient in moeten zijn
; TODO RD: currently unused
(defun get-final-value (irl-node)
  (let* ((target-var (second (irl::primitive-under-evaluation irl-node)))
         (list-of-bindings (irl::bindings irl-node))
         (target-binding (find target-var list-of-bindings :key #'var)))
    (when target-binding
      (value target-binding))))

;test

;(defun testos (x) (print (time-ratio x)))
;(testos (first test))

;(print-results test)

(defun toppie (x)
  (print "ok"))

;(toppie test)

