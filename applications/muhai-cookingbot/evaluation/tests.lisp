(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

(defun test-perfect ()
  "The same network as the simulation environment's solution."
  (let* ((solutions (evaluate-solutions "applications\\muhai-cookingbot\\evaluation\\tests\\test-perfect.solution" '(goal-condition-success dish-approximation-score execution-time)))
         (perfection (loop for solution in solutions
                             always (and ; (= (smatch-score solution) 1)
                                         (= (subgoals-ratio solution) 1)
                                         (= (dish-score solution) 1)))))
    (if perfection
      (print "test-perfect: SUCCESS")
      (error "test-perfect: FAILURE"))))
     
(defun test-permuted-perfect ()
  "The same network as the simulation environment's solution, but with some parts executed in a different order."
  (let ((solution (first (evaluate-solutions "applications\\muhai-cookingbot\\evaluation\\tests\\test-permuted-perfect.solution" *metrics* (list *almond-crescent-cookies-environment*)))))
    (if (and (= (subgoals-ratio solution) 1)
             (= (dish-score solution) 1)
           ;  (< (smatch-score solution) 1)
             (= (execution-time solution) (execution-time *almond-crescent-cookies-environment*)))
      (print "test-permuted-perfect: SUCCESS")
      (error "test-permuted-perfect: FAILURE"))))

(defun test-imperfect ()
  "The same network as the simulation environment's solution, but with some instructions missing at the end."
  (let ((solution (first (evaluate-solutions "applications\\muhai-cookingbot\\evaluation\\tests\\test-imperfect.solution" *metrics* (list *almond-crescent-cookies-environment*)))))
    (if (and (= (subgoals-ratio solution) (/ 25 26))
          ;   (< (smatch-score solution) 1)
             (< (dish-score solution) 1)
             (<= (execution-time solution) (execution-time *almond-crescent-cookies-environment*)))
      (print "test-imperfect: SUCCESS")
      (error "test-imperfect: FAILURE"))))

(defun test-extra-operations ()
  "The same network as the simulation environment's solution, but with some extra instructions at the end."
  (let ((solution (first (evaluate-solutions "applications\\muhai-cookingbot\\evaluation\\tests\\test-extra-operation.solution" *metrics* (list *almond-crescent-cookies-environment*)))))
    (if (and (= (subgoals-ratio solution) 1)
             (= (dish-score solution) 1)
        ;     (< (smatch-score solution) 1)
             (>= (execution-time solution) (execution-time *almond-crescent-cookies-environment*)))
      (print "test-extra-operations: SUCCESS")
      (error "test-extra-operations: FAILURE"))))

(defun test-empty ()
  "A solution that only contains get-kitchen."
  (let ((solution (first (evaluate-solutions "applications\\muhai-cookingbot\\evaluation\\tests\\test-empty.solution" *metrics* (list *almond-crescent-cookies-environment*)))))
    (if (and (= (subgoals-ratio solution) 0)
             (= (dish-score solution) 0)
         ;    (< (smatch-score solution) 1)
             (< (execution-time solution) (execution-time *almond-crescent-cookies-environment*)))
      (print "test-empty: SUCCESS")
      (error "test-empty: FAILURE"))))

(defun test-multiple-recipes ()
  "A file that contains two solutions, an imperfect one and a perfect one."
  (let* ((solutions (evaluate-solutions "applications\\muhai-cookingbot\\evaluation\\tests\\test-multiple-recipes.solution" *metrics* (list *almond-crescent-cookies-environment* *afghan-biscuits-environment*)))
         (solution-perfect (first solutions))
         (solution-imperfect (second solutions)))
    (if (and (= (subgoals-ratio solution-perfect) 1)
             (= (dish-score solution-perfect) 1)
            ; (= (smatch-score solution-perfect) 1)
             (= (execution-time solution-perfect) (execution-time *afghan-biscuits-environment*))
             (= (subgoals-ratio solution-imperfect) (/ 25 26))
           ;  (< (smatch-score solution-imperfect) 1)
             (< (dish-score solution-imperfect) 1)
             (<= (execution-time solution-imperfect) (execution-time *almond-crescent-cookies-environment*)))
      (print "test-multiple-recipes: SUCCESS")
      (error "test-multiple-recipes: FAILURE"))))

(defun test-list-of-kitchen-entities ()
  "A file that contains a list of kitchen entities as the solution that is closest to the gold standard container solution."
  (let ((solution (first (evaluate-solutions "applications\\muhai-cookingbot\\evaluation\\tests\\test-list-of-kitchen-entities.solution" *metrics* (list *almond-crescent-cookies-environment*)))))
    (if (and (< (dish-score solution) 1)
           ;  (< (smatch-score solution) 1)
             (= (subgoals-ratio solution) (/ 18 26))
             (< (execution-time solution) (execution-time *almond-crescent-cookies-environment*)))
      (print "test-list-of-kitchen-entities: SUCCESS")
      (error "test-list-of-kitchen-entities: FAILURE"))))

(defun test-failed-object ()
  "A file that contains a solution with failed objects."
  (let ((solution (first (evaluate-solutions "applications\\muhai-cookingbot\\evaluation\\tests\\test-failed-object.solution" *metrics* (list *almond-crescent-cookies-environment*)))))
    (if (and (= (dish-score solution) 0)
           ;  (< (smatch-score solution) 1)
             (= (subgoals-ratio solution) (/ 1 26)))
      (print "test-failed-object: SUCCESS")
      (error "test-failed-object: FAILURE"))))

(defun test-perfect-complete ()
  "The same network as the simulation environment's solution."
  (let* ((solutions (evaluate-solutions "applications\\muhai-cookingbot\\evaluation\\tests\\test-perfect-complete.solution" '(goal-condition-success dish-approximation-score execution-time)))
         (perfection (loop for solution in solutions
                             always (and ; (= (smatch-score solution) 1)
                                         (= (subgoals-ratio solution) 1)
                                         (= (dish-score solution) 1)))))
    (if perfection
      (print "test-perfect-complete: SUCCESS")
      (error "test-perfect-complete: FAILURE"))))

(defun execute-all-tests ()
  (test-perfect)
  (test-permuted-perfect)
  (test-imperfect)
  (test-extra-operations)
  (test-multiple-recipes)
  (test-empty)
  (test-list-of-kitchen-entities)
  (test-failed-object)
  (test-perfect-complete))

;(test-perfect-complete)
;(execute-all-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience Functions (Removable) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-results (solutions)
  "Convenience Function that prints the measurement results of the given solutions."
  (loop for solution in solutions
        do (print "SOLUTION:")
           (print (recipe-id solution))
           (print "Smatch Score:")
           (print (smatch-score solution))
           (print "Ratio of Reached Subgoals:")
           (print (subgoals-ratio solution))
           (print "Dish Score:")
           (print (dish-score solution))
           (print "Execution Time:")
           (print (execution-time solution))))

;;;;;;;;;;
;; DEMO ;;
;;;;;;;;;;

(defparameter *demo-env*
  (make-instance 'simulation-environment
                 :recipe-id 'demo-env
                 :kitchen-state
                 (make-instance
                  'kitchen-state
                  :contents
                  (list (make-instance 'fridge
                                       :contents (list (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'butter
                                                                                                     :temperature
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'degrees-celsius)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 5))
                                                                                                     :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 500)))))))
                        (make-instance 'pantry
                                       :contents (list (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'white-sugar :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 1000)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'vanilla-extract :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 100)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'almond-extract :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 100)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'all-purpose-flour :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 1000)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'almond-flour :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 1000)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'powdered-white-sugar :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 500)))))))
                        (make-instance 'kitchen-cabinet
                                       :contents (list
                                                  ;; bowls
                                                  (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                                  ;; tools
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)

                                                  ;; baking equipment
                                                  (make-instance 'baking-tray)
                                                  (make-instance 'baking-paper)))))
                 :meaning-network
                 (list '(get-kitchen ?ks-1)
                       '(fetch-and-proportion ?out-1 ?ks-2 ?ks-1 ?target-container-1 butter 226 g)
                       '(bring-to-temperature ?out-2 ?ks-3 ?ks-2 ?out-1 18 degrees-celsius)
                       '(fetch-and-proportion ?out-3 ?ks-4 ?ks-3 ?target-container-2 white-sugar 116 g)
                       '(fetch-and-proportion ?out-4 ?ks-5 ?ks-4 ?target-container-3 all-purpose-flour 340 g)
                       '(fetch ?medium-bowl ?ks-6 ?ks-5 medium-bowl 1)
                       '(transfer-contents ?out-x ?rest-x ?ks-out-x ?ks-6 ?medium-bowl ?out-2 ?quantity-x ?unit-x)
                       '(transfer-contents ?out-y ?rest-y ?ks-out-y ?ks-out-x ?out-x ?out-3 ?quantity-y ?unit-y)
                       '(transfer-contents ?out-z ?rest-z ?ks-out-z ?ks-out-y ?out-y ?out-4 ?quantity-z ?unit-z)
                       '(mix ?mixed-bowl ?ks-mixed ?ks-out-z ?out-z ?tool)
                       '(bake ?thing-baked ?ks-baked ?ks-mixed ?mixed-bowl ?oven-to-bake-in 15 minute 175 degrees-celsius))
                 :primary-output-var
                 '?thing-baked))

(defparameter demo (evaluate-solutions "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\evaluation\\tests\\test-demo.solution" *metrics* (list *demo-env*)))
(print-results demo)

(defparameter demo-permuted (evaluate-solutions "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\evaluation\\tests\\test-demo-2.solution" *metrics* (list *demo-env*)))
(print-results demo-permuted)

(defparameter demo-non-baked (evaluate-solutions "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\evaluation\\tests\\test-demo-3.solution" *metrics* (list *demo-env*)))
(print-results demo-non-baked)

(defparameter demo-missing-ingredient (evaluate-solutions "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\evaluation\\tests\\test-demo-4.solution" *metrics* (list *demo-env*)))
(print-results demo-missing-ingredient)

(defparameter demo-extra-fetch (evaluate-solutions "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\evaluation\\tests\\test-demo-5.solution" *metrics* (list *demo-env*)))
(print-results demo-extra-fetch)

(defparameter demo-changed-order (evaluate-solutions "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\evaluation\\tests\\test-demo-6.solution" *metrics* (list *demo-env*)))
(print-results demo-changed-order)

(defparameter demo-empty (evaluate-solutions "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\evaluation\\tests\\test-demo-7.solution" *metrics* (list *demo-env*)))
(print-results demo-empty)

(defparameter test-grammar (evaluate-solutions "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\evaluation\\tests\\test.lisp" *metrics* (list *almond-crescent-cookies-environment*)))
(print-results test-grammar)

;;;;;;;;;;;;;;
;; EVALUATE ;;
;;;;;;;;;;;;;;

(internal-evaluate "applications\\muhai-cookingbot\\evaluation\\tests\\test-multiple-recipes.solution"
                   "applications\\muhai-cookingbot\\evaluation\\tests\\test-results.csv"
                   nil
                   'goal-condition-success 'dish-approximation-score 'execution-time)