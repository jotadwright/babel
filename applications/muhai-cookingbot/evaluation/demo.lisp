(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

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
           (print (execution-time solution))
           (print (execution-time *almond-crescent-cookies-environment*))))


;(defparameter test (evaluate-solutions "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\evaluation\\tests\\test-list-of-kitchen-entities.solution" (list *almond-crescent-cookies-environment*)))
;(print-results test)


;;;;;;;;;;
;; DEMO ;;
;;;;;;;;;;

(activate-monitor trace-irl)

(defparameter *perfect-solution*
  (evaluate-solutions "applications/muhai-cookingbot/recipe-execution-benchmark/data/gold standard solutions/meaning-only/afghan-biscuits.solution"
                      '(goal-condition-success dish-approximation-score execution-time)
                      (list *afghan-biscuits-environment*)))
(print-results *perfect-solution*)

(defparameter perfect-solution (evaluate-solutions "applications\\muhai-cookingbot\\benchmark\\documentation\\metrics\\examples\\perfect.solution" *metrics* (list *almond-crescent-cookies-environment*)))
(print-results perfect-solution)

(defparameter perfect-permuted-sequence (evaluate-solutions "applications\\muhai-cookingbot\\benchmark\\documentation\\metrics\\examples\\perfect-permuted-sequence.solution" *metrics* (list *almond-crescent-cookies-environment*)))
(print-results perfect-permuted-sequence)

(defparameter perfect-switched-operations (evaluate-solutions "applications\\muhai-cookingbot\\benchmark\\documentation\\metrics\\examples\\perfect-switched-operations.solution" *metrics* (list *almond-crescent-cookies-environment*)))
(print-results perfect-switched-operations)

(defparameter missing-tool-reuse (evaluate-solutions "applications\\muhai-cookingbot\\benchmark\\documentation\\metrics\\examples\\missing-tool-reuse.solution" *metrics* (list *almond-crescent-cookies-environment*)))
(print-results missing-tool-reuse)

(defparameter missing-minor-implicit (evaluate-solutions "applications\\muhai-cookingbot\\benchmark\\documentation\\metrics\\examples\\missing-minor-implicit.solution" *metrics* (list *almond-crescent-cookies-environment*)))
(print-results missing-minor-implicit)

(defparameter partial-failure (evaluate-solutions "applications\\muhai-cookingbot\\benchmark\\documentation\\metrics\\examples\\partial-failure.solution" *metrics* (list *almond-crescent-cookies-environment*)))
(print-results partial-failure)

(defparameter wrong-ingredient (evaluate-solutions "applications\\muhai-cookingbot\\benchmark\\documentation\\metrics\\examples\\wrong-ingredient.solution" *metrics* (list *almond-crescent-cookies-environment*)))
(print-results wrong-ingredient)

(defparameter additional-side-dish (evaluate-solutions "applications\\muhai-cookingbot\\benchmark\\documentation\\metrics\\examples\\additional-side-dish.solution" *metrics* (list *almond-crescent-cookies-environment*)))
(print-results additional-side-dish)

(defparameter extended-main-dish (evaluate-solutions "applications\\muhai-cookingbot\\benchmark\\documentation\\metrics\\examples\\extended-main-dish.solution" *metrics* (list *almond-crescent-cookies-environment*)))
(print-results extended-main-dish)

(defparameter no-cooking (evaluate-solutions "applications\\muhai-cookingbot\\benchmark\\documentation\\metrics\\examples\\no-cooking.solution" *metrics* (list *almond-crescent-cookies-environment*)))
(print-results no-cooking)

