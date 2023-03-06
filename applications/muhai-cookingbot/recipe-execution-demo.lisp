(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show Gold Standard Recipe Executions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First, activate the trace-irl monitor

(activate-monitor trace-irl)

;; Show the execution of the gold standard annotation of a recipe:

(almond-crescent-cookies-gold-standard)
(afghan-biscuits-gold-standard)
(best-brownies-gold-standard)
(chocolate-fudge-cookies-gold-standard)
(easy-banana-bread-gold-standard)
(easy-oatmeal-cookies-gold-standard)
(whole-wheat-ginger-snaps-gold-standard)
(cucumber-slices-with-dill-gold-standard)
(easy-cherry-tomato-corn-salad-gold-standard)
(black-bean-and-sweet-potato-salad-gold-standard)
(almond-crescent-cookies-2-gold-standard)
(almond-crescent-cookies-3-gold-standard)
(almond-crescent-cookies-4-gold-standard)
(almond-crescent-cookies-5-gold-standard)
(coconut-tuiles-gold-standard)
(mexican-wedding-cookies-gold-standard)
(bisquick-shortcake-biscuits-gold-standard)
(chocolate-cream-cheese-cupcakes-gold-standard)
(black-bean-salad-2-gold-standard)
(black-bean-salad-3-gold-standard)
(black-bean-salad-4-gold-standard)
(black-bean-salad-5-gold-standard)
(classic-greek-salad-gold-standard)
(classic-potato-salad-gold-standard)
(cole-slaw-gold-standard)
(cranberry-fluff-salad-gold-standard)
(avocado-chicken-salad-gold-standard)
(basic-chicken-salad-gold-standard)
(broccoli-salad-gold-standard)
(croutons-vinegar-salad-gold-standard)


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

