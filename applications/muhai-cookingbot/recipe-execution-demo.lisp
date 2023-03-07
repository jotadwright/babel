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
  "Convenience function that prints
   the measurement results of the given
   solutions."
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

(defun show-metrics-on-web-interface (solutions)
  (loop for solution in solutions
        do (add-element '((hr)))
           (add-element `((h2) ,(format nil "Metrics for recipe '~(~a~)'"
                                        (recipe-id solution))))
           (when (smatch-score solution)
             (add-element `((h3) ,(format nil "Smatch Score: ~a"
                                          (smatch-score solution)))))
           (when (subgoals-ratio solution)
             (add-element `((h3) ,(format nil "Ratio of Reached Subgoals: ~a"
                                          (subgoals-ratio solution)))))
           (when (dish-score solution)
             (add-element `((h3) ,(format nil "Dish score: ~a"
                                          (dish-score solution)))))
           (when (execution-time solution)
             (add-element `((h3) ,(format nil "Execution time: ~a"
                                          (execution-time solution)))))))


(defparameter *almond-crescent-cookies-gold-standard*
  (with-disabled-monitors
     (almond-crescent-cookies-gold-standard)))


(progn
(defparameter *perfect-solution*
  (evaluate-solutions 
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "perfect" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(print-results *perfect-solution*)
(show-metrics-on-web-interface *perfect-solution*)


(defparameter *perfect-permuted-sequence*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "perfect-permuted-sequence" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(print-results *perfect-permuted-sequence*)
(show-metrics-on-web-interface *perfect-permuted-sequence*)


(defparameter *perfect-switched-operations*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "perfect-switched-operations" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(print-results *perfect-switched-operations*)
(show-metrics-on-web-interface *perfect-switched-operations*)


(defparameter *missing-tool-reuse*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "missing-tool-reuse" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(print-results *missing-tool-reuse*)
(show-metrics-on-web-interface *missing-tool-reuse*)


(defparameter *missing-minor-implicit*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                :name "missing-minor-implicit" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(print-results *missing-minor-implicit*)
(show-metrics-on-web-interface *missing-minor-implicit*)

(defparameter *partial-failure*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "partial-failure" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(print-results *partial-failure*)
(show-metrics-on-web-interface *partial-failure*)

(defparameter *wrong-ingredient*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "wrong-ingredient" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(print-results *wrong-ingredient*)
(show-metrics-on-web-interface *wrong-ingredient*)

(defparameter *additional-side-dish*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "additional-side-dish" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(print-results *additional-side-dish*)
(show-metrics-on-web-interface *additional-side-dish*)

(defparameter *extended-main-dish*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "extended-main-dish" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(print-results *extended-main-dish*)
(show-metrics-on-web-interface *extended-main-dish*)

(defparameter *no-cooking*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "no-cooking" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(print-results *no-cooking*)
(show-metrics-on-web-interface *no-cooking*)
)

