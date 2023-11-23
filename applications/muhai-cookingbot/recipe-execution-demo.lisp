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

(defun show-metrics-on-web-interface (solution)
  (add-element
   `((h2) ,(format nil "Metrics for recipe '~(~a~)'"
                   (recipe-id solution))))
  (add-element
   `((table :class "entity")
     ((tr)
      ((td) ((b) "Smatch Score"))
      ((td :style "padding-left: 20px;")
       ,(format nil "~a" (smatch-score solution))))
     ((tr)
      ((td) ((b) "Ratio of Reached Subgoals"))
      ((td :style "padding-left: 20px;")
       ,(format nil "~a" (subgoals-ratio solution))))
     ((tr)
      ((td) ((b) "Dish Approxmiation Score"))
      ((td :style "padding-left: 20px;")
       ,(format nil "~a" (dish-score solution))))
     ((tr)
      ((td) ((b) "Execution Time"))
      ((td :style "padding-left: 20px;")
       ,(format nil "~a" (execution-time solution)))))))


(defparameter *almond-crescent-cookies-gold-standard*
  (with-disabled-monitors
     (almond-crescent-cookies-gold-standard)))
  
(defparameter *perfect-solution*
  (evaluate-solutions 
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "perfect" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(show-metrics-on-web-interface (first *perfect-solution*))
;; dish approx score 1


;; operations are presented in a different order in the file
;; but the kitchen states are connected correctly
(defparameter *perfect-permuted-sequence*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "perfect-permuted-sequence" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(show-metrics-on-web-interface (first *perfect-permuted-sequence*))
;; dish approx score 1


;; almond extract is added to the bowl first before the vanilla extract is added
(defparameter *perfect-switched-operations*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "perfect-switched-operations" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(show-metrics-on-web-interface (first *perfect-switched-operations*))
;; dish approx score 1


;; using always new tools instead of reusing the same tools
(defparameter *missing-tool-reuse*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "missing-tool-reuse" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics *all-metrics*))
(show-metrics-on-web-interface (first *missing-tool-reuse*))
;; dish approx score 1


;; missing the unspecified operation of letting the butter get
;; to room temperature
(defparameter *missing-minor-implicit*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                :name "missing-minor-implicit" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics '(dish-approximation-score)))
(show-metrics-on-web-interface (first *missing-minor-implicit*))
;; dish approx score 0.99


;;  MISSING STEPS: putting the cookies on a lined baking tray,
;; baking them and sprinkling some powdered sugar on it
(defparameter *partial-failure*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "partial-failure" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics '(dish-approximation-score)))
(show-metrics-on-web-interface (first *partial-failure*))
;; dish approx score 0.82


;; used cocoa-powder instead of sugar   
(defparameter *wrong-ingredient*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "wrong-ingredient" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics '(dish-approximation-score)))
(show-metrics-on-web-interface (first *wrong-ingredient*))
;; dish approx score 0.75


;; additional operations occur in order to make some chocolate dip,
;; which is not part of the original recipe
(defparameter *additional-side-dish*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "additional-side-dish" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics '(dish-approximation-score)))
(show-metrics-on-web-interface (first *additional-side-dish*))
;; dish approx score 1


;; additional operations in order to make some chocolate
;; dip in which the cookies are dipped, which is not part of the main dish
(defparameter *extended-main-dish*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "extended-main-dish" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics '(dish-approximation-score)))
(show-metrics-on-web-interface (first *extended-main-dish*))
;; dish approx score 0.87


;; only steps that have been recognized were fetching tools
(defparameter *no-cooking*
  (evaluate-solutions
   (babel-pathname :directory '("applications" "muhai-cookingbot"
                                "recipe-execution-benchmark" "documentation"
                                "examples" "evaluation")
                   :name "no-cooking" :type "solution")
   (list *almond-crescent-cookies-gold-standard*)
   :metrics '(dish-approximation-score)))
(show-metrics-on-web-interface (first *no-cooking*))
;; dish approx score 0

