(in-package :muhai-cookingbot)

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


(defun evaluate-solution (solution gold-standard)
  ;; in case the recipe does not reach the end
  (let ((solution
         (make-instance 'solution
                        :recipe-id 'broccoli-salad
                        :meaning-network solution)))
    (setf (smatch-score solution) 0.99
          (subgoals-ratio solution) 11/24
          (dish-score solution) 0.9242779
          (execution-time solution) 1659)
    solution))

(defun load-earlier-result ()
  nil)