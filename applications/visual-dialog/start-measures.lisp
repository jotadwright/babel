(ql:quickload :visual-dialog)
(in-package :visual-dialog)

;; activate monitors
(progn 
  (activate-monitor questions-solved-by-mental-simulation)
  (activate-monitor questions-solved-by-grammar)
  (activate-monitor questions-introduced-by-grammar)
  (activate-monitor node-names-fcg)
  (activate-monitor node-names-irl)
  (activate-vis-js))

(activate-monitor trace-fcg)
(activate-monitor trace-irl)

(deactivate-all-monitors)
(deactivate-monitor trace-fcg)
(deactivate-monitor trace-irl)
;; set path
(defparameter *clevr-data-path*
  (merge-pathnames
   (make-pathname :directory '(:relative "CLEVR-v1.0"))
   cl-user::*babel-corpora*))

(defparameter *clevr-data-path*
  (make-pathname :directory '(:absolute "Users" "laraverheyen" "documents" "datasets" "CLEVR" "CLEVR_v1.0")))

;; visualise measures
(defun run-dialog-with-inn ()
  (initialize-values)
  (setf *visual-dialog-inn* (make-integrative-network))
  (add-element `((div :id "narrativeNetwork")
                   ,(wi::make-vis-network :element-id "narrativeNetwork"
                                          :nodes nil
                                          :edges nil)))
  (defparameter *ontology* (visual-dialog::build-ontology))
  (defparameter *world* (make-instance 'world :configuration '((:dataset . :clevr) 
                                                               (:datasplit . :train)
                                                               (:mode . :symbolic))))
  (setf *scene-pathname* (get-scene-pathname-by-index 0 *world*))
  (setf *ont* (initialize-agent-ontology-and-world *ontology* *world* nil))
  (setf *caption* (first (get-dialog-by-index 0 3 *world* :clevr)))
 ; (setf *memory* (understand-execute-remember-caption *scene-pathname* *caption* *ont* :silent nil))
  (setf *questions* '("if there is an object on the right side of it what shape is it"
                      "what is the color of the aforementioned red cube"
                      ;"what about size"
                      ;"and material"
                      ;"and that of the previous large object"
                      ;"does the view have other objects that share the same material with the previous tiny object"
                      ;"what color is the above tiny object"
                      ;"what number of other objects in the image share similar shape with the earlier large object"
                      ))
  (setf *questions* (subseq (get-dialog-by-index 0 3 *world* :clevr) 1 3))
 ;(loop for question in *questions*
 ;      collect (understand-execute-remember *scene-pathname* question *memory* *ont* :silent nil))
  (evaluate-dialog :scene-index 0 :dialog-index 3 :world *world* :ontology *ontology* :silent nil)
)


(all-questions-solved?)

(loop for scene from 11 to 20 
        collect (loop for dialog from 0 to 4
                      do (evaluate-dialog :scene-index scene :dialog-index dialog :world *world* :ontology *ontology* :silent nil)
                      do (print (all-questions-solved?))
                      collect (all-questions-solved?)))

(get-dialog-by-index 0 3 *world* :clevr)

;; run it
(run-dialog-with-inn)

(setf *live-demo* nil)
(setf *time* 0.5)

(reset-id-counters)

(total-questions-introduced-by-grammar) ;160
(total-questions-solved-by-grammar) ;83
(total-questions-solved-by-mental-simulation) ;77

(variables-introduced-by-grammar)
(variables-solved-by-perception)
(variables-solved-by-grammar)


(interpretation-detailed)

(write-measures-to-file 'node-names-fcg 'node-names-irl
                        '(questions-introduced-by-grammar questions-solved-by-grammar questions-solved-by-mental-simulation))

(slot-value (monitors::get-monitor 'node-names-fcg) 'values)




(detailed-measures-lists (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values))



