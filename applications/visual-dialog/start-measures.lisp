(ql:quickload :visual-dialog)
(in-package :visual-dialog)

;; activate monitors
(activate-monitor questions-solved-by-mental-simulation)
(activate-monitor questions-solved-by-grammar)
(activate-monitor questions-introduced-by-grammar)
(activate-vis-js)

;; set path
(defparameter *clevr-data-path*
  (merge-pathnames
   (make-pathname :directory '(:relative "CLEVR-v1.0"))
   cl-user::*babel-corpora*))


;; visualise measures
(defun run-dialog-with-inn ()
  (initialize-values)
  (setf *visual-dialog-inn* (make-integrative-network))
  (add-element `((div :id "narrativeNetwork")
                   ,(wi::make-vis-network :element-id "narrativeNetwork"
                                          :nodes '("{id: 'node1', label: 'Node 1'}"
                                                   "{id: 'node2', label: 'Node 2'}")
                                          :edges '("{from: 'node1', to: 'node2'}"))))
  #|(add-element `((div :id "narrativeNetwork")
                   ,(wi::make-vis-network :element-id "narrativeNetwork"
                                          :nodes (handle-node-updates *visual-dialog-inn*)
                                          :edges (collect-new-edges *visual-dialog-inn*))))|#
  (defparameter *ontology* (visual-dialog::build-ontology))
  (defparameter *world* (make-instance 'world :configuration '((:dataset . :clevr) 
                                                               (:datasplit . :train)
                                                               (:mode . :symbolic))))
  
  (setf *scene-pathname* (get-scene-pathname-by-index 1 *world*))
  (setf *ont* (initialize-agent-ontology-and-world *ontology* *world* nil))
  
  (setf *caption* "a gray cube is to the back of all objects in the view")
  (setf *memory* (understand-execute-remember-caption *scene-pathname* *caption* *ont* :silent nil))

  (setf *questions* '("if there is an object on the left side of it what shape is it"
                      "what about color"
                      "how about size"
                      "what about the earlier gray object"))

  (loop for question in *questions*
            collect (understand-execute-remember *scene-pathname* question *memory* *ont* :silent nil))
;(evaluate-dialog :scene-index 1 :dialog-index 1 :world *world* :ontology *ontology* :silent nil)
  )


;; run it 
(run-dialog-with-inn) ;; kan even duren omdat de *world* builden lang duurt. 







