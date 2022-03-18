;(ql:quickload :visual-dialog)
(in-package :visual-dialog)

(activate-monitor trace-fcg)
(activate-monitor trace-irl)
(monitors::deactivate-all-monitors)

;; set global variables to clevr-dialog and mnist-dialog datasets. 

(defparameter *clevr-data-path*
  (make-pathname :directory '(:absolute "Users" "laraverheyen" "documents" "datasets" "CLEVR" "CLEVR_v1.0")))

(defparameter *mnist-data-path*
  (make-pathname :directory '(:absolute "Users" "laraverheyen" "documents" "datasets" "MNIST-dialog")))


;; evaluation experiments

(evaluate-mnist-dialogs-symbolic 10 20)

(evaluate-clevr-dialogs-symbolic 50 60)
(evaluate-clevr-dialogs-symbolic 75 75)

(understand "does the earlier brown object have objects to its right")

;; evaluate one dialog

(defparameter *ontology* (build-ontology))

(defparameter *world* (make-instance 'world :configuration '((:dataset . :clevr)
                                                             (:datasplit . :train)
                                                             (:mode . :symbolic))))

(defparameter *world* (make-instance 'world :configuration '((:dataset . :mnist)
                                                             (:datasplit . :train)
                                                             (:mode . :symbolic))))

(evaluate-dialog :scene-index 75 :dialog-index 2 :world *world* :ontology *ontology* :silent nil)
(evaluate-dialog :scene-index 0 :dialog-index 0 :world *world* :ontology *ontology* :silent nil)

(average '((1 1 1) (0 1 1)))
(understand "how about that sphere")
(understand "there is a gray object")
(understand "if there is a gray object right of it what is its shape")
(understand "if there is a gray object behind that metal object what is its shape")
(understand "does the scene have other objects that are of same shape as that block")
(understand "the picture has one cyan thing")
(understand "the picture has multiple cyan things")
(understand "what number of things behind that metal thing")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



      

(define-configuration-default-value :world :clevr)

(setf *scene-pathname* (get-scene-pathname-by-index 0 *world*))
(setf *dialog* (get-dialog-by-index 0 4 *world*))

(run-dialog *scene-pathname* *dialog* *world* *ontology*)
(run-dialog *scene-pathname* '("there is a cyan object" "what is its size") *world* *ontology*)

;; evaluate-dialog: one dialog with one scene
(test-dialog :scene-index 0 :dialog-index 1 :world *world* :ontology *ontology* :overview t)

;; evaluate-dialogs: all dialog
(evaluate-dialogs :start-scene 0 :end-scene 1 :world 'clevr :overview nil)

;;;;;;MNIST examples
(defparameter *mnist-world* (make-instance 'world :dataset 'mnist :datasplit '("train") :load-dialogs t))

(setf *scene-pathname* (get-scene-pathname-by-index 0 *mnist-world*))
(setf *dialog* (get-dialog-by-index 0 0 *mnist-world*))

(run-dialog *scene-pathname* *dialog* *mnist-world* *ontology*)
(test-dialog :scene-index 0 :dialog-index 1 :world *mnist-world* :ontology *ontology* :overview t)


(world (cond ((eql 'clevr world) (make-instance 'world :dataset 'clevr :datasplit (list data-set)
                                           :load-dialogs t))
                      ((eql 'mnist-world world) (make-instance 'world :dataset 'mnist :datasplit (list data-set)
                                           :load-dialogs t))))