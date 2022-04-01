;(ql:quickload :visual-dialog)
(in-package :visual-dialog)

(monitors:activate-monitor trace-fcg)
(activate-monitor trace-irl)
(monitors::deactivate-all-monitors)

;; set global variables to clevr-dialog and mnist-dialog datasets. 

(defparameter *clevr-data-path*
  (make-pathname :directory '(:absolute "Users" "laraverheyen" "documents" "datasets" "CLEVR" "CLEVR_v1.0")))

(defparameter *mnist-data-path*
  (make-pathname :directory '(:absolute "Users" "laraverheyen" "documents" "datasets" "MNIST-dialog")))


;; evaluation experiments

(evaluate-mnist-dialogs-symbolic 41 41)

(evaluate-clevr-dialogs-symbolic 50 60)
(evaluate-clevr-dialogs-symbolic 250 250)

;; evaluate one dialog

(defparameter *ontology* (build-ontology))

(defparameter *world* (make-instance 'world :configuration '((:dataset . :clevr)
                                                             (:datasplit . :train)
                                                             (:mode . :symbolic))))

(defparameter *world* (make-instance 'world :configuration '((:dataset . :mnist)
                                                             (:datasplit . :train)
                                                             (:mode . :symbolic))))

(evaluate-dialog :scene-index 66796 :dialog-index 0 :world *world* :ontology *ontology* :silent nil)
(evaluate-dialog :scene-index 31538 :dialog-index 4 :world *world* :ontology *ontology* :silent nil)
(evaluate-dialog :scene-index 20204 :dialog-index 0 :world *world* :ontology *ontology* :silent nil)
(evaluate-dialog :scene-index 25070 :dialog-index 2 :world *world* :ontology *ontology* :silent nil)

(evaluate-dialog :scene-index 35111 :dialog-index 4 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 16681 :dialog-index 3 :world *world* :ontology *ontology* :silent nil);

(evaluate-dialog :scene-index 57455 :dialog-index 1 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 57466 :dialog-index 4 :world *world* :ontology *ontology* :silent nil);

(evaluate-dialog :scene-index 32286 :dialog-index 3 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 18195 :dialog-index 2 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 58387 :dialog-index 2 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 17239 :dialog-index 2 :world *world* :ontology *ontology* :silent nil);

(evaluate-dialog :scene-index 17262 :dialog-index 4 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 39916 :dialog-index 4 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 54623 :dialog-index 4 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 65088 :dialog-index 1 :world *world* :ontology *ontology* :silent nil);

(evaluate-dialog :scene-index 44965 :dialog-index 2 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 6302 :dialog-index 4 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 62849 :dialog-index 4 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 55271 :dialog-index 1 :world *world* :ontology *ontology* :silent nil);

(evaluate-dialog :scene-index 6404 :dialog-index 3 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 42805 :dialog-index 0 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 4569 :dialog-index 0 :world *world* :ontology *ontology* :silent nil);

(evaluate-dialog :scene-index 28384 :dialog-index 4 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 31418 :dialog-index 0 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 36944 :dialog-index 2 :world *world* :ontology *ontology* :silent nil);

(evaluate-dialog :scene-index 36973 :dialog-index 0 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 30415 :dialog-index 2 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 35839 :dialog-index 0 :world *world* :ontology *ontology* :silent nil);

(evaluate-dialog :scene-index 61079 :dialog-index 2 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 2376 :dialog-index 1 :world *world* :ontology *ontology* :silent nil);
(evaluate-dialog :scene-index 43943 :dialog-index 3 :world *world* :ontology *ontology* :silent nil)



(average '((1 1 1) (0 1 1)))
(understand "how about that sphere")
(understand "there is a gray object")
(understand "if there is a gray object right of it what is its shape")
(understand "if there is a gray object behind that metal object what is its shape")
(understand "does the scene have other objects that are of same shape as that block")
(understand "the picture has one cyan thing")
(understand "the picture has multiple cyan things")
(understand "what number of things behind that metal thing")

`


(setf *scene-pathname* (get-scene-pathname-by-index 0 *world*))
(setf *ont* (initialize-agent-ontology-and-world *ontology* *world*))
(setf *caption* (first *dialog*))
(setf *questions* (rest *dialog*))
(setf *memory* (understand-execute-remember-first-question *scene-pathname* *caption* *ont* :silent nil))
(setf *answers*
      (loop for question in *questions*
            collect (understand-execute-remember *scene-pathname* question *memory* *ont* :silent nil)))

(eql 1.0 1.0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVALUATION

(collect-problematic-middle-scenes)
        
(calculate-accuracy-from-dir "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/CLEVR-SYMBOLIC/*")
(calculate-accuracy-from-dir "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/MNIST-SYMBOLIC/*")

(collect-failed-dialogs "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/CLEVR-SYMBOLIC/*")
(check-failed-dialogs (collect-failed-dialogs "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/CLEVR-SYMBOLIC/*")
                      "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/scenes-with-multiple-middles.lisp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(let* ((ontology (initialize-agent-ontology-and-world *ontology* *world*))
       (scene-pathname (get-scene-pathname-by-index 0 *world*))
       (scene-pathname-2 (get-scene-pathname-by-index 1 *world*))
       (pathname-entity (make-instance 'pathname-entity :pathname scene-pathname))
       (pathname-entity-2 (make-instance 'pathname-entity :pathname scene-pathname-2)))
  (evaluate-irl-program `((segment-scene ?scene ?pathname)
                          (bind pathname-entity ?pathname ,pathname-entity)
                          (filter-by-attribute ?out ?scene ?pathname ?category)
                          (bind shape-category ?category cube)
                          (segment-scene ?scene-2 ?pathname-2)
                          (bind pathname-entity ?pathname-2 ,pathname-entity-2)
                          (filter-by-attribute ?out-2 ?scene-2 ?pathname-2 ?category-2)
                          (bind shape-category ?category-2 cylinder)) ontology :primitive-inventory (get-primitive-inventory (get-data ontology 'world))))

      

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