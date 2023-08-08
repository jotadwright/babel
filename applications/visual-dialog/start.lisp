;(ql:quickload :visual-dialog)
(in-package :visual-dialog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; START EVALUATION ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; activate monitors
(monitors:activate-monitor fcg:trace-fcg)
(monitors:activate-monitor irl:trace-irl)

;; set global variables to clevr-dialog and mnist-dialog datasets. 
(defparameter *clevr-data-path*
  (make-pathname :directory '(:absolute "Users" "laraverheyen" "documents" "datasets" "CLEVR" "CLEVR_v1.0")))

(defparameter *mnist-data-path*
  (make-pathname :directory '(:absolute "Users" "laraverheyen" "documents" "datasets" "MNIST-dialog")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; RUN YOUR OWN DIALOGUE ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;build the ontology
(defparameter *ontology* (build-ontology))

;make the world; this might take a while
(defparameter *world* (make-instance 'world :entries '((:dataset . :clevr)
                                                       (:datasplit . :train)
                                                       (:mode . :hybrid))))

;some configurations that you might want to set
;(set-configuration *world* :evaluation-mode :guess)
;(set-configuration *symbolic-primitives* :search-mode :best-first)
;(set-configuration *subsymbolic-primitives* :search-mode :best-first)
;(set-configuration *subsymbolic-primitives* :search-mode :best-first-avg-new-bindings)
;(set-configuration *subsymbolic-primitives* :search-mode :best-first)


;specify pathname index (index of the image of your choice)
(setf *scene-pathname* (get-scene-pathname-by-index 0 *world*))

;initialize the agent ontology and world
(setf *ont* (initialize-agent-ontology-and-world *ontology* *world* nil))

;make your own dialogue
(setf *caption* "there is a brown object")
(setf *questions* '("if there is an object to the left of it what is its shape"))

;start your dialogue by understanding the caption 
(setf *memory* (understand-execute-remember-caption *scene-pathname* *caption* *ont* :silent nil))

;execute the rest of the dialogue 
(setf *answers*
      (loop for question in *questions*
            collect (understand-execute-remember *scene-pathname* question *memory* *ont* :silent nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; EVALUATE DIALOGUES ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evaluation experiments
;run dialogs from start-scene to end-scene
(evaluate-mnist-dialogs-hybrid 0 0)
(evaluate-clevr-dialogs-hybrid 56 56)

;; for these experiments you first need to build the ontology and world
;build the ontology
(defparameter *ontology* (build-ontology))

;make the world; this might take a while
(defparameter *world* (make-instance 'world :entries '((:dataset . :clevr)
                                                       (:datasplit . :train)
                                                       (:mode . :hybrid))))

;run a specific dialog in a specific scene
(evaluate-dialog :scene-index 0 :dialog-index 0 :world *world* :ontology *ontology* :silent nil)

;run all dialogs from start-scene to end-scene
(evaluate-dialogs 20 30 *world*)

;comprehend dialogs from start-scene to end-scene
(comprehend-dialogs 0 0 *world*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; EVALUATION RESULTS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;final results as reported in paper:
;with guessing
(calculate-accuracy-from-dir "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/final/CLEVR-HYBRID-VAL-GUESS-latest-checkpoint/*") ;0.990432
(calculate-accuracy-from-dir "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/final/MNIST-HYBRID-TEST-GUESS-latest-checkpoint/*") ;0.99453336

;without guessing
(calculate-accuracy-from-dir "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/final/CLEVR-HYBRID-VAL-NORMAL-latest-checkpoint/*") ;0.9876187
(calculate-accuracy-from-dir "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/final/MNIST-HYBRID-TEST-latest-checkpoint/*") ;0.99385334

;;other results:       
(calculate-accuracy-from-dir "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/CLEVR-SYMBOLIC/*")
(calculate-accuracy-from-dir "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/final/MNIST-HYBRID-TEST-highest-accuracy/*")
(calculate-accuracy-from-dir "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/final/MNIST-HYBRID-TEST-lowest-loss/*")
(calculate-accuracy-from-dir "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/final/CLEVR-HYBRID-VAL-NORMAL/*") ; 0.98292



;to test problems with middle scenes in clevr:
(collect-problematic-middle-scenes)
(collect-failed-dialogs "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/final/CLEVR-HYBRID-VAL-GUESS-latest-checkpoint/*")
(check-failed-dialogs (collect-failed-dialogs "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/results/CLEVR-SYMBOLIC/*")
                      "/Users/laraverheyen/Projects/Babel3/applications/visual-dialog/evaluation/scenes-with-multiple-middles.lisp")


                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; RUN IRL PROGRAM ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;run your own irl program 
(let* ((ontology (initialize-agent-ontology-and-world *ontology* *world* nil))
       (scene-pathname (get-scene-pathname-by-index 0 *world*))
       (scene-pathname-2 (get-scene-pathname-by-index 1 *world*))
       (pathname-entity (make-instance 'pathname-entity :path scene-pathname))
       (pathname-entity-2 (make-instance 'pathname-entity :path scene-pathname-2)))
  (evaluate-irl-program `((segment-scene ?scene ?pathname)
                          (bind pathname-entity ?pathname ,pathname-entity)
                          (filter-by-attribute ?out ?scene ?pathname ?category)
                          (bind shape-category ?category sphere)
                          (unique ?unique ?out)
                          (query ?attr ?unique ?scene ?attribute-category)
                          (bind attribute-category ?attribute-category shape)
                          ) ontology :primitive-inventory (get-primitive-inventory (get-data ontology 'world))))

