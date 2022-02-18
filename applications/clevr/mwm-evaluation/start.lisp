(ql:quickload :mwm-evaluation)
(in-package :mwm-evaluation)

(activate-monitor trace-fcg)
(activate-monitor trace-irl)

;; geleerde concepten staan onder
;; Babel/experiments/multidimensional-word-meanings/learned-concepts.zip

;; cl-store:restore om .store files in te lezen

;; concepten zijn gedefinieerd in Babel/experiments/multidimensional-word-meanings/concept.lisp
;; alsook weighted-similarity methods

;; ontology aanmaken met geleerde concepten
;; ontology is een instance van #'blackboard
(defparameter *my-ontology* (make-blackboard))

;; segment-scene; symbolische clevr data omzetten naar continue features
;; zie Babel/experiments/multidimensional-word-meanings/world.lisp

;; voorbeeldzinnen in Babel/grammars/clevr-grammar/start.lisp
;; fcg gebruiken om van zin naar meaning te gaan met #'understand

(defun extract-scene-unit-variable (cipn)
  "returns scene-variable from resulting cfs of given cipn"
  (let* ((cfs (pole-structure (left-pole (car-resulting-cfs (cipn-car cipn)))))
         (scene-unit (find 'fcg::scene-unit cfs :key #'first))
         (scene-var (second (find 'fcg::scene (rest scene-unit) :key #'first))))
    scene-var))

(defparameter *clevr-scene*
  (merge-pathnames
   (make-pathname :directory '(:relative "CLEVR-v1.0" "scenes" "val")
                  :name "CLEVR_val_000000" :type "json")
   cl-user:*babel-corpora*))

(defun test-utterance-in-first-scene (utterance)
  (multiple-value-bind (irl-program cipn cip) 
      (understand utterance)
    (when (find 'fcg::succeeded (fcg::statuses cipn))
      (let ((scene-var (extract-scene-unit-variable cipn))
            (scene-path (make-instance 'pathname-entity
                                       :pathname *clevr-scene*)))
        (evaluate-irl-program
         (cons `(bind pathname-entity ,scene-var ,scene-path) irl-program)
         *my-ontology* :primitive-inventory *mwm-primitives*)))))

(test-utterance-in-first-scene "How many blue cubes are there?")
(test-utterance-in-first-scene "What color is the large sphere?")
  