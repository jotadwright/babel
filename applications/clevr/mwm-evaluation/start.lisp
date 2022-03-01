(ql:quickload :mwm-evaluation)
(in-package :mwm-evaluation)

(activate-monitor trace-fcg)
(activate-monitor trace-irl)

;; geleerde concepten staan onder
;; Babel/experiments/multidimensional-word-meanings/learned-concepts.zip
;; cl-store:restore om .store files in te lezen
(defclass concept-entity (concept entity)
  ())

(defclass color-concept (concept-entity) ())
(defclass size-concept (concept-entity) ())
(defclass material-concept (concept-entity) ())
(defclass shape-concept (concept-entity) ())
(defclass spatial-concept (concept-entity) ())

(defun restore-concept (path class)
  (let ((concept (cl-store:restore path)))
    (Make-instance class
                   :id (pathname->conceptname path)
                   :form (form concept)
                   :meaning (copy-object (meaning concept)))))

(defun pathname->conceptname (path)
  (intern (upcase (first (split (pathname-name path) #\-)))))
  
;; concepten zijn gedefinieerd in Babel/experiments/multidimensional-word-meanings/concept.lisp
;; alsook weighted-similarity methods

;; ontology aanmaken met geleerde concepten
;; ontology is een instance van #'blackboard
(defparameter *my-ontology* (make-blackboard))

(set-data *my-ontology* 'spatial-relations
          (loop for pathname in (directory (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "relationships")))
                collect (restore-concept pathname 'spatial-concept)))

(set-data *my-ontology* 'colors
          (loop for pathname in (directory
                                 (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "colors")))
                collect (restore-concept pathname 'color-concept)))

(set-data *my-ontology* 'materials
          (loop for pathname in (directory (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "materials")))
                collect (restore-concept pathname 'material-concept)))

(set-data *my-ontology* 'shapes
          (loop for pathname in (directory (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "shapes")))
                collect (restore-concept pathname 'shape-concept)))

(set-data *my-ontology* 'sizes
          (loop for pathname in (directory (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "sizes")))
                collect (restore-concept pathname 'size-concept)))

(push-data *my-ontology* 'booleans (make-instance 'boolean-category :id 'yes :bool t))
(push-data *my-ontology* 'booleans (make-instance 'boolean-category :id 'no :bool nil))
(loop for attribute in '(shape size material color)
          do (clevr-world::add-category-to-ontology *my-ontology* attribute 'attribute))
;(clevr-world::add-category-to-ontology *my-ontology* 'thing 'shape)

;(add-element (make-html *my-ontology*))

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
         (cons `(bind pathname-entity ,scene-var ,scene-path)
               (substitute-categories irl-program))
         *my-ontology* :primitive-inventory *mwm-primitives*)))))

;(test-utterance-in-first-scene "How many blue cubes are there?")
;(test-utterance-in-first-scene "What color is the large sphere?")

(defparameter *substitution-dict*
  '((color-category . color-concept)
    (shape-category . shape-concept)
    (size-category . size-concept)
    (material-category . material-concept)
    (spatial-relation-category . spatial-concept)))

(defun substitute-category-in-bind (bind-statement)
  (let* ((bind-type (second bind-statement))
         (replacement (rest (assoc bind-type *substitution-dict*))))
    (substitute replacement bind-type bind-statement)))

(defun substitute-categories (irl-program)
  (loop for predicate in irl-program
        if (eql (first predicate) 'bind)
        collect (substitute-category-in-bind predicate)
        else collect predicate))
