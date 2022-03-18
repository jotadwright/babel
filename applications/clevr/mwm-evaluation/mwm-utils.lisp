(in-package :mwm-evaluation)

;;----------------------------------;;
;; similarity and category matching ;;
;;----------------------------------;;

;; weighted similarity method that can be used to compare the prototypical values of an object and a concept (see: "Babel/experiments/multidimensional-word-meanings/concept.lisp" for the original method)
(defmethod weighted-similarity ((object mwm-object) (concept concept-entity))
  (loop for prototype in (meaning concept)
        for similarity = (mwm::similarity object prototype)
        collect (* (mwm::certainty prototype) similarity) into weighted-similarities
        finally (return (average weighted-similarities))))

;; attach category to an object that yields the highest weighted similarity out of a set of categories

(defun find-best-category (object categories) 
  (loop with best-category = nil
        with best-similarity = nil
        for cat in categories
        for similarity = (weighted-similarity object cat)
        when (or (null best-category)
                 (> similarity best-similarity))
        do (setf best-category cat
                 best-similarity similarity)
        finally
        return best-category))

;;-----------------------------;;
;; Utils for testing questions ;;
;;-----------------------------;;

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

;;-----------------------------------------------------------------;;
;; substitute category names in bind statements with concept names ;;
;;-----------------------------------------------------------------;;

(defparameter *substitution-dict*
  '((color-category . color-concept)
    (shape-category . shape-concept)
    (size-category . size-concept)
    (material-category . material-concept)
    (spatial-relation-category . spatial-concept)))

(defun substitute-category-in-bind (bind-statement)
  (let* ((bind-type (second bind-statement))
         (replacement (rest (assoc bind-type *substitution-dict*))))
    (if replacement
      (substitute replacement bind-type bind-statement)
      bind-statement)))

(defun substitute-categories (irl-program)
  (loop for predicate in irl-program
        if (eql (first predicate) 'bind)
        collect (substitute-category-in-bind predicate)
        else collect predicate))