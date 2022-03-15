(ql:quickload :mwm-evaluation)
(in-package :mwm-evaluation)

(activate-monitor trace-fcg)
(activate-monitor trace-irl)

;;--------------;;
;; The ontology ;;
;;--------------;;

;; Files containing the concepts are under: "Babel/experiments/multidimensional-word-meanings/learned-concepts.zip"
;; Concepts are defined in "Babel/experiments/multidimensional-word-meanings/concept.lisp"

;; Make an ontology (instance of #'blackboard)
(defparameter *my-ontology* (make-blackboard))

;; Load spatial concepts
;; Meanings for the spatial concepts 'front' and 'behind' are switched to contain the right values
(set-data *my-ontology* 'spatial-relations (list (let ((concept (cl-store:restore (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "relationships" "behind-cxn.store")))))
                                                   (Make-instance 'spatial-concept
                                                                  :id 'front
                                                                  :form "front"
                                                                  :meaning (copy-object (meaning concept))))

                                                 (let ((concept (cl-store:restore (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "relationships" "front-cxn.store")))))
                                                   (Make-instance 'spatial-concept
                                                                  :id 'behind
                                                                  :form "behind"
                                                                  :meaning (copy-object (meaning concept))))
                                                 (let ((concept (cl-store:restore (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "relationships" "left-cxn.store")))))
                                                   (Make-instance 'spatial-concept
                                                                  :id 'left
                                                                  :form (form concept)
                                                                  :meaning (copy-object (meaning concept))))
                                                 (let ((concept (cl-store:restore (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "relationships" "right-cxn.store")))))
                                                   (Make-instance 'spatial-concept
                                                                  :id 'right
                                                                  :form (form concept)
                                                                  :meaning (copy-object (meaning concept)))) ))

;; Load color concepts
(set-data *my-ontology* 'colors
          (loop for pathname in (directory(babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "colors")))
                collect (restore-concept pathname 'color-concept)))
;; Load material concepts
(set-data *my-ontology* 'materials
          (loop for pathname in (directory (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "materials")))
                collect (restore-concept pathname 'material-concept)))

;; Load shape concepts
(set-data *my-ontology* 'shapes
          (loop for pathname in (directory (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "shapes")))
                collect (restore-concept pathname 'shape-concept)))

;; Add 'thing as a shape-concept
(set-data *my-ontology* 'thing
          (list (make-instance 'shape-concept
                               :id 'thing
                               :form "thing"
                               :meaning nil)))

;; Load size concepts
(set-data *my-ontology* 'sizes
          (loop for pathname in (directory (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1" "sizes")))
                collect (restore-concept pathname 'size-concept)))

;; Load boolean categories
(push-data *my-ontology* 'booleans (make-instance 'boolean-category :id 'yes :bool t))
(push-data *my-ontology* 'booleans (make-instance 'boolean-category :id 'no :bool nil))

;; Load attribute categories
(loop for attribute in '(shape size material color)
          do (clevr-world::add-category-to-ontology *my-ontology* attribute 'attribute))


;; Show the ontology in the web-interface:
;; (add-element (make-html *my-ontology*))

;;---------;;
;; Testing ;;
;;---------;;

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
                  :name "CLEVR_val_000004" :type "json")
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


;; Test sentences (see "Babel/grammars/clevr-grammar/start.lisp" for more examples):
;(test-utterance-in-first-scene "there is a big gray object that is the same shape as the purple rubber object; what is it made of?")
;(test-utterance-in-first-scene "What color is the small sphere?")
;(test-utterance-in-first-scene "How many things have the same shape as the large red thing?")
;(test-utterance-in-first-scene "How many things are left of the small gray sphere that is in front of the large sphere that is right of the large blue cube?")
;(test-utterance-in-first-scene "How many things are left of the purple sphere that is behind the yellow thing?")

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

;;------------;;
;; Evaluation ;;
;;------------;;
;; See "ehai-babel/applications/clevr/clevr-evaluation/accuracy.lisp" for the example code
;; Compute the accuracy on the clevr dataset using the learned concepts

;; Make a string from the computed answer so that it can be compared to the ground-truth string
(defun answer->str (answer-value)
  (case #+lispworks (type-of answer-value)
        #+ccl (if (listp (type-of answer-value))
                  (first (type-of answer-value))
                  (type-of answer-value))
        #+sbcl (if (listp (type-of answer-value))
                  (first (type-of answer-value))
                  (type-of answer-value))
    (number (mkstr answer-value))
    (fixnum (mkstr answer-value))
    (integer (mkstr answer-value))
    (bit (mkstr answer-value))
    (shape-concept (mkstr (id answer-value)))
    (size-concept (mkstr (id answer-value)))
    (color-concept (mkstr (id answer-value)))
    (material-concept (mkstr (id answer-value)))
    (boolean-category (mkstr (id answer-value)))))


;; Compute the answer for an irll-program
(defun compute-answer (irl-program scene-var scene-path-entity)
  "Given an irl-program, a variable and a scene path,
   compute the answer."
  (let ((solutions
         (evaluate-irl-program
          (cons `(bind pathname-entity ,scene-var ,scene-path-entity)(substitute-categories irl-program))
          *my-ontology* :primitive-inventory *mwm-primitives*)))
    (when (and solutions (length= solutions 1))
      (let* ((target-var (get-target-var irl-program))
             (target-value (value (find target-var (first solutions) :key #'var))))
        (answer->str target-value)))))

;; Function that adds frequencies for primitives to a hash table (for error analysis)
(defun get-primitives (irl-program hash-table)
  (let ((unique-predicates
         (remove-duplicates (mapcar #'first irl-program))))
    (loop for predicate in unique-predicates
          do
          (case predicate
            ('count (incf (gethash 'count hash-table 0)))
            ('equal (incf (gethash 'equal hash-table 0)))
            ('equal-integer (incf (gethash 'equal-integer hash-table 0)))
            ('less-than (incf (gethash 'less-than hash-table 0)))
            ('greater-than (incf (gethash 'greater-than hash-table 0)))
            ('exist (incf (gethash 'exist hash-table 0)))
            ('filter (incf (gethash 'filter hash-table 0)))
            ('intersect (incf (gethash 'intersect hash-table 0)))
            ('query (incf (gethash 'query hash-table 0)))
            ('relate (incf (gethash 'relate hash-table 0)))
            ('same (incf (gethash 'same hash-table 0)))
            ('union (incf (gethash 'union hash-table 0)))
            ('unique (incf (gethash 'unique hash-table 0)))))))

(defun compute-error-rate (hash-table)
  (with-open-file (stream (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                          :name "primitives-errors" :type "txt")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :overwrite)
      (loop for key being the hash-keys of hash-table
        using (hash-value value)
        if (not (eql key 'nr-of-questions))
        do (progn
             (write-line
              (format nil "The error rate for ~a is ~,1f" key (/ value (gethash 'nr-of-questions hash-table))) stream)
          (force-output stream)))))


(defun compute-accuracy (logfile clevr-world nr-of-scenes nr-of-questions hash-table)
  (ensure-directories-exist logfile)
    (with-open-file (log logfile :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
    (average
     (remove nil
             (loop with processed-questions = 0
                   with processed-scenes = 0
                   for scene-path in (scenes clevr-world)
                   for question-path in (question-sets clevr-world)
                   for set-of-questions = (load-clevr-question-set question-path)
                   for path-entity = (make-instance 'pathname-entity :pathname scene-path)
                   for scene-name = (pathname-name scene-path)
                   if (and nr-of-scenes (>= processed-scenes nr-of-scenes))
                   return accuracy
                   else
                   append (loop for clevr-question in (questions set-of-questions)
                                for q = (question clevr-question)
                                for answer = (answer clevr-question)
                                for (irl-program cipn nil)
                                     = (multiple-value-list
                                        (clevr-grammar::understand q))
                                for scene-var = (extract-scene-unit-variable cipn)
                                for computed-answer = (compute-answer irl-program scene-var path-entity)
                                do (incf processed-questions)
                                (format t ".")
                                if (and nr-of-questions (>= processed-questions nr-of-questions))
                                return scene-accuracy
                                else if (and (find 'fcg::succeeded (fcg::statuses cipn))
                                             (string= (upcase answer)
                                                      (upcase computed-answer)))
                                collect 1 into scene-accuracy
                                and do (progn
                                         (write-line
                                          (format nil "~a,~a,~a,~a,1" scene-name q answer computed-answer) log)
                                         (force-output log))
                                else collect 0 into scene-accuracy
                                and do (progn
                                         (write-line
                                          (format nil "~a,~a,~a,~a,0" scene-name q answer computed-answer) log)
                                         (force-output log)
                                         (get-primitives irl-program hash-table))
                                finally return scene-accuracy)
                   into accuracy
                   do (incf processed-scenes)
                   finally (progn
                             (setf (gethash 'nr-of-questions hash-table) processed-questions)
                             (compute-error-rate hash-table)
                             (return accuracy)))))))



(defgeneric evaluate-mwm-accuracy (data-split &key nr-of-scenes nr-of-questions)
  (:documentation "Evaluate the accuracy of the mwm-concepts."))


(defmethod evaluate-mwm-accuracy (data-split &key nr-of-scenes nr-of-questions)
  (let ((clevr-world
         (make-instance 'clevr-world
                        :data-sets (list data-split)
                        :load-questions t))
        (errors-per-primitive
         (setq errors-per-primitive (make-hash-table)))
        (logfile
         (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                         :name "mwm-evaluation" :type "txt")))
    (compute-accuracy logfile
                      clevr-world
                      nr-of-scenes
                      nr-of-questions
                      errors-per-primitive)
    ))

(evaluate-mwm-accuracy "val" :nr-of-scenes 1)

