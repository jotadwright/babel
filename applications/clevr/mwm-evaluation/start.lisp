(ql:quickload :mwm-evaluation)
(in-package :mwm-evaluation)

(activate-monitor trace-fcg)
(activate-monitor trace-irl)

;;--------------;;
;; The ontology ;;
;;--------------;;

(make-mwm-ontology (babel-pathname :directory (list "experiments""multidimensional-word-meanings" "learned-concepts" "thesis-main-results" "baseline-simulated-default-lexicon" "serie-1")) *my-ontology*)

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


;; Test sentences (see "Babel/grammars/clevr-grammar/start.lisp" for more examples):
;(test-utterance-in-first-scene "there is a big gray object that is the same shape as the purple rubber object; what is it made of?")
;(test-utterance-in-first-scene "What color is the large sphere?")
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
(defun adjust-primitive-errors (irl-program hash-table)
  (let ((unique-predicates
         (remove-duplicates (mapcar #'first irl-program))))
    (loop for predicate in unique-predicates
          do
          (case predicate
            ('count! (incf (car (gethash 'count! hash-table))))
            ('equal? (incf (car (gethash 'equal? hash-table))))
            ('equal-integer (incf (car (gethash 'equal-integer hash-table))))
            ('less-than (incf (car (gethash 'less-than hash-table))))
            ('greater-than (incf (car (gethash 'greater-than hash-table))))
            ('exist (incf (car (gethash 'exist hash-table))))
            ('filter (incf (car (gethash 'filter hash-table))))
            ('intersect (incf (car (gethash 'intersect hash-table))))
            ('query (incf (car (gethash 'query hash-table))))
            ('relate (incf (car (gethash 'relate hash-table))))
            ('same (incf (car (gethash 'same hash-table))))
            ('union! (incf (car (gethash 'union! hash-table))))
            ('unique (incf (car (gethash 'unique hash-table))))))))

(defun adjust-primitive-frequencies (irl-program hash-table)
  (let ((unique-predicates
         (remove-duplicates (mapcar #'first irl-program))))
    (loop for predicate in unique-predicates
          do
          (case predicate
            ('count! (incf (cdr (gethash 'count! hash-table))))
            ('equal? (incf (cdr (gethash 'equal? hash-table))))
            ('equal-integer (incf (cdr (gethash 'equal-integer hash-table))))
            ('less-than (incf (cdr (gethash 'less-than hash-table))))
            ('greater-than (incf (cdr (gethash 'greater-than hash-table))))
            ('exist (incf (cdr (gethash 'exist hash-table))))
            ('filter (incf (cdr (gethash 'filter hash-table))))
            ('intersect (incf (cdr (gethash 'intersect hash-table))))
            ('query (incf (cdr (gethash 'query hash-table))))
            ('relate (incf (cdr (gethash 'relate hash-table))))
            ('same (incf (cdr (gethash 'same hash-table))))
            ('union! (incf (cdr (gethash 'union! hash-table))))
            ('unique (incf (cdr (gethash 'unique hash-table))))))))

(defun compute-error-rate (hash-table)
  (let ((logfile (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                          :name "primitives-errors" :type "txt")))
    (ensure-directories-exist logfile)
  (with-open-file (stream logfile
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :overwrite)
      (loop for key being the hash-keys of hash-table
            if (not (eql 0 (cdr (gethash key hash-table))))
            do (progn
                 (write-line
                  (format nil "The error rate for ~a is ~,2f" key (/ (car (gethash key hash-table)) (cdr (gethash key hash-table)))) stream)
                  (force-output stream))
            else do (progn
                 (write-line
                  (format nil "The error rate for ~a is ~,2f" key  0 ) stream)
                  (force-output stream))))))

(defun compute-accuracy (log clevr-world nr-of-scenes nr-of-questions hash-table)
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
                                         (adjust-primitive-frequencies irl-program hash-table)
                                         (write-line
                                          (format nil "~a,~a,~a,~a,1" scene-name q answer computed-answer) log)
                                         (force-output log))
                                else collect 0 into scene-accuracy
                                and do (progn
                                         (adjust-primitive-frequencies irl-program hash-table)
                                         (write-line
                                          (format nil "~a,~a,~a,~a,0" scene-name q answer computed-answer) log)
                                         (force-output log)
                                         (adjust-primitive-errors irl-program hash-table))
                                finally return scene-accuracy)
                   into accuracy
                   do (incf processed-scenes)
                   finally (return accuracy)))))



(defgeneric evaluate-mwm-accuracy (data-split &key nr-of-scenes nr-of-questions)
  (:documentation "Evaluate the accuracy of the mwm-concepts."))


(defmethod evaluate-mwm-accuracy (data-split &key nr-of-scenes nr-of-questions)
  (let ((clevr-world
         (make-instance 'clevr-world
                        :data-sets (list data-split)
                        :load-questions t))
        (accuracy 0)
        (errors-per-primitive (make-hash-table))
        (logfile
         (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                         :name "mwm-evaluation" :type "txt")))
    
    (setf (gethash 'count! errors-per-primitive) '(0 . 0))
    (setf (gethash 'equal? errors-per-primitive) '(0 . 0))
    (setf (gethash 'equal-integer errors-per-primitive) '(0 . 0))
    (setf (gethash 'less-than errors-per-primitive) '(0 . 0))
    (setf (gethash 'greater-than errors-per-primitive) '(0 . 0))
    (setf (gethash 'exist errors-per-primitive) '(0 . 0))
    (setf (gethash 'filter errors-per-primitive) '(0 . 0))
    (setf (gethash 'intersect errors-per-primitive) '(0 . 0))
    (setf (gethash 'query errors-per-primitive) '(0 . 0))
    (setf (gethash 'relate errors-per-primitive) '(0 . 0))
    (setf (gethash 'same errors-per-primitive) '(0 . 0))
    (setf (gethash 'union! errors-per-primitive) '(0 . 0))
    (setf (gethash 'unique errors-per-primitive) '(0 . 0))
    
    (ensure-directories-exist logfile)
    (with-open-file (log logfile :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
    (setf accuracy (compute-accuracy log
                      clevr-world
                      nr-of-scenes
                      nr-of-questions
                      errors-per-primitive)))
    (compute-error-rate errors-per-primitive)
    accuracy))

(evaluate-mwm-accuracy "val" :nr-of-scenes 1)

