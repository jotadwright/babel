(in-package :mwm-evaluation)


;;------------;;
;; Evaluation ;;
;;------------;;
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


;; Compute the answer for an irl-program
(defun compute-answer (irl-program scene-var scene-path-entity ontology)
  "Given an irl-program, a variable and a scene path,
   compute the answer."
  (let* ((irl-program-with-scene
          (cons `(bind pathname-entity ,scene-var ,scene-path-entity)
                (substitute-categories irl-program)))
         (solutions
          (evaluate-irl-program irl-program-with-scene ontology
                                :primitive-inventory *mwm-primitives*
                                :n 1 ;;; !!!
                                )))
    (when (and solutions (length= solutions 1))
      (let* ((target-var (get-target-var irl-program))
             (target-value (value (find target-var (first solutions) :key #'var))))
        (answer->str target-value)))))


;; Frequencies of a primitive occurring in a question and frequencies of errors per primitive are added to a hash-table
(defun adjust-primitive-errors (irl-program hash-table)
  (let ((unique-predicates
         (remove-duplicates (mapcar #'first irl-program))))
    (loop for predicate in unique-predicates
          do (case predicate
               (count! (incf (car (gethash 'count! hash-table))))
               (equal? (incf (car (gethash 'equal? hash-table))))
               (equal-integer (incf (car (gethash 'equal-integer hash-table))))
               (less-than (incf (car (gethash 'less-than hash-table))))
               (greater-than (incf (car (gethash 'greater-than hash-table))))
               (exist (incf (car (gethash 'exist hash-table))))
               (filter (incf (car (gethash 'filter hash-table))))
               (intersect (incf (car (gethash 'intersect hash-table))))
               (query (incf (car (gethash 'query hash-table))))
               (relate (incf (car (gethash 'relate hash-table))))
               (same (incf (car (gethash 'same hash-table))))
               (union! (incf (car (gethash 'union! hash-table))))
               (unique (incf (car (gethash 'unique hash-table))))))))

(defun adjust-primitive-frequencies (irl-program hash-table)
  (let ((unique-predicates
         (remove-duplicates (mapcar #'first irl-program))))
    (loop for predicate in unique-predicates
          do (case predicate
               (count! (incf (cdr (gethash 'count! hash-table))))
               (equal? (incf (cdr (gethash 'equal? hash-table))))
               (equal-integer (incf (cdr (gethash 'equal-integer hash-table))))
               (less-than (incf (cdr (gethash 'less-than hash-table))))
               (greater-than (incf (cdr (gethash 'greater-than hash-table))))
               (exist (incf (cdr (gethash 'exist hash-table))))
               (filter (incf (cdr (gethash 'filter hash-table))))
               (intersect (incf (cdr (gethash 'intersect hash-table))))
               (query (incf (cdr (gethash 'query hash-table))))
               (relate (incf (cdr (gethash 'relate hash-table))))
               (same (incf (cdr (gethash 'same hash-table))))
               (union! (incf (cdr (gethash 'union! hash-table))))
               (unique (incf (cdr (gethash 'unique hash-table))))))))


;; Error rate is computed by dividing number of errors per primitive by the number of questions that contain the primitive
(defun compute-error-rate (hash-table errors-filename)
  (let ((logfile
         (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                         :name errors-filename  :type "txt")))
    (ensure-directories-exist logfile)
    (with-open-file (stream logfile
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :overwrite)
      (loop for key being the hash-keys of hash-table
            if (not (= 0 (cdr (gethash key hash-table))))
            do (progn
                 (write-line
                  (format nil "The error rate for ~a is ~,2f"
                          key (/ (car (gethash key hash-table))
                                 (cdr (gethash key hash-table))))
                  stream)
                 (force-output stream))
            else
            do (progn
                 (write-line
                  (format nil "The error rate for ~a is ~,2f" key 0) stream)
                 (force-output stream))))))

(defun get-result (cipn answer computed-answer)
  (if (and (find 'fcg::succeeded (fcg::statuses cipn))
           (string= (upcase answer)
                    (upcase computed-answer)))
      1 0))

(defun write-result (log scene-name q answer computed-answer result irl-program hash-table)
  (if (= result 1)
    (progn (adjust-primitive-frequencies irl-program hash-table)
      (write-line (format nil "~a,~a,~a,~a,~a" scene-name q answer computed-answer result) log)
      (force-output log))
    (progn (adjust-primitive-frequencies irl-program hash-table)
      (write-line (format nil "~a,~a,~a,~a,~a" scene-name q answer computed-answer result) log)
      (force-output log)
      (adjust-primitive-errors irl-program hash-table))))

;; Compute the accuracy on a specified number of scenes or questions
(defun compute-accuracy (log clevr-world ontology hash-table
                             &key nr-of-scenes nr-of-questions)
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
                                for computed-answer = (compute-answer irl-program scene-var path-entity ontology)
                                for result = (get-result cipn answer computed-answer)
                                do (incf processed-questions)
                                   (format t ".")
                                if (and nr-of-questions (>= processed-questions nr-of-questions))
                                  return scene-accuracy
                                else if (= result 1)
                                       collect 1 into scene-accuracy
                                       and do (write-result log scene-name q answer computed-answer result irl-program hash-table)
                                  else collect 0 into scene-accuracy
                                       and do (write-result log scene-name q answer computed-answer result irl-program hash-table)
                                finally (return scene-accuracy))
                     into accuracy
                 do (incf processed-scenes)
                 finally (return accuracy)))))

;; make an empty error-table
(defun make-error-table ()
  (let ((error-table (make-hash-table)))
    (setf (gethash 'count! error-table) '(0 . 0))
    (setf (gethash 'equal? error-table) '(0 . 0))
    (setf (gethash 'equal-integer error-table) '(0 . 0))
    (setf (gethash 'less-than error-table) '(0 . 0))
    (setf (gethash 'greater-than error-table) '(0 . 0))
    (setf (gethash 'exist error-table) '(0 . 0))
    (setf (gethash 'filter error-table) '(0 . 0))
    (setf (gethash 'intersect error-table) '(0 . 0))
    (setf (gethash 'query error-table) '(0 . 0))
    (setf (gethash 'relate error-table) '(0 . 0))
    (setf (gethash 'same error-table) '(0 . 0))
    (setf (gethash 'union! error-table) '(0 . 0))
    (setf (gethash 'unique error-table) '(0 . 0))
    error-table))
  


(defgeneric evaluate-mwm-accuracy (ontology &key data-split csv-filename errors-filename
                                            nr-of-scenes nr-of-questions)
  (:documentation "Evaluate the accuracy of the mwm-concepts."))


(defmethod evaluate-mwm-accuracy (ontology
                                  &key (data-split "val")
                                  (csv-filename "mwm-evaluation")
                                  (errors-filename "mwm-errors")
                                  nr-of-scenes nr-of-questions)
  (let ((clevr-world
         (make-instance 'clevr-world
                        :data-sets (list data-split)
                        :load-questions t))
        (accuracy 0)
        (error-table (make-error-table))
        (logfile
         (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                         :name csv-filename :type "txt")))
    (ensure-directories-exist logfile)
    (with-open-file (log logfile :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
      (setf accuracy (compute-accuracy log clevr-world ontology error-table
                                       :nr-of-scenes nr-of-scenes
                                       :nr-of-questions nr-of-questions)))
    (compute-error-rate error-table errors-filename)
    accuracy))



(defun evaluate-mwm-serie (serie-number)
  (let* ((serie-name
          (format nil "serie-~a" serie-number))
         (concepts-directory
          (merge-pathnames
           (make-pathname :directory (list :relative serie-name))
           *simulated-concepts-path*))
         (ontology
          (make-mwm-ontology concepts-directory))
         (output-filename (format nil "mwm-evaluation-~a" serie-number))
         (error-filename (format nil "mwm-errors-~a" serie-number)))
    (evaluate-mwm-accuracy ontology
                           :csv-filename  output-filename
                           :errors-filename error-filename)))


(defun evaluate-all-series ()
  (loop for serie-nr from 1 to 10
        for serie = (format nil "serie-~a" serie-nr)
        for ontology = (make-mwm-ontology
                        (merge-pathnames
                         (make-pathname :directory (list :relative serie))
                         *simulated-concepts-path*))
        do (evaluate-mwm-accuracy ontology
                                  :csv-filename (concatenate 'string serie "-evaluation")
                                  :errors-filename (concatenate 'string serie "-errors"))))



