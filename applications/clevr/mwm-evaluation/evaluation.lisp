(in-package :mwm-evaluation)


;;------------------------;;
;; Default Configurations ;;
;;------------------------;;
(define-configuration-default-value :dot-interval 100)
(define-configuration-default-value :nr-of-scenes nil)
(define-configuration-default-value :nr-of-questions nil)
(define-configuration-default-value :data-split "val")
(define-configuration-default-value :world-type :simulated)


#|
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

(defun write-result (log scene-name q answer computed-answer result irl-program hash-table)
  (if (= result 1)
    (progn (adjust-primitive-frequencies irl-program hash-table)
      (write-line (format nil "~a,~a,~a,~a,~a" scene-name q answer computed-answer result) log)
      (force-output log))
    (progn (adjust-primitive-frequencies irl-program hash-table)
      (write-line (format nil "~a,~a,~a,~a,~a" scene-name q answer computed-answer result) log)
      (force-output log)
      (adjust-primitive-errors irl-program hash-table))))
|#

;;------------;;
;; Evaluation ;;
;;------------;;
;; Compute the accuracy on the clevr dataset using the learned concepts
(defun answer->str (answer-value)
  "Make a string from the computed answer so
   that it can be compared to the ground-truth string"
  (if (numberp answer-value)
    (mkstr answer-value)
    (case #+lispworks (type-of answer-value)
      #+ccl (if (listp (type-of answer-value))
              (first (type-of answer-value))
              (type-of answer-value))
      #+sbcl (if (listp (type-of answer-value))
               (first (type-of answer-value))
               (type-of answer-value))
      (shape-concept (mkstr (id answer-value)))
      (size-concept (mkstr (id answer-value)))
      (color-concept (mkstr (id answer-value)))
      (material-concept (mkstr (id answer-value)))
      (boolean-category (mkstr (id answer-value))))))


(defun compute-answer (irl-program scene-var scene-path-entity ontology)
  "Given an irl-program, a variable and a scene path,
   compute the answer."
  (let* ((irl-program-with-scene
          (cons `(bind pathname-entity ,scene-var ,scene-path-entity)
                (substitute-categories irl-program)))
         (solutions
          (evaluate-irl-program irl-program-with-scene ontology :n 1
                                :primitive-inventory *mwm-primitives*)))
    (when (and solutions (length= solutions 1))
      (let* ((target-var (get-target-var irl-program))
             (target-value (value (find target-var (first solutions) :key #'var))))
        (answer->str target-value)))))


(defun get-result (cipn answer computed-answer)
  (if (and (find 'fcg::succeeded (fcg::statuses cipn))
           (string= (upcase answer)
                    (upcase computed-answer)))
      1 0))


(define-event question-evaluation
  (scene-name string) (question string) (irl-program list)
  (answer t) (computed-answer t) (result fixnum))


;; Compute the accuracy on a specified number of scenes or questions
(defun compute-accuracy (clevr-world ontology configurations)
  (let ((nr-of-scenes (get-configuration configurations :nr-of-scenes))
        (nr-of-questions (get-configuration configurations :nr-of-questions)))
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
                                     (notify interaction-started configurations t processed-questions)
                                     (notify question-evaluation scene-name q irl-program answer computed-answer result)
                                     (notify interaction-finished configurations t processed-questions)
                                  collect result into scene-accuracy
                                  when (and nr-of-questions (>= processed-questions nr-of-questions))
                                    return scene-accuracy
                                  finally (return scene-accuracy))
                       into accuracy
                   do (incf processed-scenes)
                   finally
                     (return accuracy))))))







(defun evaluate-mwm-accuracy (ontology config)
  (notify reset-monitors)
  (let* ((clevr-world
          (make-instance 'clevr-world :load-questions t
                         :data-sets (list (get-configuration config :data-split))))   
         (accuracy
          (compute-accuracy clevr-world ontology config)))
    (notify series-finished 1)
    (notify batch-finished (get-configuration config :experiment-name))
    accuracy))


(defparameter *default-output-dir*
  (babel-pathname :directory '("applications" "clevr" "mwm-evaluation" "raw-data")))


(defun evaluate-mwm-serie (serie-number config-entries
                           &key (monitors (get-all-monitors))
                           (output-dir *default-output-dir*))
  (let* ((experiment-name
          (format nil "serie-~a" serie-number))
         (config
          (make-configuration
           :entries (cons (cons :experiment-name experiment-name)
                          config-entries)))
         (world-type
          (get-configuration config :world-type))
         (concepts-directory
          (merge-pathnames
           (make-pathname :directory (list :relative experiment-name))
           (case world-type
             (:simulated *simulated-concepts-path*)
             (:extracted *extracted-concepts-path*))))
         (ontology
          (make-mwm-ontology concepts-directory world-type)))
    ;; adapt file-writing monitors so they output in the correct output-dir
    ;(monitors::deactivate-all-monitors)
    (loop for monitor-string in monitors
          for monitor = (monitors::get-monitor (read-from-string monitor-string))
          do (monitors::activate-monitor-method (read-from-string monitor-string))
          when (slot-exists-p monitor 'file-name)
          do (setf (slot-value monitor 'file-name)
                   (ensure-directories-exist
                    (merge-pathnames
                     (make-pathname :directory 
                                    `(:relative ,(string-downcase experiment-name))
                                    :name (pathname-name (file-name monitor)) 
                                    :type (pathname-type (file-name monitor)))
                     output-dir))))
    ;; run the evaluation
    (evaluate-mwm-accuracy ontology config)))


(defun evaluate-all-series (config-entries)
  (loop for serie-nr from 1 to 10
        do (evaluate-mwm-serie serie-nr config-entries)))


;;---------;;
;; Metrics ;;
;;---------;;

(defun compute-accuracy-per-primitive (serie-number)
  (let ((logfiles
         (directory
          (merge-pathnames
           (make-pathname :directory `(:relative ,(format nil "serie-~a" serie-number))
                          :name :wild :type "log")
           *default-output-dir*)))
        results)
    (loop for file in logfiles
          for primitive = (intern (upcase (pathname-name file)))
          do (with-open-file (stream file :direction :input)
               (let* ((data (caar (read stream)))
                      (accuracy (average (remove nil data))))
                 (push (cons primitive accuracy) results))))
    results))
    



