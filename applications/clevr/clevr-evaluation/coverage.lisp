(in-package :clevr-evaluation)

;;;; evaluate coverage

(defgeneric evaluate-clevr-coverage (data-split direction
                                     &key nr-of-questions)
  (:documentation "Evaluate the coverage of the clevr grammar."))

(defmethod evaluate-clevr-coverage (data-split (direction (eql '<-))
                                    &key nr-of-questions)
  (let ((clevr-world
         (make-instance 'clevr-world
                        :data-sets (list data-split)
                        :load-questions t))
        (logfile
         (babel-pathname :directory '("applications" "clevr" "clevr-evaluation" "logs")
                         :name "comprehension-coverage" :type "log")))
    (ensure-directories-exist logfile)
    (with-open-file (log logfile :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
      (average
       (remove nil
               (loop with processed-questions = 0
                     for filename in (question-sets clevr-world)
                     for set-of-questions = (load-clevr-question-set filename)
                     if (and nr-of-questions (>= processed-questions nr-of-questions))
                     return coverage
                     else
                     append (loop for clevr-question in (questions set-of-questions)
                                  for q = (question clevr-question)
                                  for (nil cipn nil)
                                  = (multiple-value-list
                                     (understand q *clevr* '?scene))
                                  do (incf processed-questions)
                                  (format t ".")
                                  if (and nr-of-questions (>= processed-questions nr-of-questions))
                                  return question-set-coverage
                                  else if (find 'fcg::succeeded (fcg::statuses cipn))
                                  collect 1 into question-set-coverage
                                  else collect 0 into question-set-coverage
                                  and do (write-line q log)
                                  finally (return question-set-coverage))
                     into coverage
                     finally (return coverage)))))))
    

(defmethod evaluate-clevr-coverage (data-split (direction (eql '->))
                                    &key nr-of-questions)
  (let ((clevr-world
         (make-instance 'clevr-world
                        :data-sets (list data-split)
                        :load-questions t))
        (logfile
         (babel-pathname :directory '("applications" "clevr" "clevr-evaluation" "logs")
                         :name "formulation-coverage" :type "log")))
    (ensure-directories-exist logfile)
    (with-open-file (log logfile :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
    (average
     (remove nil
             (loop with processed-questions = 0
                   for filename in (question-sets clevr-world)
                   for set-of-questions = (load-clevr-question-set filename)
                   if (and nr-of-questions (>= processed-questions nr-of-questions))
                   return coverage
                   else
                   append (loop for clevr-question in (questions set-of-questions)
                                for q = (question clevr-question)
                                for (nil cipn nil)
                                = (multiple-value-list
                                   (understand-and-formulate q *clevr* '?scene))
                                do (incf processed-questions)
                                (format t ".")
                                if (and nr-of-questions (>= processed-questions nr-of-questions))
                                return question-set-coverage
                                else if (find 'fcg::succeeded (fcg::statuses cipn))
                                collect 1 into question-set-coverage
                                else collect 0 into question-set-coverage
                                and do (write-line q log)
                                finally (return question-set-coverage))
                   into coverage
                   finally (return coverage)))))))