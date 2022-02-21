(in-package :clevr-evaluation)

;;;; evaluate accuracy

(defgeneric evaluate-clevr-accuracy (data-split &key nr-of-scenes nr-of-questions)
  (:documentation "Evaluate the accuracy of the clevr grammar."))

(defmethod evaluate-clevr-accuracy (data-split &key nr-of-scenes nr-of-questions)
  (let ((clevr-world
         (make-instance 'clevr-world
                        :data-sets (list data-split)
                        :load-questions t))
        (logfile
         (babel-pathname :directory '("applications" "clevr" "clevr-evaluation" "logs")
                         :name "accuracy" :type "log")))
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
                                do (incf processed-questions)
                                (format t ".")
                                if (and nr-of-questions (>= processed-questions nr-of-questions))
                                return scene-accuracy
                                else if (and (find 'fcg::succeeded (fcg::statuses cipn))
                                             (string= (upcase answer)
                                                      (upcase (compute-answer irl-program scene-var path-entity))))
                                collect 1 into scene-accuracy
                                else collect 0 into scene-accuracy
                                and do (progn (write-line (format nil "~a - ~a" scene-name q) log)
                                         (force-output log))
                                finally return scene-accuracy)
                   into accuracy
                   do (incf processed-scenes)
                   finally return accuracy))))))