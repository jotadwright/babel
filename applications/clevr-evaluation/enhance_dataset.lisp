(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)
(deactivate-all-monitors)

(defun answer-question-in-scene (question meaning scene)
  (set-data *clevr-ontology* 'clevr-context scene)
  (let ((solutions
         (evaluate-irl-program meaning *clevr-ontology*)))
    (if (and solutions (= (length solutions) 1))
      (let* ((answer (answer->str
                      (get-target-value meaning (first solutions)))))
        (values (name scene) (mkstr (downcase answer))))
      (values nil nil))))
  
(defun enhance-data (inputfile outputfile seq2seq-server-port)
  ;; set the seq2seq endpoint
  (set-configuration *fcg-constructions* :seq2seq-endpoint
                     (format nil "http://localhost:~a/next-cxn"
                             seq2seq-server-port)
                     :replace t)
  ;; open streams
  (let ((in-stream
         (open inputfile
               :direction :input))
        (out-stream
         (open outputfile
               :direction :output
               :if-exists :supersede))
        (world
         (make-instance 'clevr-world
                        :data-sets '("val"))))
    ;; skip the header line
    (read-line in-stream nil nil)
    ;; write the opening bracket to the output file
    (write-string "[" out-stream)
    ;; loop over each line
    (loop for line = (read-line in-stream nil nil)
          while line
          for fields = (split line #\,)
          for question = (second fields)
          ;; comprehend it to get the meaning
          for (meaning cipn) = (multiple-value-list (comprehend question))
          when (eql (first (statuses cipn))
                    'fcg::succeeded)
          ;; execute the meaning in every scene
          do (with-progress-bar (bar (length (scenes world))
                                     ("Processing question \"~a\"" question))
               (let ((counter 0)
                     (scenes nil)
                     (answers nil))
                 (do-for-scenes
                  world (lambda (scene)
                          (multiple-value-bind (scene-name answer)
                              (answer-question-in-scene question meaning scene)
                            (update bar)
                            ;; when an answer is computed, store it
                            (when answer
                              (incf counter)
                              (push scene-name scenes)
                              (push answer answers)))
                          t))
                 (format t "Storing ~a lines~%" counter)
                 (write-line
                  (encode-json-alist-to-string
                   `((question . ,question)
                     (meaning . ,(downcase (mkstr meaning)))
                     (answers ,@(loop for (scene-name . answer) in (pairlis scenes answers)
                                      collect `((scene . ,scene-name)
                                                (answer . ,answer))))))
                  out-stream))
               (force-output out-stream)
               (sleep 10)))
    (write-string "]" out-stream)
    (force-output out-stream)))

(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))

(defun main (args)
  (let ((arg-plist (args->plist args)))
    (enhance-data (parse-namestring (getf arg-plist 'inputfile))
                  (parse-namestring (getf arg-plist 'outputfile))
                  (parse-integer (getf arg-plist 'seq2seq-port)))))
                  

(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))

                                       