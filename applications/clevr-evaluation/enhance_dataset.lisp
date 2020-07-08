(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)
(deactivate-all-monitors)

(defun answer-question-in-scene (question meaning scene)
  (set-data *clevr-ontology* 'clevr-context scene)
  (let ((solutions
         (evaluate-irl-program meaning *clevr-ontology*)))
    (when (and solutions (= (length solutions) 1))
      (let* ((answer (answer->str
                      (get-target-value meaning (first solutions))))
             (line (format nil "~a,~a,~a,~a"
                           question
                           (mkstr (downcase meaning))
                           (name scene)
                           (mkstr (downcase answer)))))
        line))))
  
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
               (let ((counter 0))
                 (do-for-scenes
                  world (lambda (scene)
                          (let ((out-line (answer-question-in-scene question meaning scene)))
                            (update bar)
                            ;; when an answer is computed, store it
                            (when out-line
                              (incf counter)
                              (write-line out-line out-stream)
                              (force-output out-stream)))
                          t))
                 (format t "Stored ~a lines~%" counter))))))

(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))

(defun main (args)
  (print args))
  ;(let ((arg-plist (args->plist args)))
  ;  (enhance-data (parse-namestring (getf arg-plist 'inputfile))
  ;                (parse-namestring (getf arg-plist 'outputfile))
  ;                (parse-integer (getf arg-plist 'seq2seq-port)))))
                  

(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))

                                       