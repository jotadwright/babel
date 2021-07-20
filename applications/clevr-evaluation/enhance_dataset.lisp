(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)
(deactivate-all-monitors)

(defun answer-question-in-scene (meaning scene)
  (set-data *clevr-ontology* 'clevr-context scene)
  (let ((solutions
         (evaluate-irl-program meaning *clevr-ontology*
                               :primitive-inventory *clevr-primitives*)))
    (if (and solutions (= (length solutions) 1))
      (let* ((answer (answer->str
                      (get-target-value meaning (first solutions)))))
        (values (name scene) (mkstr (downcase answer))))
      (values nil nil))))

(defun set-depth-first-configurations (cxn-inventory)
  (set-configurations cxn-inventory
                      '((:cxn-supplier-mode . :ordered-by-label-hashed)
                        (:priority-mode . :nr-of-applied-cxns)
                        (:parse-order hashed nom cxn)
                        (:production-order hashed-lex nom cxn hashed-morph)
                        (:max-nr-of-nodes . 10000))))
  
(defun enhance-data (inputfile outputdir dataset)
  ;; set the depth first configuration
  (set-depth-first-configurations *fcg-constructions*)
  ;; open streams
  (let ((in-stream (open inputfile :direction :input))
        (world (make-instance 'clevr-world :data-sets (list dataset))))
    ;; skip the header line
    (read-line in-stream nil nil)
    ;; loop over each line
    (loop for line = (read-line in-stream nil nil)
          for line-counter from 0
          while line
          for fields = (split line #\,)
          for question = (second fields)
          for meaning = (read-from-string (third fields))
          ;; comprehend it to get the meaning
          ;for (meaning cipn) = (multiple-value-list (comprehend question))
          ;when (eql (first (statuses cipn)) 'fcg::succeeded)
          ;; execute the meaning in every scene
          do (with-progress-bar (bar (length (scenes world))
                                     ("Processing question \"~a\"" question))
               (let* ((counter 0)
                      (scenes nil)
                      (answers nil)
                      (outputname (format nil "question-~6,'0d" line-counter))
                      (outputfile (merge-pathnames
                                   (make-pathname :name outputname :type "lisp")
                                   outputdir)))
                 (ensure-directories-exist outputfile)
                 (do-for-scenes
                  world (lambda (scene)
                          (multiple-value-bind (scene-name answer)
                              (answer-question-in-scene meaning scene)
                            (update bar)
                            ;; when an answer is computed, store it
                            (when answer
                              (incf counter)
                              (push scene-name scenes)
                              (push answer answers)))
                          t))
                 (format t "Storing ~a lines~%" counter)
                 (with-open-file (out-stream outputfile :direction :output
                                             :if-exists :supersede)
                   (let ((data (list question (downcase (mkstr meaning))
                                     (pairlis scenes answers))))
                     (write data :stream out-stream)
                     (force-output out-stream)))
                 (sleep 10))))))

(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))

(defun main (args)
  (let ((arg-plist (args->plist args)))
    (enhance-data (parse-namestring (getf arg-plist 'inputfile))
                  (parse-namestring (getf arg-plist 'outputdir))
                  (getf arg-plist 'dataset))))
                  
#-lispworks
(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))

                                       