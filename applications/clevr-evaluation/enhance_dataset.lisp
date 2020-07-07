(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)


;; loop over all questions and meanings using the seq2seq data file
;;   loop over all scenes using clevr-world
;;     execute the meaning in the scene
;;     if it leads to an answer, store it


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
  
(defun main ()
  (let ((out-stream
         (open
          (babel-pathname :directory '(".tmp")
                          :name "CLEVR_val_enhanced"
                          :type "csv")
          :direction :output))
        (world
         (make-instance 'clevr-world
                        :data-sets '("val"))))
    (do-for-scenes world
                   (lambda (scene)
                     (let ((in-stream
                            (open
                             (parse-namestring
                              "/Users/jensnevens/Projects/seq2seq/data/CLEVR_val_v2.csv")
                             :direction :input))
                           (counter 0))
                       (read-line in-stream nil nil)
                       (with-progress-bar (bar 150000 ("Evaluating scene ~a~%" (name scene)))
                         (loop for line = (read-line in-stream nil nil)
                               while line
                               for fields = (split line #\,)
                               for question = (second fields)
                               for (meaning cipn) = (multiple-value-list (comprehend question :cxn-inventory *clevr*))
                               do (update bar)
                               when (eql (first (statuses cipn)) 'fcg::succeeded)
                               do (let ((out-line (answer-question-in-scene question meaning scene)))
                                    (when out-line
                                      (incf counter)
                                      (write-line out-line out-stream)
                                      (force-output out-stream)))))
                       (format t "Stored ~a question-answer pairs for scene ~a~%"
                               counter (name scene))
                       (close in-stream))))
    (force-output out-stream)
    (close out-stream)))

;(main)

                                       