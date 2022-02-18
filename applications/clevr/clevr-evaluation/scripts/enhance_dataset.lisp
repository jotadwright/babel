;;;; enhance_dataset.lisp

(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)
(deactivate-all-monitors)

(defun answer-question-in-scene (meaning scene)
  (set-data *clevr-ontology* 'clevr-context scene)
  (let ((solutions
         (evaluate-irl-program meaning *clevr-ontology* :silent t :n 1
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

(defparameter *stage-1-primitives*
  '(count! exist filter get-context query unique))
(defparameter *stage-2-primitives*
  '(count! exist filter get-context 
           query relate same unique))
(defparameter *stage-3-primitives*
  '(count! equal-integer less-than greater-than
           equal? exist filter get-context intersect
           query relate same union! unique))

(defun assign-meaning-to-stage-dir (meaning)
  (let ((predicates (remove 'bind (mapcar #'car meaning))))
    (make-pathname
     :directory
     (list
      :relative
      (cond ((loop for p in predicates
                   always (find p *stage-1-primitives*))
             ;; move to stage-1 dir
             "stage-1")
            ((loop for p in predicates
                   always (find p *stage-2-primitives*))
             ;; move to stage-2 dir
             "stage-1")
            (t
             ;; move to stage-3 file
             "stage-3"))))))

    
(defun enhance-data (inputfile outputdir dataset start end)
  ;; set the depth first configuration for *fcg-constructions*
  (set-depth-first-configurations *fcg-constructions*)
  ;; make the clevr world and open a stream to the input file
  (let ((world (make-instance 'clevr-world :data-sets (list dataset)))
        (in-stream (open inputfile :direction :input))
        (lines-to-process (- end start)))
    ;; skip the first N lines, according to start
    (format t "~%Skipping the first ~a lines" start)
    (loop repeat (1+ start) do (read-line in-stream nil nil))
    ;; from then on, process N lines, according to start and end
    (format t "~%Processing ~a lines" lines-to-process)
    (loop repeat lines-to-process
          for line = (read-line in-stream nil nil)
          while line
          for fields = (split line #\,)
          for question-index = (parse-integer (first fields))
          for question = (second fields)
          for meaning = (read-from-string (third fields))
          for outputname = (format nil "question-~6,'0d" question-index)
          for stagedir = (assign-meaning-to-stage-dir meaning)
          for outputfile = (merge-pathnames
                            (merge-pathnames
                             (make-pathname :name outputname :type "lisp")
                             stagedir)
                            outputdir)
          for counter = 0
          for scenes = nil
          for answers = nil
          do (ensure-directories-exist outputfile)
          do (with-progress-bar (bar (length (scenes world))
                                     ("Processing question ~a: \"~a\""
                                      question-index question))
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
                                         t)))
          do (format t "~%Storing ~a lines" counter)
          do (with-open-file (out-stream outputfile :direction :output
                                         :if-exists :supersede)
               (let ((data (list question meaning (pairlis scenes answers))))
                 (write data :stream out-stream)
                 (force-output out-stream))))))

#|
(enhance-data
 (parse-namestring "/Users/jensnevens/Babel-Corpora/seq2seq/data/CLEVR_train_corpus_comprehension.csv")
 (parse-namestring "/Users/jensnevens/Babel-Corpora/clevr-learning-data/train/")
 "train" 0 10)
|#


(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))

(defun main (args)
  (let ((arg-plist (args->plist args)))
    (enhance-data (parse-namestring (getf arg-plist 'inputfile))
                  (parse-namestring (getf arg-plist 'outputdir))
                  (getf arg-plist 'dataset)
                  (parse-integer (getf arg-plist 'start))
                  (parse-integer (getf arg-plist 'end)))))
                  
#-lispworks
(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))

                                       
