(ql:quickload :propbank-grammar)
(ql:quickload :bordeaux-threads)
(in-package :propbank-grammar)

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "learning-grammars-sa")
                      :name "parameter-learn-eval" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "learning-grammars-sa")
                      :name "simulated-annealing" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "learning-grammars-sa")
                      :name "parallel-simulated-annealing" :type "lisp"))

(defun single-grammar-predict (grammar-file-name &key (test-batch-size 100) (propbank-sentences *dev-corpus*) (test-grammar test-grammar) (random-number 0000) (num-threads 4))
  "Evaluates a single PropBank grammar using specified parameters and returns the F1 score.
   Arguments:
   - GRAMMAR-FILE-NAME: The output file name for the grammar.
   - TEST-BATCH-SIZE (optional): The batch size for testing.
   - PROPBANK-SENTENCES (optional): The sentences from PropBank to use for testing.
   - TEST-GRAMMAR (optional): The grammar to test.
   - RANDOM-NUMBER (optional): A random number for the evaluation process.
   - NUM-THREADS (optional): The number of threads to use for the evaluation."
  (let* ((random-number random-number)
         (test-grammar test-grammar))
      (let* ((test-sentences propbank-sentences)
             (f1-scores nil)
             (f1-scores-loose nil))
        (loop for i below (ceiling (length test-sentences) test-batch-size)
              for batch = (subseq test-sentences (* i test-batch-size) (* (1+ i) test-batch-size))
              do (let ((predictions-comprehend (comprehend-propbank-corpus-parameters test-grammar grammar-file-name batch :random-number random-number :num-threads num-threads)))
                   (push (evaluate-predictions-f1 predictions-comprehend) f1-scores)
                   (push (evaluate-predictions-f1 predictions-comprehend :include-word-sense nil :include-timed-out-sentences nil) f1-scores-loose)))
        (let ((final-f1 (/ (reduce #'+ f1-scores) (length f1-scores))))
          (format t "Final mean F1 score for current combination: ~a ~A~%" grammar-file-name final-f1)
          (store-f1-scores-batch grammar-file-name f1-scores test-batch-size)
          (store-f1-scores-batch-loose grammar-file-name f1-scores-loose test-batch-size)
          final-f1))))

(defun comprehend-sentences-worker (start end cxn-inventory sentences silent timeout)
  "Works on a subset of sentences, comprehending each one and extracting frames.
   Arguments:
   - START: The index of the first sentence to process.
   - END: The index of the last sentence to process.
   - CXN-INVENTORY: The construction inventory.
   - SENTENCES: The list of sentences to process.
   - SILENT: If true, does not print any output.
   - TIMEOUT: The timeout for each comprehension operation."
  (loop for index from start below end
        for sentence = (elt sentences index)
        for sentence-number = (1+ index)
        do (format t "~%Sentence ~a: ~a" sentence-number (sentence-string sentence))
        collect (let* ((cipn (second (multiple-value-list (comprehend-and-extract-frames sentence :cxn-inventory cxn-inventory :silent silent :timeout timeout))))
                       (annotation (propbank-frames sentence)))
                  (if (eql cipn 'time-out)
                      (progn (format t " --> timed out .~%")
                             (list sentence annotation 'time-out))
                      (let ((solution (remove-if-not #'frame-with-name (frames (extract-frames (car-resulting-cfs (cipn-car cipn)))))))
                        (format t " --> done .~%")
                        (list sentence annotation solution))))))


(defun comprehend-propbank-corpus-parameters (cxn-inventory grammar-name &optional (list-of-propbank-sentences *dev-corpus*) &key (output-file nil) (timeout 60) (silent t) (random-number 0000) (num-threads 4))
  "Makes predictions for a PropBank grammar using specified parameters and saves the predictions.
   Arguments:
   - CXN-INVENTORY: The construction inventory.
   - GRAMMAR-NAME: The current combination of settings.
   - LIST-OF-PROPBANK-SENTENCES (optional): The sentences from PropBank to use for making predictions.
   - OUTPUT-FILE (optional): The file path for the output file.
   - TIMEOUT (optional): The timeout for each prediction.
   - SILENT (optional): If true, does not print any output.
   - RANDOM-NUMBER (optional): A random number for the prediction process.
   - NUM-THREADS (optional): The number of threads to use for the prediction."
  (let* ((total-sentences (length list-of-propbank-sentences))
         (sentences-per-thread (ceiling total-sentences num-threads))
         (predictions nil)
         (threads (list)))

    ;; Create threads and distribute sentences
    (dotimes (i num-threads)
      (let ((start (* i sentences-per-thread))
            (end (min (* (1+ i) sentences-per-thread) total-sentences)))
        (push (bt:make-thread (lambda () (comprehend-sentences-worker start end cxn-inventory list-of-propbank-sentences silent timeout))) threads)))

    ;; Join threads and collect results
    (dolist (thread threads)
      (setq predictions (append predictions (bt:join-thread thread))))

    ;; Save predictions
    (let* ((filename (format nil "~a" grammar-name))
           (filename-string (namestring filename))
           (filename-length (length filename-string))
           (filename-cutoff (min filename-length 240))
           (filename-final (subseq filename-string 0 filename-cutoff))
           (output-file (format nil (format nil "~a~a-prd~a~a" (babel-pathname :directory '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "predictions-config-eval")
                                                                    ) filename-final random-number ".store"))))
      (with-open-file (out-stream output-file :direction :output :if-does-not-exist :create :if-exists :append)
        (let ((binary-stream (flexi-streams:make-flexi-stream out-stream :element-type '(unsigned-byte 8))))
          (cl-store:store predictions binary-stream))))
    predictions))


