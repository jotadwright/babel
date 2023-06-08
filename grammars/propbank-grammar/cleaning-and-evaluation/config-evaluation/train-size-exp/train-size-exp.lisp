(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation")
                      :name "parameter-learn-eval" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation")
                      :name "simulated-annealing" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation")
                      :name "parallel-simulated-annealing" :type "lisp"))

;; Functions for running train-size evaluation experiments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn-comprehend-evaluate-grammar-train-size (max-train-set &key (output-file-exp "results-train-size-exp.csv") (test-corpus *dev-corpus*) (train-corpus *train-corpus*) (step-size 5000) (fcg-configuration *training-configuration-all*))
  "Perform PropBank grammar learning and prediction with an incrementally increasing training set size while keeping the test set size constant. 

  The function takes a maximum training set size (max-train-set) as the primary argument and has several optional keyword arguments:
    - output-file-exp: specifies the output file name for the results. Default is 'results-train-size-exp.csv'.
    - test-corpus: specifies the test corpus to be used. Default is *dev-corpus*.
    - train-corpus: specifies the training corpus to be used. Default is *train-corpus*.
    - step-size: specifies the increment size for the training set. Default is 5000.
    - fcg-configuration: specifies the training configuration to be used. Default is *training-configuration-all*.
  Returns a list of prediction f1-scores for the development and training set."
  (let* ((train-set-size 0)
         (predictions-f1-scores-dev (list))
         (predictions-f1-scores-train (list))
         (start-time (get-internal-real-time))
         (total-runtime nil))
    (loop while (< train-set-size max-train-set)
          do
          (setq step-size (dynamic-step-size train-set-size))
          (setq train-set-size (min (+ train-set-size step-size) max-train-set))
          (let* ((train-corpus-subset (subseq train-corpus 0 train-set-size)))
            (format t "Training set size: ~a~%" train-set-size)
            (learn-propbank-grammar-roles train-corpus-subset :cxn-inventory 'test-grammar :fcg-configuration fcg-configuration)
            (store-learned-grammar-train-size train-set-size test-grammar)
            (let ((predictions-comprehend-dev (comprehend-propbank-corpus-parameters-train-size test-grammar (length train-corpus-subset) test-corpus :fcg-configuration fcg-configuration))
                  (predictions-comprehend-train (comprehend-propbank-corpus-parameters-train-size test-grammar (length train-corpus-subset) (subseq train-corpus 0 (length test-corpus)) :fcg-configuration fcg-configuration)))
              (push (list :set-size train-set-size :test-corpus 'dev :f1-score (evaluate-predictions-f1 predictions-comprehend-dev :core-roles-only nil :include-timed-out-sentences nil :include-sentences-with-incomplete-role-constituent-mapping nil)) predictions-f1-scores-dev)
              (push (list :set-size train-set-size :test-corpus 'train :f1-score (evaluate-predictions-f1 predictions-comprehend-train :core-roles-only nil :include-timed-out-sentences nil :include-sentences-with-incomplete-role-constituent-mapping nil)) predictions-f1-scores-train))))
    (setf total-runtime (/ (- (get-internal-real-time) start-time) internal-time-units-per-second))
    (store-run-info-size total-runtime step-size max-train-set (length test-corpus) predictions-f1-scores-dev predictions-f1-scores-train)
    (format t "Total runtime: ~f seconds~%" total-runtime)
    (list predictions-f1-scores-dev predictions-f1-scores-train)
    (save-prediction-f1-scores-to-csv (list predictions-f1-scores-dev predictions-f1-scores-train) output-file-exp)))

(defun dynamic-step-size (train-set-size)
  "Determine the increment size for the training set based on the current size of the training set. 

  The function takes the current training set size (train-set-size) as an argument. 
  Returns the step size based on a pre-defined range."
  (cond ((< train-set-size 500) 500)
        ((< train-set-size 1000) 500)
        ((< train-set-size 1500) 500)
        ((< train-set-size 2000) 500)
        ((< train-set-size 3000) 1000)
        ((< train-set-size 4000) 1000)
        ((< train-set-size 5000) 1000)
        ((< train-set-size 7500) 2500)
        ((< train-set-size 10000) 2500)
        ((< train-set-size 15000) 5000)
        ((< train-set-size 25000) 10000)
        ((< train-set-size 50000) 25000)
        ((< train-set-size 75000) 25000)
        ((< train-set-size 100000) 25000)
        ((< train-set-size 150000) 50000)
        (t 20000)))

(defun test-dynamic-step-sizes (max-train-size)
  "Test the dynamic-step-size function by continuously incrementing the training set size up to a maximum size (max-train-size). 

  The function takes the maximum training set size as an argument. 
  Prints the current training set size and step size at each increment."
  (let ((train-set-size 0)
        (step-size 0))
    (loop while (< train-set-size max-train-size)
          do
          (setq step-size (dynamic-step-size train-set-size))
          (format t "Train set size: ~a, Step size: ~a~%" train-set-size step-size)
          (setq train-set-size (+ train-set-size step-size)))))

(defun comprehend-sentences-worker (start end cxn-inventory sentences silent timeout)
  "Conduct comprehension tasks for a subset of sentences. 

  Takes as input the start and end indices of the sentences, the cxn-inventory, the list of sentences, a silent flag, and a timeout. 
  Processes each sentence, extracts frames, and collects comprehension results. Returns a list of sentence comprehension results."
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

(defun comprehend-propbank-corpus-parameters-train-size (cxn-inventory train-corpus-subset &optional (list-of-propbank-sentences *dev-corpus*) &key (output-file nil) (timeout 60) (silent t) (num-threads 4) (fcg-configuration fcg-configuration))
  "Conduct comprehension tasks on a PropBank corpus using specified configs and a training corpus subset. 

  Arguments include cxn-inventory, train-corpus-subset, and optionally, a list of PropBank sentences, with several keyword arguments:
    - output-file: specifies the output file name for the results.
    - timeout: specifies the timeout value. Default is 60.
    - silent: specifies if output should be silent. Default is t.
    - num-threads: specifies the number of threads to be used. Default is 4.
    - fcg-configuration: specifies the training configuration to be used.
  Returns the prediction results."
  (let* ((predictions (make-array (length list-of-propbank-sentences) :adjustable t :fill-pointer 0))
         (corpus-name (if (equal list-of-propbank-sentences *dev-corpus*)
                          "dev"
                          "train"))
         (filename (format nil "~a~a-corpus-prd~a" (get-configuration cxn-inventory :heuristics) corpus-name train-corpus-subset))
         (output-file (or output-file
                          (babel-pathname :directory '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "predictions-config-eval")
                                          :name filename)))
         (pathname-string (namestring output-file))
         (pathname-length (length pathname-string))
         (pathname-cutoff (min pathname-length 255))
         (output-file (subseq pathname-string 0 pathname-cutoff))
         (output-file (format nil (format nil "~a~a" output-file ".store")))
         (sentences-per-thread (/ (length list-of-propbank-sentences) num-threads))
         (threads (list))
         (mutex (bt:make-lock)))

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

    ;; Save and return predictions
    (cl-store:store predictions output-file)
    predictions)))

(defun save-prediction-f1-scores-to-csv (predictions-f1-scores filename)
  "Save a list of prediction-f1 scores to a CSV file"
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "train_set_size,test_corpus,f1_score~%")
    (dolist (prediction predictions-f1-scores)
      (dolist (score prediction)
        (let* ((set-size (getf score :SET-SIZE))
               (test-corpus (getf score :TEST-CORPUS))
               (f1-score (getf score :F1-SCORE)))
          (format stream "~a,~a,~a~%" set-size test-corpus f1-score))))))

(defun store-run-info-size (total-runtime step-size max-train-set length-test-corpus predictions-f1-scores-dev predictions-f1-scores-train)
  "Store information about the run, including total runtime, step size, max train set size, test corpus size, and f1 scores for development and training set. 
  Takes as input the total runtime, step size, max train set size, length of test corpus, and lists of f1 scores for development and training sets.
  Writes the information to a text file."
  (let ((test-info (list :total-runtime total-runtime
                         :step-size step-size
                         :max-train-set max-train-set
                         :test-corpus-size length-test-corpus
                         :dev-f1-scores predictions-f1-scores-dev
                         :train-f1-scores predictions-f1-scores-train)))
    (let ((file-name (format nil "info-test-~A-step-~A-max-~A-test" step-size max-train-set length-test-corpus)))
      (with-open-file (stream (babel-pathname :directory '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "predictions-config-eval")
                                          :name file-name
                                          :type "txt")
                       :direction :output
                       :if-exists :supersede)
        (format stream "Total runtime: ~f seconds~%" total-runtime)
        (format stream "Step-size: ~A~%" step-size)
        (format stream "Max-train-set-size: ~A~%" max-train-set)
        (format stream "Dev-set-size: ~A~%" length-test-corpus)
        (format stream "Dev Evolution F1-scores: ~A~%" predictions-f1-scores-dev)
        (format stream "Train Evolution F1-scores: ~A~%" predictions-f1-scores-train)))))

(defun store-learned-grammar-train-size (set-size &optional (grammar test-grammar))
  "Stores the learned grammar using the configuration specified in 'updated-training-config' for the given 'parameter'
   in a file with a name specified in 'config-file-name' and path"
  (let* ((filename (format nil "~a~a" "gr" set-size))
         (pathname (babel-pathname :directory '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "grammars-config-eval")
                                   :name filename
                                   ))
         (pathname-string (namestring pathname))
         (pathname-length (length pathname-string))
         (pathname-cutoff (min pathname-length 251))
         (pathname (subseq pathname-string 0 pathname-cutoff))
         (pathname (format nil (format nil "~a~a" pathname ".fcg"))))
    (cl-store:store grammar pathname)))




