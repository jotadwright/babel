(in-package :propbank-grammar)

(defun simulated-annealing-plots (initial-params list-of-combinations &key (temperature 1000) (cooling-rate 0.95) (steps 16) (train-set *train-corpus*) (dev-set *dev-corpus*))
  "Uses simulated annealing to optimize the parameters for a given evaluation function."
  (let* ((current-params initial-params)
         (current-score (learn-predict-evaluate-sa current-params))
         (best-params current-params)
         (init-temp temperature)
         (best-score current-score)
         (used-params '())
         (f1-scores (list (cons current-params current-score)))
         (start-time (get-internal-real-time)))
    (dotimes (step steps)
      (let* ((new-params (random-neighbour current-params list-of-combinations used-params))
             (new-score (learn-predict-evaluate-sa new-params)))
        (when (> new-score best-score)
          (setf best-params new-params
                best-score new-score))
        (when (> new-score current-score)
          (setf current-params new-params
                current-score new-score
                used-params '()))
        (when (or (> new-score current-score)
                  (< (random 1.0) (exp (/ (- current-score new-score) (max 1.0 temperature)))))
          (setf current-params new-params
                current-score new-score
                used-params (cons current-params used-params)))
        (setf temperature (* temperature cooling-rate))
        (push (cons current-params current-score) f1-scores)
        (store-f1-params (reverse f1-scores) init-temp cooling-rate steps (length train-set) (length dev-set)))
      (format t "Step ~a: temperature = ~a, best score = ~a, best parameters = ~a~% " step temperature best-score best-params))
    (format t "F1-scores evolution = ~a~% " (reverse f1-scores))
    (store-f1-params (reverse f1-scores) init-temp cooling-rate steps (length train-set) (length dev-set))
    (plot-f1-scores (reverse f1-scores))
    (let ((total-runtime (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
      (store-run-info total-runtime init-temp temperature cooling-rate steps (length train-set) (length dev-set) (reverse f1-scores) list-of-combinations)
      (format t "Total runtime: ~f seconds~%" total-runtime))
    best-params))

(defun plot-f1-scores (f1-scores-and-params)
  "Plots the f1 scores using Gnuplot."
  (utils:with-open-pipe (stream (monitors:pipe-to-gnuplot))
    (format stream "set yrange [0:1]~%")
    (format stream "set key off~%")
    (format stream "plot '-' with linespoints title 'F1 Score'~%")
    (dotimes (i (length f1-scores-and-params))
      (let ((f1-score (cdr (nth i f1-scores-and-params)))
            (params (car (nth i f1-scores-and-params))))
        (format stream "~f ~f ~a~%" i f1-score params)))
    (format stream "e~%")
    (finish-output stream)))

(defun store-run-info (total-runtime initial-temperature final-temperature cooling-rate steps train-set-size dev-set-size evolution-f1 list-of-comb)
  (let ((test-info (list :total-runtime total-runtime
                         :average-time (/ total-runtime steps)
                         :initial-temperature initial-temperature
                         :final-temperature final-temperature
                         :cooling-rate cooling-rate
                         :steps steps
                         :train-set-size train-set-size
                         :dev-set-size dev-set-size
                         :evolution-f1 evolution-f1
                         :list-of-comb list-of-comb)))
    (let ((file-name (format nil "info-test-params-temp~A-cool~A-steps~A-train~A-dev~A" initial-temperature cooling-rate steps train-set-size dev-set-size)))
      (with-open-file (stream (babel-pathname :directory '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation" "predictions-parameter")
                                          :name file-name
                                          :type "txt")
                       :direction :output
                       :if-exists :supersede)
        (format stream "Total runtime: ~f seconds~%" total-runtime)
        (format stream "Average time to learn and predict one grammar: ~f seconds~%" (/ total-runtime steps))
        (format stream "Initial temperature: ~A~%" initial-temperature)
        (format stream "Final temperature: ~A~%" final-temperature)
        (format stream "Cooling rate: ~A~%" cooling-rate)
        (format stream "Steps: ~A~%" steps)
        (format stream "Train-set-size: ~A~%" train-set-size)
        (format stream "Dev-set-size: ~A~%" dev-set-size)
        (format stream "Evolution F1-scores: ~A~%" evolution-f1)
        (format stream "List of parameter combinations: ~A~%" list-of-comb)))))


(defun store-f1-params (f1-scores-params temperature cooling-rate steps train-set-size test-set-size)
  (let ((file-name (format nil "f1-scores-params-temp~A-cool~A-steps~A-train~A-dev~A" temperature cooling-rate steps train-set-size test-set-size)))
        (cl-store:store f1-scores-params
                    (babel-pathname :directory '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation" "predictions-parameter")
                                    :name file-name
                                    :type "store"))
    (with-open-file (stream (babel-pathname :directory '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation" "predictions-parameter")
                                          :name file-name
                                          :type "csv")
                         :direction :output
                         :if-exists :supersede)
      (loop for (params . score) in f1-scores-params
            do (format stream "~A,~A~%" (format nil "~{~A~^,~}" params) score)))))


(defun simulated-annealing (initial-params list-of-combinations &optional (temperature 1000) (cooling-rate 0.95) (steps 20))
  "Uses simulated annealing to optimize the parameters for a given evaluation function."
  (let* ((current-params initial-params)
        (current-score (learn-predict-evaluate-sa current-params))
        (best-params current-params)
        (best-score current-score)
        (used-params '()))
    (dotimes (step steps)
      (let* ((new-params (random-neighbour current-params list-of-combinations used-params))
            (new-score (learn-predict-evaluate-sa new-params)))
        (when (> new-score best-score)
          (setf best-params new-params
                best-score new-score))
        (when (> new-score current-score)
          (setf current-params new-params
                current-score new-score
                used-params '()))
        (when (or (> new-score current-score)
                  (< (random 1.0) (exp (/ (- current-score new-score) (max 1.0 temperature)))))
          (setf current-params new-params
                current-score new-score
                used-params (cons current-params used-params)))
        (setf temperature (* temperature cooling-rate)))
      (format t "Step ~a: temperature = ~a, best score = ~a, best parameters = ~a~% " step temperature best-score best-params))
    best-params))

(defun learn-predict-evaluate-sa (current-combination &key (test-batch-size 500) (old-training-configuration training-configuration-all) (new-training-configuration training-configuration-new) (train-corpus *train-corpus*))
  "Evaluates a PropBank grammar using specified parameters, training configurations and corpus."
  (let* ((random-number (format nil "~4,'0d" (random 10000)))
         (new-training-configuration (copy-list old-training-configuration))
         (test-grammar nil))
    (let ((updated-training-config (adjust-training-configuration current-combination new-training-configuration)))
      (format t "Running configuration: ~A~%" updated-training-config)
      (learn-propbank-grammar-roles
             *train-corpus*
             :cxn-inventory 'test-grammar
             :fcg-configuration updated-training-config)
      (store-learned-grammar current-combination :random-number random-number)
      (let* ((test-sentences *dev-corpus*)
             (f1-scores nil)
             (f1-scores-loose nil))
        (loop for i below (ceiling (length test-sentences) test-batch-size)
              for batch = (subseq test-sentences (* i test-batch-size) (* (1+ i) test-batch-size))
              do (let ((predictions-comprehend (comprehend-propbank-corpus-parameters test-grammar current-combination batch :random-number random-number)))
                   (push (evaluate-predictions-f1 predictions-comprehend) f1-scores)
                   (push (evaluate-predictions-f1 predictions-comprehend :include-word-sense nil :include-timed-out-sentences nil) f1-scores-loose)))
        (let ((final-f1 (/ (reduce #'+ f1-scores) (length f1-scores))))
          (format t "Final mean F1 score for current combination: ~a ~A~%" current-combination final-f1)
          (store-f1-scores-batch current-combination f1-scores test-batch-size)
          (store-f1-scores-batch-loose current-combination f1-scores-loose test-batch-size)
          final-f1)))))

(defun random-neighbour (current-parameter list-of-combinations-parameters &optional used-parameters)
  "Generates a random near neighbour of the given parameters."
  (let ((used-parameters (or used-parameters '()))
        (random-index (random (length list-of-combinations-parameters)))
        (neighbour nil))
    (loop for i below (length list-of-combinations-parameters) do
          (setf neighbour (nth (mod (+ i random-index) (length list-of-combinations-parameters)) list-of-combinations-parameters))
          (unless (member neighbour used-parameters)
            (return-from random-neighbour neighbour)))
    (unless neighbour
      (error "All remaining parameter combinations have been used"))
    neighbour))
