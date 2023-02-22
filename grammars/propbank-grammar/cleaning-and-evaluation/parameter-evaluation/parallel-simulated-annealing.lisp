(ql:quickload :bordeaux-threads)
(in-package :propbank-grammar)

(defun parallel-simulated-annealing-plots (initial-params list-of-combinations &key (num-threads 2) (temperature 1000) (cooling-rate 0.95) (steps 16) (train-set *train-corpus*) (dev-set *dev-corpus*))
  "Runs simulated annealing in parallel on multiple threads."
  (let* ((num-combinations (length list-of-combinations))
         (num-combinations-per-thread (floor (/ num-combinations num-threads)))
         (combinations-lists (split-list list-of-combinations num-threads))
         (best-score (learn-predict-evaluate-sa initial-params))
         (f1-scores (list (cons initial-params best-score)))
         (init-temp temperature)
         (best-params initial-params)
         (start-time (get-internal-real-time)))
    (format t "Running simulated annealing in parallel on ~a threads~%" num-threads)
    (format t "Each thread will handle ~a combinations~%" num-combinations-per-thread)
    (format t "Total number of combinations to explore: ~a~%" num-combinations)
    (format t "Starting annealing process...~%")
    (let ((threads (list)))
  (dotimes (i num-threads)
    (let* ((combinations (nth i combinations-lists))
           (thread-nmb (1+ i))
           (thread (bt:make-thread
                    (lambda ()
                      (dolist (score (simulated-annealing-for-par (random-neighbour best-params combinations initial-params) combinations :temperature temperature :cooling-rate cooling-rate :steps steps :train-set train-set :dev-set dev-set :thread-nmb thread-nmb))
                        (push score f1-scores))))))
      (push thread threads)))
  (mapc #'bt:join-thread threads))
    (let ((sorted-f1-scores (sort f1-scores #'> :key #'cdr)))
      (setf best-score (cdr (car sorted-f1-scores))
            best-params (car (car sorted-f1-scores)))
      (plot-f1-scores (reverse sorted-f1-scores))
      (store-f1-params-for-par sorted-f1-scores temperature cooling-rate steps (length train-set) (length dev-set))
      (let ((total-runtime (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
      (store-run-info-par total-runtime init-temp temperature cooling-rate steps (length train-set) (length dev-set) (reverse f1-scores) list-of-combinations num-threads)
      (format t "Total runtime all threads: ~f seconds~%" total-runtime)))
    (format t "All threads finished. Best score: ~a, Best parameters: ~a~%" best-score best-params)
    (list f1-scores)))

(defun get-best-score (f1-scores)
  "Returns the key-value pair with the best score from a list of key-value pairs."
  (reduce (lambda (a b) (if (> (cdr a) (cdr b)) a b)) f1-scores))

(defun split-list (list num-parts)
  "Splits a list into a number of sublists"
  (let* ((n (length list))
         (n-per-part (ceiling (/ n num-parts)))
         (parts (loop for i from 0 below num-parts
                      collect (subseq list (* i n-per-part) (min (* (1+ i) n-per-part) n)))))
    parts))

(defun simulated-annealing-for-par (initial-params list-of-combinations &key (temperature 1000) (cooling-rate 0.95) (steps 16) (train-set *train-corpus*) (dev-set *dev-corpus*) (thread-nmb 1))
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
        (store-f1-params-for-par (reverse f1-scores) init-temp cooling-rate steps (length train-set) (length dev-set) thread-nmb))
      (format t "Step ~a: temperature = ~a, best score = ~a, best parameters = ~a~% " step temperature best-score best-params thread-nmb))
    (format t "F1-scores evolution = ~a~% " (reverse f1-scores))
    (store-f1-params-for-par (reverse f1-scores) init-temp cooling-rate steps (length train-set) (length dev-set) thread-nmb)
    (let ((total-runtime (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
      (format t "Total runtime: ~f seconds~%" total-runtime))
    f1-scores))

(defun store-run-info-par (total-runtime initial-temperature final-temperature cooling-rate steps train-set-size dev-set-size evolution-f1 list-of-comb num-threads &optional (thread-nmb "all"))
  (let ((test-info (list :total-runtime total-runtime
                         :num-threads num-threads
                         :thread-nmb thread-nmb
                         :average-time (/ total-runtime steps)
                         :initial-temperature initial-temperature
                         :final-temperature final-temperature
                         :cooling-rate cooling-rate
                         :steps steps
                         :train-set-size train-set-size
                         :dev-set-size dev-set-size
                         :evolution-f1 evolution-f1
                         :list-of-comb list-of-comb)))
    (let ((file-name (format nil "info-test-params-thread-~A-temp~A-cool~A-steps~A-train~A-dev~A" thread-nmb initial-temperature cooling-rate steps train-set-size dev-set-size)))
      (with-open-file (stream (babel-pathname :directory '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation" "predictions-parameter")
                                          :name file-name
                                          :type "txt")
                       :direction :output
                       :if-exists :supersede)
        (format stream "Total runtime: ~f seconds~%" total-runtime)
        (format stream "Average time to learn and predict one grammar: ~f seconds~%" (/ total-runtime (* num-threads steps)))
        (format stream "Initial temperature: ~A~%" initial-temperature)
        (format stream "Final temperature: ~A~%" final-temperature)
        (format stream "Cooling rate: ~A~%" cooling-rate)
        (format stream "Steps: ~A~%" steps)
        (format stream "Train-set-size: ~A~%" train-set-size)
        (format stream "Dev-set-size: ~A~%" dev-set-size)
        (format stream "Evolution F1-scores: ~A~%" evolution-f1)
        (format stream "List of parameter combinations: ~A~%" list-of-comb)))))


(defun store-f1-params-for-par (f1-scores-params temperature cooling-rate steps train-set-size test-set-size &optional (thread-nmb "all"))
  (let ((file-name (format nil "thread-~A-f1-scores-params-temp~A-cool~A-steps~A-train~A-dev~A" thread-nmb temperature cooling-rate steps train-set-size test-set-size)))
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