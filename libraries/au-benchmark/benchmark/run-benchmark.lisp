(in-package :au-benchmark.benchmark)

;;;;;;;;;;;;
;; Timing ;;
;;;;;;;;;;;;

(defparameter *internal-time-units-per-millisecond*
  (/ internal-time-units-per-second 1000))
  
(defun timings (function)
  "Run <function> while measuring the time it takes.
   Return the function's multiple return values, as
   well as the real time and the run time."
  (let ((real-base (get-internal-real-time))
        (run-base (get-internal-run-time))
        (function-return-values (multiple-value-list (funcall function))))
    (values function-return-values
            (/ (- (get-internal-real-time) real-base) *internal-time-units-per-millisecond*)
            (/ (- (get-internal-run-time) run-base) *internal-time-units-per-millisecond*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make Output File Name ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-output-file-path (input-file
                              output-dir
                              list-of-algorithms
                              k k-supplied-p
                              W W-supplied-p
                              V V-supplied-p
                              omega-scope scope-supplied-p
                              timeout timeout-supplied-p
                              shuffle shuffle-supplied-p
                              x x-supplied-p
                              y y-supplied-p
                              z z-supplied-p)
  (let ((base-name (format nil "results-~a-~a" (pathname-name input-file)
                           (list-of-strings->string (mapcar #'mkstr list-of-algorithms) :separator "-vs-"))))
    (when k-supplied-p
      (setf base-name (mkstr base-name (format nil "-k-~a" k))))
    (when W-supplied-p
      (setf base-name (mkstr base-name (format nil "-W-~a" W))))
    (when V-supplied-p
      (setf base-name (mkstr base-name (format nil "-V-~a" V))))
    (when scope-supplied-p
      (setf base-name (mkstr base-name (format nil "-scope-~a" omega-scope))))
    (when timeout-supplied-p
      (setf base-name (mkstr base-name (format nil "-timeout-~a" timeout))))
    (when shuffle-supplied-p
      (setf base-name (mkstr base-name (format nil "-shuffle-~a" shuffle))))
    (when x-supplied-p
      (setf base-name (mkstr base-name (format nil "-x-~a" x))))
    (when y-supplied-p
      (setf base-name (mkstr base-name (format nil "-y-~a" y))))
    (when z-supplied-p
      (setf base-name (mkstr base-name (format nil "-z-~a" z))))
    (merge-pathnames
     (make-pathname :name base-name :type "csv")
     output-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process Single Benchmark Line ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-anti-unification-benchmark-line (line &key list-of-algorithms k W V omega-scope timeout class shuffle x y z)
  (let ((data (read-from-string line)))
    (destructuring-bind (G1 G2 G1-len G2-len G1-num-vars G2-num-vars var-coeff atom-coeff) data
      (let ((out-line (list G1-len G2-len G1-num-vars G2-num-vars var-coeff atom-coeff
                            class k W V (downcase (mkstr omega-scope)) timeout shuffle x y z))
            (real-G1 (if shuffle (shuffle G1) G1))
            (real-G2 (if shuffle (shuffle G2) G2)))
        (dolist (algorithm list-of-algorithms)
          (multiple-value-bind (return-values real-time run-time)
              (case algorithm
                (:exhaustive_msg
                 (handler-case (trivial-timeout:with-timeout (timeout)
                                 (timings
                                  (lambda ()
                                    (au-benchmark.msg.exhaustive:anti-unify-predicate-networks real-G1 real-G2))))
                   (trivial-timeout:timeout-error (error) (values nil nil nil))))
                (:exhaustive_lcg
                 (handler-case (trivial-timeout:with-timeout (timeout)
                                 (timings
                                  (lambda ()
                                    (au-benchmark.lcg.exhaustive:anti-unify-predicate-networks real-G1 real-G2))))
                   (trivial-timeout:timeout-error (error) (values nil nil nil))))
                (:kswap_msg_heuristic
                 (handler-case (trivial-timeout:with-timeout (timeout)
                                 (timings
                                  (lambda ()
                                    (au-benchmark.msg.kswap-heuristic:anti-unify-predicate-networks
                                     real-G1 real-G2 :k k :W W :V V :omega-scope omega-scope))))
                   (trivial-timeout:timeout-error (error) (values nil nil nil))))
                (:1swap_msg_omega
                 (handler-case (trivial-timeout:with-timeout (timeout)
                                 (timings
                                  (lambda ()
                                    (au-benchmark.msg.1swap-omega:anti-unify-predicate-networks
                                     real-G1 real-G2))))
                   (trivial-timeout:timeout-error (error) (values nil nil nil))))
                (:kswap_msg_omega
                 (handler-case (trivial-timeout:with-timeout (timeout)
                                 (timings
                                  (lambda ()
                                    (au-benchmark.msg.kswap-omega:anti-unify-predicate-networks
                                     real-G1 real-G2 :k k :W W :x x :y y :z z))))
                   (trivial-timeout:timeout-error (error) (values nil nil nil))))
                (:kswap_lcg
                 (handler-case (trivial-timeout:with-timeout (timeout)
                                 (timings
                                  (lambda ()
                                    (au-benchmark.lcg.kswap:anti-unify-predicate-networks
                                     real-G1 real-G2 :k k :W W :V V :omega-scope omega-scope))))
                   (trivial-timeout:timeout-error (error) (values nil nil nil))))
                (:baseline_msg
                 (handler-case (trivial-timeout:with-timeout (timeout)
                                 (timings
                                  (lambda ()
                                    (au-benchmark.msg.baseline:anti-unify-predicate-networks real-G1 real-G2))))
                   (trivial-timeout:timeout-error (error) (values nil nil nil))))
                (:baseline_lcg
                 (handler-case (trivial-timeout:with-timeout (timeout)
                                 (timings
                                  (lambda ()
                                    (au-benchmark.lcg.baseline:anti-unify-predicate-networks real-G1 real-G2))))
                 (trivial-timeout:timeout-error (error) (values nil nil nil)))))
            (if (and return-values real-time run-time)
              (destructuring-bind (solutions cost number-of-alignments) return-values
                (push (length (generalisation (first solutions))) out-line)  ; size of the generalisation (phi)
                (push number-of-alignments out-line)  ; size of gen or number of alignments
                (push (float run-time) out-line)
                (push (float real-time) out-line)
                (push cost out-line))
              (setf out-line (append (list -1 -1 -1 -1 -1) out-line)))))
        (format nil "~{~a~^,~}" out-line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Benchmark Sequentially ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-anti-unification-benchmark (input-file output-dir list-of-algorithms
                                                  &key (k nil k-supplied-p)
                                                  (W nil W-supplied-p)
                                                  (V nil V-supplied-p)
                                                  (omega-scope :global scope-supplied-p)
                                                  (timeout 60 timeout-supplied-p)
                                                  (shuffle nil shuffle-supplied-p)
                                                  (x nil x-supplied-p)
                                                  (y nil y-supplied-p)
                                                  (z nil z-supplied-p))
  (let ((output-file (make-output-file-path input-file
                                            output-dir
                                            list-of-algorithms
                                            k k-supplied-p
                                            W W-supplied-p
                                            V V-supplied-p
                                            omega-scope scope-supplied-p
                                            timeout timeout-supplied-p
                                            shuffle shuffle-supplied-p
                                            x x-supplied-p
                                            y y-supplied-p
                                            z z-supplied-p))
        (class (loop for substr in (split-sequence:split-sequence #\- (pathname-name input-file))
                     when (parse-integer substr :junk-allowed t)
                       return (parse-integer substr))))
    (ensure-directories-exist output-file)
    (with-open-file (in-stream input-file :direction :input)
      (with-open-file (out-stream output-file :direction :output :if-exists :supersede)
        (loop for input-line = (read-line in-stream nil nil)
              for output-line = (when input-line
                                  (process-anti-unification-benchmark-line input-line
                                                                           :list-of-algorithms list-of-algorithms
                                                                           :k k :W W :V V :omega-scope omega-scope
                                                                           :timeout timeout :class class
                                                                           :shuffle shuffle :x x :y y :z z))
              while input-line
              do (write-line output-line out-stream)
                 (force-output out-stream))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Benchmark in Parallel ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-anti-unification-benchmark-in-parallel (input-file
                                                   output-dir
                                                   list-of-algorithms
                                                   num-processes
                                                   &key (k nil k-supplied-p)
                                                   (W nil w-supplied-p)
                                                   (V nil v-supplied-p)
                                                   (omega-scope :global scope-supplied-p)
                                                   (timeout 60 timeout-supplied-p)
                                                   (shuffle nil shuffle-supplied-p)
                                                   (x nil x-supplied-p)
                                                   (y nil y-supplied-p)
                                                   (z nil z-supplied-p)
                                                   temp-dir)
  (let ((output-file (make-output-file-path input-file
                                            output-dir
                                            list-of-algorithms
                                            k k-supplied-p
                                            W W-supplied-p
                                            V V-supplied-p
                                            omega-scope scope-supplied-p
                                            timeout timeout-supplied-p
                                            shuffle shuffle-supplied-p
                                            x x-supplied-p
                                            y y-supplied-p
                                            z z-supplied-p))
        (class (loop for substr in (split-sequence:split-sequence #\- (pathname-name input-file))
                     when (parse-integer substr :junk-allowed t)
                       return (parse-integer substr))))
    (ensure-directories-exist output-file)
    (when temp-dir
      (ensure-directories-exist temp-dir))
    (cl-pcp:process-corpus-in-parallel
     input-file output-file
     :asdf-systems '(:au-benchmark)
     :process-fn 'process-anti-unification-benchmark-line
     :process-fn-kwargs `(:list-of-algorithms ,list-of-algorithms
                          :k ,k :W ,W :V ,V :omega-scope ,omega-scope
                          :timeout ,timeout :class ,class :shuffle ,shuffle
                          :x ,x :y ,y :z ,z)
     :nr-of-processes num-processes
     :temp-directory temp-dir
     :keep-order t)))



