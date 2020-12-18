
(ql:quickload :fcg-search-evaluation)
(in-package :fcg-search-evaluation)

;;;; Utils
;;;; ----------------------------------------

(defun directoryp (pathspec)
  "Check if the given pathspec is a directory"
  (and (null (pathname-name pathspec))
       (null (pathname-type pathspec))))

;;;; Merge priming data
;;;; ----------------------------------------

(defun merge-priming-tables-aux (target source)
  (loop for key being the hash-keys of source using (hash-value value)
        if (gethash key target)
        do (setf (gethash key target)
                 (+ (gethash key target) value))
        else
        do (setf (gethash key target) value)))

(defun merge-priming-tables (target source)
  (loop for key being the hash-keys of source using (hash-value value)
        if (gethash key target)
        do (merge-priming-tables-aux (gethash key target) value)
        else
        do (setf (gethash key target) value)))

(defun merge-priming-data-tables (input-files output-file)
  (let ((merged-tables
         (loop with result = nil
               for file in input-files
               for table = (cl-store:restore file)
               if result
               do (merge-priming-tables result table)
               else
               do (setf result table)
               finally
               (return result))))
    (cl-store:store merged-tables output-file)
    t))

#|
(merge-priming-data-tables
 (directory
  (babel-pathname :directory '("applications" "fcg-search-evaluation" "results" "formulation" "priming-gather-data")
                  :name :wild :type "lsp"))
 (babel-pathname :directory '("applications" "fcg-search-evaluation" "results" "formulation")
                 :name "priming-data" :type "lsp"))
|#

;;;; Monitors
;;;; ----------------------------------------

(defun succeededp (cipn)
  "Check if the goal test succeeded"
  (and cipn (eql (first (statuses cipn)) 'fcg::succeeded)))

(defun get-input-length (input)
  (if (stringp input)
    (length (split input #\space))
    (length input)))

(defun get-output-length (input)
  (if (stringp input)
    (length (split input #\space))
    (length input)))

(defmethod cip-leafs ((cip construction-inventory-processor))
  "Get all leafs from the cipn"
  (labels ((leafs-rec (node acc)
             (if (null (children node))
               (cons node acc)
               (loop for child in (children node)
                     append (leafs-rec child acc)))))
    (leafs-rec (top-node cip) nil)))

(defun get-avg-branching-factor (cipn)
  (let* ((nr-of-nodes (node-counter (cip cipn)))
         (nr-of-non-root-nodes (- nr-of-nodes 1))
         (nr-of-leaf-nodes (length (cip-leafs (cip cipn))))
         (nr-of-non-leaf-nodes (- nr-of-nodes nr-of-leaf-nodes)))
    (float (/ nr-of-non-root-nodes nr-of-non-leaf-nodes))))

(defun get-nr-of-nodes (cipn)
  (node-counter (cip cipn)))

(defun get-depth-of-solution (cipn)
  (length (all-parents cipn)))

(defun get-search-space-size (cipn)
  (float
   (/ (get-nr-of-nodes cipn)
      (get-depth-of-solution cipn))))

(defun no-search-p (cipn)
  ;; when nodes that are NOT on the path to the solution
  ;; NEVER have any children, there was no search
  (let ((path-to-solution (all-parents cipn))
        (no-search-p t))
    (traverse-depth-first
     (cip cipn)
     :do-fn #'(lambda (node)
                (when (and (not (member node path-to-solution))
                           (children node))
                  (setf no-search-p nil))))
    no-search-p))

(defun run-monitors (id input output cipn run-time out-stream)
  (let ((input-length (get-input-length input)) ;; length of the input
         (output-length
          (if (and cipn (succeededp cipn))
            (get-output-length output)
            "None"))
         (success
          (if (and cipn (succeededp cipn))
            "True" "False"))
         (avg-branching-factor
          (if (and cipn (succeededp cipn))
            (get-avg-branching-factor cipn)
            "None"))
         (nr-of-nodes
          (if (and cipn (succeededp cipn))
            (get-nr-of-nodes cipn)
            "None"))
         (search-space-size
          (if (and cipn (succeededp cipn))
            (get-search-space-size cipn)
            "None"))
         (depth-of-solution
          (if (and cipn (succeededp cipn))
            (get-depth-of-solution cipn)
            "None"))
         (processing-time
          (if (and cipn (succeededp cipn))
            run-time "None")))
    (write-csv-row
     (list id input-length output-length
           success avg-branching-factor
           nr-of-nodes search-space-size
           depth-of-solution processing-time)
     :stream out-stream)
    (force-output out-stream)))

;;;; Comprehension
;;;; ----------------------------------------

(defun comprehend-with-timings (grammar utterance time-out)
  (let ((start-time (get-internal-real-time)))
    (multiple-value-bind (meaning cipn)
        (handler-case
            (trivial-timeout:with-timeout (time-out)
              (comprehend utterance :cxn-inventory grammar))
          (trivial-timeout:timeout-error (error)
            (values nil nil)))
      (let* ((end-time (get-internal-real-time))
             (run-time (float (/ (- end-time start-time)
                                 internal-time-units-per-second))))
        (if (null meaning)
          (values meaning cipn time-out)
          (values meaning cipn run-time))))))

(defun comprehend-until-solution (grammar utterance)
  (loop with cipn = nil
        with meaning = nil
        until (succeededp cipn)
        for start-time = (get-internal-real-time)
        for (irl-program node)
        = (multiple-value-list
           (comprehend utterance :cxn-inventory grammar :silent t))
        when (succeededp node)
        do (setf cipn node
                 meaning irl-program)
        finally (let* ((end-time (get-internal-real-time))
                       (run-time (float (/ (- end-time start-time)
                                           internal-time-units-per-second))))
                  (return (values meaning cipn run-time)))))

(defun comprehend-line (grammar id utterance out-stream timeout)
  (multiple-value-bind (meaning cipn run-time)
      (if (null timeout)
        (comprehend-until-solution grammar utterance)
        (comprehend-with-timings grammar utterance timeout))
    (run-monitors id utterance meaning cipn run-time out-stream)))

;;;; Formulation
;;;; ----------------------------------------

(defun formulate-with-timings (grammar meaning time-out)
  (let ((start-time (get-internal-real-time)))
    (multiple-value-bind (utterance cipn)
        (handler-case
            (trivial-timeout:with-timeout (time-out)
              (handler-case (formulate meaning :cxn-inventory grammar)
                (usocket:timeout-error (error)
                  (values nil nil))
                (error (e)
                  (values nil nil))))
          (trivial-timeout:timeout-error (error)
            (values nil nil)))
      (let* ((end-time (get-internal-real-time))
             (run-time (float (/ (- end-time start-time)
                                 internal-time-units-per-second))))
        (if (null utterance)
          (values utterance cipn time-out)
          (values utterance cipn run-time))))))

(defun formulate-line (grammar id irl-program out-stream timeout)
  (multiple-value-bind (utterance cipn run-time)
      (formulate-with-timings grammar irl-program timeout)
    (run-monitors id irl-program utterance cipn run-time out-stream)))

;;;; Main loop
;;;; ----------------------------------------

(defun make-out-stream (inputfile outputdir)
  (unless (directoryp outputdir)
    (setf outputdir (mkstr outputdir "/")))
  (ensure-directories-exist outputdir)
  (let* ((outputfile
          (make-pathname :directory (pathname-directory outputdir)
                         :name (pathname-name inputfile)
                         :type (pathname-type inputfile)))
         (out-stream
          (open outputfile :direction :output
                :if-exists :supersede
                :if-does-not-exist :create))
         (header
          (list "id" "input_length" "output_length"
                "success" "avg_branching_factor"
                "nr_of_nodes" "search_space_size"
                "depth_of_solution" "run_time"
                ;"no_search"
                )))
    (ensure-directories-exist outputfile)
    (write-csv-row header :stream out-stream)
    (force-output out-stream)
    out-stream))

(defun process-inputfile (grammar inputfile outputdir timeout direction)
  (let ((lines-to-process (- (number-of-lines inputfile) 1))
        (in-stream (open inputfile :direction :input))
        (out-stream (make-out-stream inputfile outputdir)))
    ;; loop over the lines
    ;; process them
    ;; and write monitors
    (with-progress-bar (bar lines-to-process
                            ("Processing ~a"
                             (pathname-name inputfile))
                            :width 50)
      (do-csv (row in-stream :skip-first-p t)
        (destructuring-bind (id utterance &optional irl-program &rest rest) row
          (declare (ignorable rest))
          (case direction
            (:comprehension (comprehend-line grammar id utterance out-stream timeout))
            (:formulation (formulate-line grammar id (read-from-string irl-program)
                                          out-stream timeout)))
          (update bar))))
    ;; close the pipes
    (close in-stream)
    (force-output out-stream)
    (close out-stream)
    t))

;;;; Configurations
;;;; ----------------------------------------

(defun activate-strategy (grammar strategy max-nr-of-nodes seq2seq-server-port)
  (let ((configurations
         (case strategy
           (:depth-first
            `((:queue-mode . :greedy-best-first)
              (:cxn-supplier-mode . :all-cxns-except-incompatible-hashed-cxns)
              (:hash-mode . :hash-string-meaning-lex-id)
              (:priority-mode . :nr-of-applied-cxns)
              ;(:max-nr-of-nodes . ,max-nr-of-nodes)
              ))
           (:priming
            `((:queue-mode . :greedy-best-first)
              (:cxn-supplier-mode . :all-cxns-except-incompatible-hashed-cxns)
              (:hash-mode . :hash-string-meaning-lex-id)
              (:priority-mode . :priming)
              ;(:max-nr-of-nodes . ,max-nr-of-nodes)
              ))
           (:seq2seq
            (let ((endpoint #+ccl (format nil "http://127.0.0.1:~a/next-cxn"
                                          seq2seq-server-port)
                            #-ccl (format nil "http://localhost:~a/next-cxn"
                                          seq2seq-server-port)))
              `((:queue-mode . :greedy-best-first)
                (:cxn-supplier-mode . :hashed+seq2seq-heuristic)
                (:hash-mode . :hash-string-meaning-lex-id)
                (:priority-mode . :seq2seq-heuristic-additive)
                (:seq2seq-endpoint . ,endpoint)
                ;(:max-nr-of-nodes . ,max-nr-of-nodes)
                )))
           (:beam2
            (let ((endpoint #+ccl (format nil "http://127.0.0.1:~a/next-cxn"
                                          seq2seq-server-port)
                            #-ccl (format nil "http://localhost:~a/next-cxn"
                                          seq2seq-server-port)))
              `((:queue-mode . :greedy-best-first)
                (:cxn-supplier-mode . :hashed+seq2seq-heuristic)
                (:hash-mode . :hash-string-meaning-lex-id)
                (:priority-mode . :seq2seq-heuristic-additive)
                (:seq2seq-endpoint . ,endpoint)
                (:seq2seq-probability-cutoff . 0.05)
                (:seq2seq-number-cutoff . 2)
                ;(:max-nr-of-nodes . ,max-nr-of-nodes)
                )))
           (:beam3
            (let ((endpoint #+ccl (format nil "http://127.0.0.1:~a/next-cxn"
                                          seq2seq-server-port)
                            #-ccl (format nil "http://localhost:~a/next-cxn"
                                          seq2seq-server-port)))
              `((:queue-mode . :greedy-best-first)
                (:cxn-supplier-mode . :hashed+seq2seq-heuristic)
                (:hash-mode . :hash-string-meaning-lex-id)
                (:priority-mode . :seq2seq-heuristic-additive)
                (:seq2seq-endpoint . ,endpoint)
                (:seq2seq-probability-cutoff . 0.05)
                (:seq2seq-number-cutoff . 3)
                ;(:max-nr-of-nodes . ,max-nr-of-nodes)
                )))
           (:beam7
            (let ((endpoint #+ccl (format nil "http://127.0.0.1:~a/next-cxn"
                                          seq2seq-server-port)
                            #-ccl (format nil "http://localhost:~a/next-cxn"
                                          seq2seq-server-port)))
              `((:queue-mode . :greedy-best-first)
                (:cxn-supplier-mode . :hashed+seq2seq-beam)
                (:hash-mode . :hash-string-meaning-lex-id)
                (:priority-mode . :seq2seq-heuristic-additive)
                (:seq2seq-endpoint . ,endpoint)
                (:beam-width . 7)
                ;(:max-nr-of-nodes . ,max-nr-of-nodes)
                )))
           )))
    (if (null max-nr-of-nodes)
      (setf configurations
            (append '((:node-tests :check-duplicate)
                      (:max-nr-of-nodes . 1000000))
                    configurations))
      (push `(:max-nr-of-nodes . ,max-nr-of-nodes) configurations))
    (set-configurations grammar configurations :replace t)
    (set-configurations (processing-cxn-inventory grammar)
                        configurations :replace t)))

;;;; Import/Export of priming data
;;;; ----------------------------------------

(defmethod import-priming-data (grammar path (direction (eql :comprehension)))
  (fcg-import-comprehension-priming-data path grammar))

(defmethod import-priming-data (grammar path (direction (eql :formulation)))
  (fcg-import-formulation-priming-data path grammar))

(defmethod export-priming-data (grammar path (direction (eql :comprehension)))
  (fcg-export-comprehension-priming-data grammar :path path)) 

(defmethod export-priming-data (grammar path (direction (eql :formulation)))
  (fcg-export-formulation-priming-data grammar :path path))

;;;; Testing
;;;; ----------------------------------------

#|

;; (defun activate-strategy (grammar strategy max-nr-of-nodes seq2seq-server-port)
(activate-strategy *clevr* :beam7 50000 8888)

;; (defun process-inputfile (grammar inputfile outputdir timeout direction)
(process-inputfile *clevr*
 (parse-namestring "/Users/jensnevens/Desktop/seq2seq/beam5_failed.csv")
 (babel-pathname :directory '(".tmp"))
 400 :comprehension)

|#


;;;; Command line interface
;;;; ----------------------------------------
(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))

(defun process-args (args)
  (setf (getf args 'grammar)
        (copy-object (eval (internal-symb (upcase (getf args 'grammar))))))
  (setf (getf args 'strategy)
        (make-kw (upcase (getf args 'strategy))))
  (when (getf args 'seq2seq-server-port)
    (setf (getf args 'seq2seq-server-port)
          (parse-integer (getf args 'seq2seq-server-port))))     
  (setf (getf args 'direction)
        (make-kw (upcase (getf args 'direction))))
  (setf (getf args 'timeout)
        (if (string= (getf args 'timeout) "nil")
          nil (parse-integer (getf args 'timeout))))
  (setf (getf args 'max-nr-of-nodes)
        (if (string= (getf args 'max-nr-of-nodes) "nil")
          nil (parse-integer (getf args 'max-nr-of-nodes))))
  (when (getf args 'import-priming-data-path)
    (setf (getf args 'import-priming-data-path)
          (parse-namestring (getf args 'import-priming-data-path))))
  (when (getf args 'export-priming-data-path)
    (setf (getf args 'export-priming-data-path)
          (parse-namestring (getf args 'export-priming-data-path))))
  args)
  

(defun main (args)
  (let ((arg-plist (args->plist args)))
    ;; check the command line args
    (loop for indicator in '(grammar inputfile outputdir
                             strategy timeout direction
                             max-nr-of-nodes)
          unless (getf arg-plist indicator)
          do (error "Missing command line argument: ~a" indicator))
    ;; process the command line args
    (setf arg-plist (process-args arg-plist))
    (print arg-plist)
    ;; set the configurations for the CLEVR grammar
    ;; depending on the strategy
    (activate-strategy (getf arg-plist 'grammar)
                       (getf arg-plist 'strategy)
                       (getf arg-plist 'max-nr-of-nodes)
                       (getf arg-plist 'seq2seq-server-port))
    ;; check if there is priming data to be imported
    (when (getf arg-plist 'import-priming-data-path)
      (import-priming-data (getf arg-plist 'grammar)
                           (getf arg-plist 'import-priming-data-path)
                           (getf arg-plist 'direction)))
    ;; process the inputfile
    (process-inputfile (getf arg-plist 'grammar)
                       (getf arg-plist 'inputfile)
                       (getf arg-plist 'outputdir)
                       (getf arg-plist 'timeout)
                       (getf arg-plist 'direction))
    ;; check if there is priming data to be exported
    (when (getf arg-plist 'export-priming-data-path)
      (export-priming-data (getf arg-plist 'grammar)
                           (getf arg-plist 'export-priming-data-path)
                           (getf arg-plist 'direction)))))

#|
How to call this script:

ccl -l script.lisp -b
    -- grammar *clevr*
       inputfile batch-0.csv
       outputdir raw-data/comprehension-depth-first/  ;; outputfile has same name as inputfile
       strategy depth-first
       timeout 60
       direction comprehension
       max-nr-of-nodes 50000
       [seq2seq-server-port 8000]  ;; only useful in seq2seq strategy
       [import-priming-data-path raw-data/comprehension-priming-data.lsp]  ;; only useful in priming strategy
       [export-priming-data-path raw-data/comprehension-priming-data.lsp]  ;; only useful in priming strategy
|#

#-lispworks
(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))