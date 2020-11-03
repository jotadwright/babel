
(ql:quickload :fcg-search-evaluation)
(in-package :fcg-search-evaluation)

;;;; Utils
;;;; ----------------------------------------

(defun make-csv-line (&rest args)
  "Create a comma separated string of args"
  (format nil "~{~a~^,~}" args))

(defun directoryp (pathspec)
  "Check if the given pathspec is a directory"
  (and (null (pathname-name pathspec))
       (null (pathname-type pathspec))))

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
  ;; input_length, output_length, success
  ;; avg_branching_factor, run_time, no_search
  (let ((line
         (make-csv-line id
                        ;; length of the input
                        (get-input-length input)
                        ;; length of the output, if success
                        (if (and cipn (succeededp cipn))
                          (get-output-length output)
                          "None")
                        ;; success
                        (if (and cipn (succeededp cipn))
                          "True" "False")
                        ;; avg branching factor
                        (if (and cipn (succeededp cipn))
                          (get-avg-branching-factor cipn)
                          "None")
                        ;; processing time
                        (if (and cipn (succeededp cipn))
                          run-time "None")
                        ;; no search
                        (if (and cipn (succeededp cipn))
                          (if (no-search-p cipn)
                            "True" "False")
                          "None"))))
    (write-line line out-stream)
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

(defun comprehend-line (grammar id utterance out-stream timeout)
  (multiple-value-bind (meaning cipn run-time)
      (comprehend-with-timings grammar utterance timeout)
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
  (let* ((outputfile
          (make-pathname :directory (pathname-directory outputdir)
                         :name (pathname-name inputfile)
                         :type (pathname-type inputfile)))
         (out-stream
          (open outputfile :direction :output
                :if-exists :supersede
                :if-does-not-exist :create))
         (header
          (make-csv-line "id" "input_length" "output_length"
                         "success" "avg_branching_factor"
                         "run_time" "no_search")))
    (ensure-directories-exist outputfile)
    (write-line header out-stream)
    (force-output out-stream)
    out-stream))

(defun process-inputfile (grammar inputfile outputdir timeout direction)
  (let ((lines-to-process (- (number-of-lines inputfile) 1))
        (in-stream (open inputfile :direction :input))
        (out-stream (make-out-stream inputfile outputdir)))
    ;; skip the header
    (read-line in-stream nil nil)
    ;; loop over the lines
    ;; process them
    ;; and write monitors
    (with-progress-bar (bar lines-to-process ("Processing ~a" (pathname-name inputfile)))
      (loop for line = (remove #\Return (read-line in-stream nil nil))
            while line
            do (let* ((fields (split line #\,))
                      (id (first fields))
                      (utterance (second fields))
                      (irl-program (read-from-string (third fields))))
                 (case direction
                   (:comprehension (comprehend-line grammar id utterance out-stream timeout))
                   (:formulation (formulate-line grammar id irl-program out-stream timeout))))
            do (update bar)))
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
              (:cxn-supplier-mode . :hashed-simple-queue)
              (:priority-mode . :nr-of-applied-cxns)
              (:max-nr-of-nodes . ,max-nr-of-nodes)))
           (:priming
            `((:queue-mode . :greedy-best-first)
              (:cxn-supplier-mode . :hashed-simple-queue)
              (:priority-mode . :priming)
              (:max-nr-of-nodes . ,max-nr-of-nodes)))
           (:seq2seq
            (let ((endpoint #+ccl (format nil "http://127.0.0.1:~a/next-cxn"
                                          seq2seq-server-port)
                            #-ccl (format nil "http://localhost:~a/next-cxn"
                                          seq2seq-server-port)))
              `((:queue-mode . :greedy-best-first)
                (:cxn-supplier-mode . :hashed+seq2seq-heuristic)
                (:priority-mode . :seq2seq-heuristic-additive)
                (:seq2seq-endpoint . ,endpoint)
                (:max-nr-of-nodes . ,max-nr-of-nodes)))))))
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

(activate-strategy :depth-first 20000 nil)

(process-inputfile
 (babel-pathname :directory '("applications" "fcg-search-evaluation")
                 :name "batch-0-comprehension" :type "csv")
 (babel-pathname :directory '(".tmp"))
 60 :formulation)

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
  (unless (member (getf args 'strategy) '(:depth-first :priming :seq2seq))
    (error "Unknown strategy: ~a. Expected :depth-first, :priming or :seq2seq"
           (getf args 'strategy)))
  (when (eql (getf args 'strategy) :seq2seq)
    (if (getf args 'seq2seq-server-port)
      (setf (getf args 'seq2seq-server-port)
            (parse-integer (getf args 'seq2seq-server-port)))
      (error "Must specify a seq2seq-server-port when using the seq2seq strategy!")))      
  (setf (getf args 'direction)
        (make-kw (upcase (getf args 'direction))))
  (setf (getf args 'timeout)
        (parse-integer (getf args 'timeout)))
  (setf (getf args 'max-nr-of-nodes)
        (parse-integer (getf args 'max-nr-of-nodes)))
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