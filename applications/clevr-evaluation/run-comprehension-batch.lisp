
(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

(defun succeededp (cipn)
  "Check if the goal test succeeded"
  (and cipn (eql (first (statuses cipn)) 'fcg::succeeded)))


(defun get-depth-of-solution (cipn)
  (length (all-parents cipn)))


(defun comprehend-until-solution (utterance id)
  "Try to comprehend the utterance until a
   solution is found"
  (declare (ignorable id))
  (loop with cipn = nil
        with meaning = nil
        until (succeededp cipn)
        for attempt from 1
        for (irl-program node)
        = (multiple-value-list
           (comprehend utterance :cxn-inventory *CLEVR* :silent t))
        when (succeededp node)
        do (setf cipn node
                 meaning irl-program)
        finally (return (values meaning cipn))))


(defun get-meaning-and-comprehension-cxns (id utterance)
  "Run comprehension until a solution is found.
   Export the meaning network in IRL and RPN format
   and the applied constructions."
  (multiple-value-bind (irl-program cipn)
      (comprehend-until-solution utterance id)
    (declare (ignorable cipn))
    (values (downcase (mkstr irl-program))
            (clevr-meaning->rpn irl-program)
            (list-of-strings->string
             (reverse
              (mapcar (compose #'downcase #'mkstr #'name)
                      (applied-constructions cipn))))
            (get-depth-of-solution cipn))))


(defun process-inputfile (inputfile outputdir)
  "Process the inputfile"
  ;; open read/write pipes and create the header
  ;; of the outputfile based on the header of
  ;; the inputfile
  (let* ((lines-to-process (- (number-of-lines inputfile) 1))
         (in-stream (open inputfile :direction :input))
         (outputfile (make-pathname :directory (pathname-directory outputdir)
                                    :name (pathname-name inputfile)
                                    :type (pathname-type inputfile)))
         (out-stream-header
          (list "id" "utterance" "irl_program"
                "rpn" "comprehension_cxns"
                "depth_of_solution"))
         (outputfile-exists-p (probe-file outputfile))
         out-stream)
    ;; when the outputfile already exists, check how many
    ;; lines have already been processed and skip these
    ;; in the inputfile (they no longer need processing)
    (if outputfile-exists-p
      (let ((lines-already-processed (- (number-of-lines outputfile) 1)))
        (decf lines-to-process lines-already-processed)
        (loop repeat lines-already-processed
              do (read-line in-stream nil nil))
        (setf out-stream
              (open outputfile :direction :output
                    :if-exists :append)))
      ;; if the outputfile does not exist, create it and
      ;; write the header to it
      (progn (ensure-directories-exist outputfile)
        (setf out-stream
            (open outputfile :direction :output
                  :if-does-not-exist :create))
        (write-csv-row out-stream-header :stream out-stream)
        (force-output out-stream)))
    ;; loop over the lines, comprehend them and
    ;; write to output
    (with-progress-bar (bar lines-to-process
                            ("Processing ~a" (pathname-name inputfile)))
      (do-csv (row in-stream :skip-first-p (not outputfile-exists-p))
        (destructuring-bind (id utterance) row
          (multiple-value-bind (irl-program rpn
                                comprehension-cxns
                                depth-of-solution)
              (get-meaning-and-comprehension-cxns id utterance)
            (let ((out-row
                   (list id utterance irl-program rpn
                         comprehension-cxns
                         depth-of-solution)))
              (write-csv-row out-row :stream out-stream)
              (force-output out-stream)
              (update bar))))))
    ;; close the pipes
    (close in-stream)
    (force-output out-stream)
    (close out-stream)))


#|
 (activate-monitor trace-fcg)
 
(set-configurations *CLEVR*
                    '((:cxn-supplier-mode . :ordered-by-label-hashed)
                      (:priority-mode . :nr-of-applied-cxns)
                      (:parse-order hashed cxn)
                      (:production-order hashed cxn hashed)
                      (:max-nr-of-nodes . 5000)
                      (:queue-mode . :greedy-best-first))
                    :replace t)

(process-inputfile
 (babel-pathname :directory '("applications" "clevr-evaluation")
                 :name "batch-0" :type "csv")
 (babel-pathname :directory '(".tmp")))
|#


(defun parse-args (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))


(defun main (command-line-args)
  (let ((args (parse-args command-line-args)))
    ;; check the command line args
    (loop for arg in '(inputfile outputdir max-nr-of-nodes strategy)
          unless (getf args arg)
          do (error "Missing command line argument: ~a" arg))
    ;; print the command line args
    (format t "~%Received args: ~a" args)
    ;; set the strategy
    (let* ((strategy (make-kw (getf args 'strategy)))
           (max-nr-of-nodes (parse-integer (getf args 'max-nr-of-nodes)))
           (clevr-configurations
            (case strategy
              (:depth-first
               `((:queue-mode . :greedy-best-first)
                 (:cxn-supplier-mode . :ordered-by-label-hashed)
                 (:hash-mode . :hash-string-meaning-lex-id)
                 (:priority-mode . :nr-of-applied-cxns)
                 (:max-nr-of-nodes . ,max-nr-of-nodes)
                 (:parse-order hashed cxn)
                 (:production-order hashed cxn hashed)))
              (:priming
               `((:queue-mode . :greedy-best-first)
                 (:cxn-supplier-mode . :ordered-by-label-hashed)
                 (:hash-mode . :hash-string-meaning-lex-id)
                 (:priority-mode . :priming)
                 (:max-nr-of-nodes . ,max-nr-of-nodes)
                 (:parse-order hashed cxn)
                 (:production-order hashed cxn hashed))))))
      ;; set the configurations for the CLEVR grammar
      (set-configurations *CLEVR* clevr-configurations :replace t)
      ;; import priming data when found
      (when (and (eql strategy :priming)
                 (getf args 'import-priming-data-path))
        (fcg-import-comprehension-priming-data
         (parse-namestring (getf args 'import-priming-data-path))
         *CLEVR*)))
    ;; process the inputfile
    (process-inputfile (getf args 'inputfile)
                       (getf args 'outputdir))))


#-lispworks
(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))
