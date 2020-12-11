
(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

(defun make-csv-line (&rest args)
  "Create a comma separated string of args"
  (format nil "~{~a~^,~}" args))

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


(defun get-meaning-and-comprehension-cxns (input-line)
  "Run comprehension until a solution is found.
   Export the meaning network in IRL and RPN format
   and the applied constructions."
  (let* ((fields (split input-line #\,))
         (id (parse-integer (first fields)))
         (utterance (second fields)))
    (multiple-value-bind (irl-program cipn)
        (comprehend-until-solution utterance id)
      (declare (ignorable cipn))
      (values (downcase (mkstr irl-program))
              (clevr-meaning->rpn irl-program)
              (list-of-strings->string
               (reverse
                (mapcar (compose #'downcase #'mkstr #'name)
                        (applied-constructions cipn))))
              (get-depth-of-solution cipn)))))


(defun process-inputfile (inputfile outputdir)
  "Process the inputfile"
  ;; open read/write pipes and create the header
  ;; of the outputfile based on the header of
  ;; the inputfile
  (let* ((lines-to-process (- (number-of-lines inputfile) 1))
         (in-stream (open inputfile :direction :input))
         (in-stream-header (remove #\Return (read-line in-stream)))
         (outputfile (make-pathname :directory (pathname-directory outputdir)
                                    :name (pathname-name inputfile)
                                    :type (pathname-type inputfile)))
         (outputfile-exists-p (probe-file outputfile))
         (out-stream (open outputfile :direction :output
                           :if-exists :append
                           :if-does-not-exist :create))
         (out-stream-header
          (make-csv-line in-stream-header "irl_program"
                         "rpn" "comprehension_cxns" "depth_of_solution")))
    ;; when the outputfile already exists, check how many
    ;; lines have already been processed and skip these
    ;; in the inputfile (they no longer need processing)
    (when outputfile-exists-p
      (let ((lines-already-processed (- (number-of-lines outputfile) 1)))
        (decf lines-to-process lines-already-processed)
        (loop repeat lines-already-processed
              do (read-line in-stream nil nil))))
    ;; if the outputfile does not exist, create it and
    ;; write the header to it
    (unless outputfile-exists-p
      (ensure-directories-exist outputfile)
      (write-line out-stream-header out-stream)
      (force-output out-stream))
    ;; loop over the lines, comprehend them and
    ;; write to output
    (with-progress-bar (bar lines-to-process ("Processing ~a" (pathname-name inputfile)))
      (loop for line = (remove #\Return (read-line in-stream nil nil))
            while line
            do (multiple-value-bind (irl-program rpn comprehension-cxns depth-of-solution)
                   (get-meaning-and-comprehension-cxns line)
                 (let ((out-line
                        (make-csv-line line irl-program rpn comprehension-cxns depth-of-solution)))
                   (write-line out-line out-stream)
                   (force-output out-stream)
                   (update bar)))))
    ;; close the pipes
    (close in-stream)
    (force-output out-stream)
    (close out-stream)))

#|
(set-configurations *CLEVR*
                    '((:max-nr-of-nodes . 50000)
                      (:cxn-supplier-mode . :all-cxns-except-incompatible-hashed-cxns)
                      (:priority-mode . :nr-of-applied-cxns))
                    :replace t)

(process-inputfile
 (babel-pathname :directory '("applications" "clevr-evaluation")
                 :name "batch-0" :type "csv")
 (babel-pathname :directory '(".tmp")))
|#

(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))


(defun main (args)
  (let ((arg-plist (args->plist args)))
    ;; check the command line args
    (loop for indicator in '(inputfile outputdir max-nr-of-nodes strategy)
          unless (getf arg-plist indicator)
          do (error "Missing command line argument: ~a" indicator))
    (let* ((strategy (make-kw (getf arg-plist 'strategy)))
           (max-nr-of-nodes (parse-integer (getf arg-plist 'max-nr-of-nodes)))
           (clevr-configurations
            (case strategy
              (:depth-first
               `((:queue-mode . :greedy-best-first)
                 (:cxn-supplier-mode . :all-cxns-except-incompatible-hashed-cxns)
                 (:hash-mode . :hash-string-meaning-lex-id)
                 (:priority-mode . :nr-of-applied-cxns)
                 (:max-nr-of-nodes . ,max-nr-of-nodes)))
              (:priming
               `((:queue-mode . :greedy-best-first)
                 (:cxn-supplier-mode . :all-cxns-except-incompatible-hashed-cxns)
                 (:hash-mode . :hash-string-meaning-lex-id)
                 (:priority-mode . :priming)
                 (:max-nr-of-nodes . ,max-nr-of-nodes))))))
      ;; set the configurations for the CLEVR grammar
      (set-configurations *CLEVR* clevr-configurations :replace t)
      ;; import priming data when found
      (when (and (eql strategy :priming)
                 (getf arg-plist 'import-priming-data-path))
        (fcg-import-comprehension-priming-data
         (parse-namestring (getf arg-plist 'import-priming-data-path))
         *CLEVR*)))
    ;; process the inputfile
    (process-inputfile (getf arg-plist 'inputfile)
                       (getf arg-plist 'outputdir))))

#-lispworks
(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))
