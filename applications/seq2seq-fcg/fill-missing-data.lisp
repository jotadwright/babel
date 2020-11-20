(ql:quickload :seq2seq-fcg)
(in-package :seq2seq-fcg)

;;;; Helper functions
;;;; ----------------

(defun make-csv-line (&rest args)
  "Create a comma separated string of args"
  (format nil "~{~a~^,~}" args))

(defun succeededp (cipn)
  "Check if the goal test succeeded"
  (and cipn (eql (first (statuses cipn)) 'fcg::succeeded)))

;;;; Formulation
;;;; -----------

(defun formulate-with-timeout (irl-program rpn cxn-inventory timeout)
  (set-data (blackboard cxn-inventory) :rpn-input rpn)
  (multiple-value-bind (utterance cipn)
      (handler-case
          (with-timeout (timeout)
            (formulate irl-program
                       :cxn-inventory cxn-inventory
                       :silent t))
        (timeout-error (e)
          (values nil nil)))
    (remove-data (blackboard cxn-inventory) :rpn-input)
    (if (succeededp cipn)
      (values (list-of-strings->string utterance)
              (list-of-strings->string
               (reverse
                (mapcar (compose #'downcase #'mkstr #'name)
                        (applied-constructions cipn)))))
      (values nil nil))))

;;;; Main Functions
;;;; --------------

(defun set-seq2seq-configurations (cxn-inventory port)
  (let ((configs `((:cxn-supplier-mode . :hashed+seq2seq-heuristic)
                   (:priority-mode . :seq2seq-heuristic-additive)
                   (:seq2seq-endpoint . ,(format nil "http://localhost:~a/next-cxn" port))
                   (:seq2seq-model-comprehension . "clevr_comprehension_model")
                   (:seq2seq-model-formulation . "clevr_formulation_model"))))
    (set-configurations cxn-inventory configs :replace t)
    (set-configurations (processing-cxn-inventory cxn-inventory)
                        configs :replace t)))

(defun fill-missing-data (inputfile outputdir grammar timeout)
  ;; first, open a port to the inputfile and skip the header line
  ;; next, create the outputfile and write the header line
  ;; finally, process lines with no cxns and skip the other ones
  (let* ((nr-of-lines (number-of-lines inputfile))
         (in-stream (open inputfile :direction :input))
         (header (read-line in-stream nil nil))
         (outputfile (make-pathname :directory (pathname-directory outputdir)
                                    :name (pathname-name inputfile)
                                    :type (pathname-type inputfile)))
         (new-solutions-found 0)
         out-stream)
    (ensure-directories-exist outputfile)
    (setf out-stream (open outputfile :direction :output
                           :if-exists :supersede))
    (write-line header out-stream)
    (with-progress-bar (bar (- nr-of-lines 1) ("Processing ~a" (namestring inputfile)))
      (loop for line = (read-line in-stream nil nil)
            while line
            for fields = (split line #\,)
            if (string= (last-elt fields) "None")
            do (let ((id (first fields))
                     (irl-program (read-from-string (third fields)))
                     (rpn (fourth fields)))
                 (multiple-value-bind (utterance formulation-cxns)
                     (formulate-with-timeout irl-program rpn grammar timeout)
                   (if (and utterance formulation-cxns)
                     (let ((new-line (make-csv-line id utterance irl-program rpn formulation-cxns)))
                       (incf new-solutions-found)
                       (write-line new-line out-stream))
                     (write-line line out-stream))
                   (force-output out-stream)))
            else do (progn (write-line line out-stream)
                      (force-output out-stream))
            end
            do (update bar)))
    (format t "~%~%~a new solutions found" new-solutions-found)
    (close in-stream)
    (force-output out-stream)
    (close out-stream)))


;;;; Testing stuff
;;;; -------------

#|

 (activate-monitor trace-fcg)
 
 (set-seq2seq-configurations *CLEVR* 8888)
 
 (fill-missing-data
  (babel-pathname :directory '("applications" "seq2seq-fcg")
                  :name "batch-0" :type "csv")
  (babel-pathname :directory '("applications" "seq2seq-fcg" "out"))
  *clevr* 400)

|#
    

;;;; Command line interface
;;;; ----------------------

(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))

(defun main (args)
  (let ((args (args->plist args)))
    (loop for indicator in '(inputfile outputdir grammar
                             timeout port)
          unless (getf args indicator)
          do (error "Missing command line argument: ~a" indicator))
    (let* ((grammar (copy-object (eval (internal-symb (upcase (getf args 'grammar))))))
           (timeout (parse-integer (getf args 'timeout "60")))
           (port (parse-integer (getf args 'port "8888")))
           (inputfile (parse-namestring (getf args 'inputfile)))
           (outputdir (parse-namestring (getf args 'outputdir))))
      (set-seq2seq-configurations grammar port)
      (fill-missing-data inputfile outputdir grammar timeout))))

#-lispworks
(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))