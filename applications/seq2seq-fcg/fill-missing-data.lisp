(ql:quickload :seq2seq-fcg)
(in-package :seq2seq-fcg)

;;;; Helper functions
;;;; ----------------

(defun succeededp (cipn)
  "Check if the goal test succeeded"
  (and cipn (eql (first (statuses cipn)) 'fcg::succeeded)))

(defun get-depth-of-solution (cipn)
  (length (all-parents cipn)))

;;;; Formulation
;;;; -----------

(defun formulate-until-solution (irl-program rpn timeout)
  (set-data (blackboard *CLEVR*) :rpn-input rpn)
  (loop with cipn = nil
        with utterance = nil
        until (succeededp cipn)
        for (form node)
        = (multiple-value-list
           (handler-case
               (with-timeout (timeout)
                 (formulate irl-program :cxn-inventory *CLEVR*))
             (timeout-error (e)
               (values nil nil))))
        when (succeededp node)
        do (progn
             (remove-data (blackboard *CLEVR*) :rpn-input)
             (setf cipn node
                   utterance form))
        finally
        (return
         (values
          (list-of-strings->string utterance)
          (list-of-strings->string
           (reverse
            (mapcar (compose #'downcase #'mkstr #'name)
                    (applied-constructions cipn))))
          (get-depth-of-solution cipn)))))

(defun repair-formulate (irl-program rpn timeout)
  (set-data (blackboard *CLEVR*) :rpn-input rpn)
  (multiple-value-bind (utterance cipn)
      (handler-case
          (with-timeout (timeout)
            (formulate irl-program
                       :cxn-inventory *CLEVR*))
        (timeout-error (e)
          (values nil nil)))
    (remove-data (blackboard *CLEVR*) :rpn-input)
    (if (succeededp cipn)
      (values (list-of-strings->string utterance)
              (list-of-strings->string
               (reverse
                (mapcar (compose #'downcase #'mkstr #'name)
                        (applied-constructions cipn))))
              (get-depth-of-solution cipn))
      (values nil nil))))

;;;; Main Functions
;;;; --------------

(defun set-seq2seq-configurations (port)
  (let ((configs
         `(;; use the seq2seq model in formulation
           (:cxn-supplier-mode . :ordered-by-label-hashed+seq2seq)
           (:priority-mode . :seq2seq-additive-with-sets)
           (:parse-order hashed cxn)
           (:production-order hashed-lex cxn hashed-morph)
           (:seq2seq-endpoint . #-ccl ,(format nil "http://localhost:~a/next-cxn" port)
                                #+ccl ,(format nil "http://127.0.0.1:~a/next-cxn" port))
           (:seq2seq-model-comprehension . "clevr_comprehension_model")
           (:seq2seq-model-formulation . "clevr_formulation_model_v1")
           ;; remove the max nr of nodes
           (:node-tests :check-duplicate)
           (:hash-mode . :hash-string-meaning-lex-id)
           (:queue-mode . :greedy-best-first)
           (:cxn-sets-with-sequential-application hashed-lex hashed-morph))))
    (set-configurations *CLEVR* configs :replace t)
    (set-configurations (processing-cxn-inventory *CLEVR*)
                        configs :replace t)))


(defun set-depth-first-configurations ()
  (let ((configs
         '((:queue-mode . :greedy-best-first)
           (:cxn-supplier-mode . :ordered-by-label-hashed)
           (:hash-mode . :hash-string-meaning-lex-id)
           (:priority-mode . :nr-of-applied-cxns)
           (:parse-order hashed cxn)
           (:production-order hashed-lex nom cxn hashed-morph)
           (:node-tests :check-duplicate)
           (:cxn-sets-with-sequential-application hashed-lex hashed-morph))))
    (set-configurations *clevr* configs :replace t)
    (set-configurations (processing-cxn-inventory *CLEVR*)
                        configs :replace t)))


(defun fill-missing-data (inputfile outputdir timeout)
  ;; first, open a port to the inputfile and skip the header line
  ;; next, create the outputfile and write the header line
  ;; finally, process lines with no cxns and skip the other ones
  (let* ((nr-of-lines (number-of-lines inputfile))
         (in-stream (open inputfile :direction :input))
         (header (read-line in-stream nil nil))
         (outputfile (make-pathname :directory (pathname-directory outputdir)
                                    :name (pathname-name inputfile)
                                    :type (pathname-type inputfile)))
         (missing-solutions 0)
         (repaired-solutions 0)
         out-stream)
    (ensure-directories-exist outputfile)
    (if (probe-file outputfile)
      (let ((processed-lines (- (number-of-lines outputfile) 1)))
        (loop repeat processed-lines
              do (read-line in-stream nil nil))
        (setf out-stream (open outputfile :direction :output
                               :if-exists :append))
        (setf nr-of-lines (- nr-of-lines processed-lines)))
      (progn (setf out-stream (open outputfile :direction :output))
        (write-line header out-stream)))
    (with-progress-bar (bar (- nr-of-lines 1) ("Processing ~a" (namestring inputfile)))
      (do-csv (row in-stream)
        (destructuring-bind (id irl-program rpn utterance formulation-cxns depth) row
          (declare (ignorable depth utterance))
          (if (string= formulation-cxns "None")
            (multiple-value-bind (repair-utterance repair-cxns repair-depth)
                (formulate-until-solution (fcg::instantiate-variables
                                           (read-from-string irl-program))
                                          rpn timeout)
                ;(repair-formulate (fcg::instantiate-variables
                ;                   (read-from-string irl-program))
                ;                  rpn timeout)
              (incf missing-solutions)
              (if (and repair-utterance repair-cxns repair-depth)
                (let ((repair-row (list id irl-program rpn
                                        repair-utterance
                                        repair-cxns
                                        repair-depth)))
                  (incf repaired-solutions)
                  (write-csv-row repair-row :stream out-stream))
                (write-csv-row row :stream out-stream)))
            (write-csv-row row :stream out-stream)))
        (force-output out-stream)
        (update bar)))
    (format t "~%~%Repaired ~a out of ~a missing solutions"
            repaired-solutions missing-solutions)
    (close in-stream)
    (force-output out-stream)
    (close out-stream)))


;;;; Testing stuff
;;;; -------------

#|

 (activate-monitor trace-fcg)
 
 (set-seq2seq-configurations 8888)

 (fill-missing-data
  (parse-namestring
   "/Users/jensnevens/Desktop/seq2seq/input/batch-235.csv")
  (parse-namestring
   "/Users/jensnevens/Desktop/seq2seq/output/")
  400)
 
 (fill-missing-data
  (babel-pathname :directory '("applications" "seq2seq-fcg")
                  :name "batch-0" :type "csv")
  (babel-pathname :directory '("applications" "seq2seq-fcg" "out"))  400)

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
    (print "Received command line arguments:")
    (print args)
    (loop for indicator in '(inputfile outputdir timeout strategy)
          unless (getf args indicator)
          do (error "Missing command line argument: ~a" indicator))
    (let ((timeout (parse-integer (getf args 'timeout "400")))
          (port (parse-integer (getf args 'port "8888")))
          (inputfile (parse-namestring (getf args 'inputfile)))
          (outputdir (parse-namestring (getf args 'outputdir)))
          (strategy (make-kw (upcase (getf args 'strategy "seq2seq")))))
      (case strategy
        (:seq2seq (set-seq2seq-configurations port))
        (:depth-first (set-depth-first-configurations)))
      (fill-missing-data inputfile outputdir timeout))))

#-lispworks
(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))