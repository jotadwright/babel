(ql:quickload :seq2seq-fcg)
(in-package :seq2seq-fcg)

;;;; append to output
;;;; --------------------------------------------------
    
(defun append-to-output (inputfile outputfile solution-rate)
  ;; if the file does not yet exist, create it and write the header
  (unless (probe-file outputfile)
    (with-open-file (out-stream outputfile :direction :output)
      (write-line "model_id,solution_rate" out-stream)))
  ;; append to outputfile
  (with-open-file (out-stream outputfile :direction :output
                              :if-exists :append)
    (write-line (format nil "~a,~a"
                        (pathname-name inputfile)
                        solution-rate)
                out-stream))
  t)

;;;; cxn supplier with sequence of cxns
;;;; --------------------------------------------------

(defclass cxn-supplier-with-sequence-of-cxns ()
  ((cxn-sequence
    :type list :initarg :cxn-sequence :accessor cxn-sequence)
   (attempted-nodes
    :type list :initarg :attempted-nodes :accessor attempted-nodes)))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :cxn-sequence)))
  (make-instance 'cxn-supplier-with-sequence-of-cxns
                 :cxn-sequence (find-data (blackboard (construction-inventory node))
                                          'cxn-sequence)
                 :attempted-nodes nil))
                 
(defmethod next-cxn ((cxn-supplier cxn-supplier-with-sequence-of-cxns)
                     (node cip-node))
  (unless (find node (attempted-nodes cxn-supplier))
    (push node (attempted-nodes cxn-supplier))
    (nth (length (all-parents node))
         (cxn-sequence cxn-supplier))))

;;;; comprehension
;;;; --------------------------------------------------

(defmethod comprehend-cxn-sequence (utterance cxn-sequence
                                    &key (cxn-inventory *fcg-constructions*) 
                                    (silent nil) &allow-other-keys)
  (let ((package
         (cond ((eq cxn-inventory *clevr*)
                :clevr-grammar)
               ((eq cxn-inventory *clevr-dialog*)
                :clevr-dialog-grammar))))
    (set-configurations cxn-inventory
                        '((:cxn-supplier-mode . :cxn-sequence)
                          (:priority-mode . :nr-of-applied-cxns)
                          (:node-expansion-mode . :default)
                          (:shuffle-cxns-before-application . nil))
                        :replace t)
    (set-data (blackboard cxn-inventory) 'cxn-sequence
              (loop with cxn-list = (constructions-list cxn-inventory)
                    for cxn-name in cxn-sequence
                    collect (get-processing-cxn
                             (find (intern (upcase cxn-name) package)
                                   cxn-list :key #'name))))
    (comprehend utterance :cxn-inventory cxn-inventory :silent silent)))

;;;; formulation
;;;; --------------------------------------------------

(defmethod formulate-cxn-sequence (irl-program cxn-sequence
                                   &key (cxn-inventory *fcg-constructions*)
                                   (silent nil) &allow-other-keys)
  (let ((package
         (cond ((eq cxn-inventory *clevr*)
                :clevr-grammar)
               ((eq cxn-inventory *clevr-dialog*)
                :clevr-dialog-grammar))))
    (set-configurations cxn-inventory
                        '((:cxn-supplier-mode . :cxn-sequence)
                          (:priority-mode . :nr-of-applied-cxns)
                          (:node-expansion-mode . :default)
                          (:shuffle-cxns-before-application . nil))
                        :replace t)
    (set-data (blackboard cxn-inventory) 'cxn-sequence
              (loop with cxn-list = (constructions-list cxn-inventory)
                    for cxn-name in cxn-sequence
                    collect (get-processing-cxn
                             (find (intern (upcase cxn-name) package)
                                   cxn-list :key #'name))))
    (formulate irl-program :cxn-inventory cxn-inventory :silent silent)))

;;;; main function
;;;; --------------------------------------------------

(defun prediction-accuracy (inputfile outputfile direction cxn-inventory)
  (let ((nr-of-lines (number-of-lines inputfile))
        solution-rate)
    (with-open-file (stream inputfile :direction :input)
      (with-progress-bar (bar nr-of-lines ("Processing ~a" (namestring inputfile)))
        (do-csv (row stream :skip-first-p t)
          (destructuring-bind (id utterance
                               irl_program rpn
                               formulation_cxns
                               sorted_formulation_cxns) row
            (declare (ignorable index id utterance rpn formulation_cxns))
            (let ((irl-program (read-from-string irl_program))
                  (cxn-sequence (split sorted_formulation_cxns #\space)))
              (case direction
                (:formulation
                 (multiple-value-bind (utterance cipn)
                     (cond ((eq cxn-inventory *clevr*)
                            (when (length> cxn-sequence 1)
                              (formulate-cxn-sequence irl-program cxn-sequence
                                                      :cxn-inventory cxn-inventory))))
                   (declare (ignorable utterance))
                   (if (and cipn (find 'fcg::succeeded (fcg::statuses cipn)))
                     (push 1 solution-rate) (push 0 solution-rate)))))))
          (update bar))))
    (format t "~%~%Solution rate: ~a~%" (average solution-rate))
    (ensure-directories-exist outputfile)
    (append-to-output inputfile outputfile (average solution-rate))
    (format t "Done!~%")))

#|

 (activate-monitor trace-fcg)
 
(prediction-accuracy
 (parse-namestring "/Users/jensnevens/Projects/seq2seq/data/data-no-cxn-sets/CLEVR_val_corpus_formulation_v2_sorted.csv")
 (babel-pathname :directory '(".tmp") :name "test" :type "csv")
 :formulation *CLEVR*)
 
|#

;;;; Command line interface
;;;; --------------------------------------------------

(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))

(defun main (args)
  "Read the input file, line per line, check if the
   sequence of constructions leads to a solution
   and add a line to the output file containing the
   ID and the accuracy"
  (let ((arg-plist (args->plist args)))
    (loop for indicator in '(inputfile outputfile grammar direction)
          unless (getf arg-plist indicator)
          do (error "Missing command line argument: ~a" indicator))
    (prediction-accuracy (parse-namestring (getf arg-plist 'inputfile))
                         (parse-namestring (getf arg-plist 'outputfile))
                         (make-kw (upcase (getf arg-plist 'direction)))
                         (eval (internal-symb (upcase (getf arg-plist 'grammar)))))))

#-lispworks
(main #+ccl ccl:*unprocessed-command-line-arguments*)
