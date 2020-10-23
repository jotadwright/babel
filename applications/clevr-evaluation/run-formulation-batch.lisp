
(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

(defun make-csv-line (&rest args)
  (format nil "~{~a~^,~}" args))

(defun succeededp (cipn)
  (and cipn (eql (first (statuses cipn)) 'fcg::succeeded)))

(defun all-succeededp (cipns)
  (apply #'always (mapcar #'succeededp cipns)))


(defun formulate-until-solution (irl-program id num-solutions)
  "Try to formulate the irl-program until
   num-solutions are found"
  (loop with cipns = nil
        with utterances = nil
        until (all-succeededp cipns)
        for attempt from 1
        for (forms nodes)
        = (multiple-value-list
           (formulate-all irl-program :cxn-inventory *clevr* :silent t :n num-solutions))
        if (all-succeededp nodes)
        do (setf cipns nodes
                 utterances forms)
        else
        do (format t "[~a] formulation attempt ~a~%"
                   id attempt)
        end
        finally (return (values utterances cipns))))

(defun get-utterance-and-formulation-cxns (id irl-program num-solutions)
  "Run formulation until num-solutions are found.
   Export the utterances and the constructions."
  (multiple-value-bind (utterances cipns)
      (formulate-until-solution
       (fcg::instantiate-variables irl-program)
       id num-solutions)
    (values utterances
            (mapcar
             #'(lambda (cipn)
                 (list-of-strings->string
                  (reverse
                   (mapcar (compose #'downcase #'mkstr #'name)
                           (applied-constructions cipn)))))
             cipns))))


(defun process-inputfile (inputfile outputdir num-solutions)
  "Process the inputfile"
  ;; open read/write pipes and create the header
  ;; for the outputfile
  (let* ((lines-to-process (- (number-of-lines inputfile) 1))
         (in-stream (open inputfile :direction :input))
         (outputfile (make-pathname :directory (pathname-directory outputdir)
                                    :name (pathname-name inputfile)
                                    :type (pathname-type inputfile)))
         (outputfile-exists-p (probe-file outputfile))
         (out-stream (open outputfile :direction :output
                           :if-exists :append
                           :if-does-not-exist :create))
         (out-stream-header
          (make-csv-line "id" "utterance" "irl_program"
                         "rpn" "formulation_cxns")))
    ;; when the outputfile already exists, check how many
    ;; lines have already been processed and skip these
    ;; in the inputfile (they no longer need processing)
    (when outputfile-exists-p
      (let ((lines-already-processed
             (round
              (/ (- (number-of-lines outputfile) 1) num-solutions))))
        (decf lines-to-process lines-already-processed)
        (loop repeat lines-already-processed
              do (read-line in-stream nil nil))))
    ;; if the outputfile does not exist, create it and
    ;; write the header to it
    (unless outputfile-exists-p
      (ensure-directories-exist outputfile)
      (write-line out-stream-header out-stream)
      (force-output out-stream))
    ;; loop over the lines, formulate them
    ;; for num-solutions times and
    ;; write to output
    (with-progress-bar (bar lines-to-process ("Processing ~a" (pathname-name inputfile)))
      (loop for line = (remove #\Return (read-line in-stream nil nil))
            while line
            do (let* ((fields (split line #\,))
                      (id (first fields))
                      (irl-program (third fields))
                      (rpn (fourth fields)))
                 (multiple-value-bind (utterances formulation-cxns)
                     (get-utterance-and-formulation-cxns id irl-program num-solutions)
                   (loop for utterance in utterances
                         for cxns in formulation-cxns
                         for out-line = (make-csv-line id utterance irl-program rpn cxns)
                         do (write-line out-line out-stream))
                   (force-output out-stream)
                   (update bar)))))))

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
    (loop for indicator in '(inputfile outputdir max-nr-of-nodes num-solutions)
          unless (getf arg-plist indicator)
          do (error "Missing command line argument: ~a" indicator))
    ;; set the configurations for the CLEVR grammar
    (set-configurations *CLEVR*
                        `((:max-nr-of-nodes . ,(parse-integer (getf arg-plist 'max-nr-of-nodes)))
                          (:cxn-supplier-mode . :all-cxns-except-incompatible-hashed-cxns)
                          (:priority-mode . :nr-of-applied-cxns))
                        :replace t)
    ;; process the inputfile
    (process-inputfile (getf arg-plist 'inputfile)
                       (getf arg-plist 'outputdir)
                       (parse-integer (getf arg-plist 'num-solutions)))))

#-lispworks 
(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))
