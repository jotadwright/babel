
(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

(defun make-csv-line (&rest args)
  (format nil "~{~a~^,~}" args))

(defun succeededp (cipn)
  (and cipn (eql (first (statuses cipn)) 'fcg::succeeded)))


(defun comprehend-until-solution (utterance id)
  (loop with cipn = nil
        with meaning = nil
        until (succeededp cipn)
        for attempt from 1
        for (irl-program node)
        = (multiple-value-list
           (comprehend utterance :cxn-inventory *clevr* :silent t))
        if (succeededp node)
        do (setf cipn node
                 meaning irl-program)
        else
        do (format t "[~a] comprehension attempt ~a~%"
                   id attempt)
        end
        finally (return (values meaning cipn))))


(defun get-meaning-and-comprehension-cxns (input-line)
  "Run comprehension until a solution is found.
   Export the meaning network and the applied constructions."
  (let* ((fields (split input-line #\,))
         (id (parse-integer (first fields)))
         (utterance (second fields)))
    (multiple-value-bind (irl-program cipn)
        (comprehend-until-solution utterance id)
      (declare (ignorable cipn))
      (format t "[~a] comprehension done~%" id)
      (values (downcase (mkstr irl-program))
              (clevr-meaning->rpn irl-program)
              (list-of-strings->string
               (reverse
                (mapcar (compose #'downcase #'mkstr #'name)
                        (applied-constructions cipn))))))))


(defun process-inputfile (inputfile outputdir)
  ;; the outputfile has the same name as the inputfile,
  ;; but in the outputdir. if the outputfile already exists,
  ;; check how many lines it has and append to it
  ;; (i.e. continuing the work)
  ;; Also, the outputfile contains all columns from the inputfile
  ;; but adds additional information.
  (let* ((outputfile (make-pathname :directory (pathname-directory outputdir)
                                    :name (pathname-name inputfile)
                                    :type (pathname-type inputfile)))
         (in-stream (open inputfile :direction :input))
         (in-stream-header (remove #\Return (read-line in-stream)))
         (outputfile-exists-p (probe-file outputfile))
         (out-stream (open outputfile :direction :output
                           :if-exists :append
                           :if-does-not-exist :create))
         (out-stream-header
          (make-csv-line in-stream-header "irl_program"
                         "rpn" "comprehension_cxns")))
    (when outputfile-exists-p
      (let ((lines-already-processed (- (number-of-lines outputfile) 1)))
        (loop repeat lines-already-processed
              do (read-line in-stream nil nil))))
    (unless outputfile-exists-p
      (ensure-directories-exist outputfile)
      (write-line out-stream-header out-stream)
      (force-output out-stream))
    (loop for line = (remove #\Return (read-line in-stream nil nil))
          while line
          do (multiple-value-bind (irl-program rpn comprehension-cxns)
                 (get-meaning-and-comprehension-cxns line)
               (let ((out-line
                      (make-csv-line line irl-program rpn comprehension-cxns)))
                 (write-line out-line out-stream)
                 (force-output out-stream))))))

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
    (loop for indicator in '(inputfile outputdir max-nr-of-nodes)
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
                       (getf arg-plist 'outputdir))))

(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))
