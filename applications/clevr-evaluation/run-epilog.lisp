
(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

;; #######################################
;; The epilog script will read the outputs
;; of the separate jobs and combine it
;; into a single output file
;; ######################################

(defun merge-batch-files (inputdir outputfile &key (type :wild))
  ;; check that the inputdir exists
  (unless (probe-file inputdir)
    (error "Could not find the directory ~a" inputdir))
  ;; print some stuff
  (format t "~%~%************************Running epilog************************")
  (format t "~%Reading input from ~a" (namestring inputdir))
  (format t "~%Writing output to ~a" (namestring outputfile))
  ;; create the path to outputfile
  (ensure-directories-exist outputfile)
  ;; open the pipe to outputfile
  (with-open-file (out-stream outputfile :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
    ;; get all files (of type) from inputdir
    (let ((files (directory
                  (make-pathname :directory (pathname-directory inputdir)
                                 :name :wild :type type))))
      (loop for file in files
            do (format t "Reading from ~a~%" (namestring file))
            do (with-open-file (in-stream file :direction :input)
                 (loop for line = (read-line in-stream nil nil)
                       while line
                       do (write-line line out-stream)))))))
        

(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))


(defun main (args)
  (let ((arg-plist (args->plist args)))
    ;; parse the command line arguments
    (loop for indicator in '(inputdir outputfile type)
          unless (getf arg-plist indicator)
          do (error "Missing command line argument: ~a" indicator))
    ;; merge the batch files
    (merge-batch-files (parse-namestring (getf arg-plist 'inputdir))
                       (parse-namestring (getf arg-plist 'outputfile))
                       :type (getf arg-plist 'type))
    (format t "~%Done!")))

(main ccl:*unprocessed-command-line-arguments*)
