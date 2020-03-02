
(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

;; #######################################
;; The epilog script will read the outputs
;; of the separate jobs and combine it
;; into a single output file
;; ######################################

(defun merge-batch-files (&key inputdir outputfile)
  ;; check some stuff
  (unless (probe-file inputdir)
    (error "Could not find the directory ~a" inputdir))
  ;; print some stuff
  (format t "~%~%************************Running epilog************************")
  (format t "~%Reading input from ~a" (namestring inputdir))
  (format t "~%Writing output to ~a" (namestring outputfile))
  ;; merge the batches
  (ensure-directories-exist outputfile)
  (with-open-file (out outputfile :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((num-files (length
                      (directory
                       (make-pathname :directory (namestring inputdir)
                                      :name :wild :type "csv")))))
      (loop for i from 0 below num-files
         for infile = (make-pathname :directory (namestring inputdir)
                                     :name (format nil "output-batch-~a" i)
                                     :type "csv")
           do (format t "Reading from ~a~%" (namestring infile))
         do (with-open-file (in infile :direction :input)
              (loop for line = (read-line in nil nil)
                 while line
                   do (write-line line out)))))))
        

(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))


(defun main (args)
  (let ((arg-plist (args->plist args)))
    (loop for indicator in '(inputdir outputfile)
          unless (getf arg-plist indicator)
          do (error "Missing command line argument: ~a" indicator))
    (merge-batch-files :inputdir (parse-namestring (getf arg-plist 'inputdir))
                       :outputfile (parse-namestring (getf arg-plist 'outputfile)))
    (format t "~%Done!")))

(main ccl:*unprocessed-command-line-arguments*)
