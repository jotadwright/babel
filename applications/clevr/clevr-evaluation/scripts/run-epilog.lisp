
(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

;; #######################################
;; The epilog script will read the outputs
;; of the separate jobs and combine it
;; into a single output file
;; ######################################

(defun merge-batch-files (inputdir outputfile
                                   &key (type :wild)
                                   (skip-header t))
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
    ;; and sort them by name
    (let ((files (sort (directory
                        (make-pathname :directory (pathname-directory inputdir)
                                       :name :wild :type type))
                       #'string< :key #'namestring)))
      (loop for file in files
            for lines-read = 0
            do (format t "~%Reading from ~a" (namestring file))
            do (with-open-file (in-stream file :direction :input)
                 (when skip-header
                   (read-line in-stream nil nil))
                 (loop for line = (read-line in-stream nil nil)
                       while line
                       do (write-line line out-stream)
                       do (incf lines-read)))
            do (format t "~%Read ~a lines" lines-read)))))
        

(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))


(defun main (args)
  (let ((arg-plist (args->plist args)))
    ;; parse the command line arguments
    (loop for indicator in '(inputdir outputfile type skip-header)
          unless (getf arg-plist indicator)
          do (error "Missing command line argument: ~a" indicator))
    ;; merge the batch files
    (merge-batch-files (parse-namestring (getf arg-plist 'inputdir))
                       (parse-namestring (getf arg-plist 'outputfile))
                       :type (getf arg-plist 'type)
                       :skip-header (= (parse-integer (getf arg-plist 'skip-header)) 1))
    (format t "~%Done!")))

#|
How to call this script:
ccl -l run-epilog -b
    -- inputdir path/to/folder/with/csv/files/
       outputfile path/to/file.csv
       type csv
       skip-header 1
|#

#-lispworks
(main #+ccl ccl:*unprocessed-command-line-arguments*)
