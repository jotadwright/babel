
(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

;; #######################################
;; The prolog script will divide the total
;; input file over several separate input
;; files. Additionally, it creates a .csv
;; file that can be used to create a job
;; array.
;; ######################################

(defun split-into-batchfiles (&key inputfile batchdir datafile batchsize)
  ;; check some stuff
  (unless (probe-file inputfile)
    (error "The file ~a could not be found." inputfile))
  ;; print some stuff
  (format t "~%~%************************Running prolog************************")
  (format t "~%Reading input from ~a" (namestring inputfile))
  (format t "~%Writing batch files to ~a" (namestring batchdir))
  (format t "~%Writing data lines to ~a" (namestring datafile))
  (format t "~%Using batchsize ~a" batchsize)
  ;; determine the number of batches
  (let ((number-of-lines (number-of-lines inputfile))
        (batch-directory (pathname-directory batchdir))
        (input-name (pathname-name inputfile))
        (input-type (pathname-type inputfile))
        (in (open inputfile :direction :input))
        (data-lines nil))
    (multiple-value-bind (number-of-complete-batches number-of-lines-in-last-batch)
        (floor number-of-lines batchsize)
      ;; write the complete batches
      (dotimes (n number-of-complete-batches)
        (let ((outputfile (make-pathname :directory batch-directory
                                         :name (format nil "~a-batch-~a" input-name n)
                                         :type input-type)))
          (ensure-directories-exist outputfile)
          (with-open-file (out outputfile :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (loop repeat batchsize
                  do (write-line (read-line in) out)))
          (push (list n outputfile batchsize) data-lines)))
      ;; do the final batch
      (when (> number-of-lines-in-last-batch 0)
        (let ((outputfile (make-pathname :directory batch-directory
                                         :name (format nil "~a-~a" input-name
                                                       number-of-complete-batches)
                                         :type input-type)))
          (ensure-directories-exist outputfile)
          (with-open-file (out outputfile :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (loop repeat number-of-lines-in-last-batch
                  do (write-line (read-line in) out)))
          (push (list number-of-complete-batches
                      outputfile
                      number-of-lines-in-last-batch)
                data-lines))))
    ;; close the input stream
    (close in)
    ;; write the data file
    (ensure-directories-exist datafile)
    (with-open-file (data datafile :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (write-line "index, file, batchsize" data)
      (loop for line in (reverse data-lines)
            for list-of-strings = (mapcar #'mkstr line)
            for string = (format nil "~{~a~^, ~}" list-of-strings)
            do (write-line string data)))))
            
      
(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))


(defun main (args)
  (let ((arg-plist (args->plist args)))
    (loop for indicator in '(inputfile batchdir datafile batchsize)
          unless (getf arg-plist indicator)
          do (error "Missing command line argument: ~a" indicator))
    (split-into-batchfiles :inputfile (parse-namestring (getf arg-plist 'inputfile))
                           :batchdir (parse-namestring (getf arg-plist 'batchdir))
                           :datafile (parse-namestring (getf arg-plist 'datafile))
                           :batchsize (parse-integer (getf arg-plist 'batchsize)))
    (format t "~%Done!")))

(main ccl:*unprocessed-command-line-arguments*)