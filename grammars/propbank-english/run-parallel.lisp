
(ql:quickload :propbank-english)
(ql:quickload :corpus-processing)
(ql:quickload :cl-csv)

(in-package :propbank-english)

;;;; Propbank Grammar
;;;; ----------------------------------------

(defparameter *restored-300-grammar*
  ;; The propbank grammar with cleaning parameter 300
  ;; is used by default. This could become a command
  ;; line option in the future.
  (restore
   (babel-pathname :directory '("grammars" "propbank-english" "grammars")
                   :name "propbank-grammar-ontonotes-ewt-cleaned-300"
                   :type "fcg")))


;;;; Main functionality
;;;; ----------------------------------------

(defun process-csv-line (line &key (cxn-inventory *fcg-constructions*) csv-header)
  ;; Function for processing a single line of the CSV file.
  ;; Don't do any processing if the line is the header of the CSV file.
  ;; Otherwise, get the first and second column (index and utterance)
  ;; and process the utterance with the propbank grammar.
  ;; The results are written to a .json file.
  ;; All CSV columns after the utterance are copied to the .json file.
  ;; The script first tries to parse these values as integers.
  ;; If they cannot be parsed as integers, the value is written as-is
  ;; (probably as a string).
  (let ((columns (cl-csv:read-csv-row line)))
    (unless (equal columns csv-header)
      (let* ((index (parse-integer (first columns)))
             (utterance (second columns))
             (remaining-headers (subseq csv-header 2))
             (remaining-data (subseq columns 2)))
        (cl-json:encode-json-alist-to-string
         (append
          (multiple-value-bind (solution cipn frame-set)
              (comprehend-and-extract-frames utterance :silent t
                                             :cxn-inventory cxn-inventory)
            (declare (ignorable cipn))
            (if (eql solution 'time-out)
              `((:index . ,index)
                (:frame-set . nil)
                (:utterance . ,utterance))
              `((:index . ,index)
                (:frame-set  . ,(loop for frame in (frames frame-set)
                                      collect `((:frame-name . ,(frame-name frame))
                                                (:roles . ,(append
                                                            `(((:role . "V")
                                                               (:string . ,(fel-string (frame-evoking-element frame)))
                                                               (:indices . ,(indices (frame-evoking-element frame)))))
                                                            (loop for fe in (frame-elements frame)
                                                                  collect `((:role . ,(fe-role fe))
                                                                            (:string . ,(fe-string fe))
                                                                            (:indices . ,(indices fe)))))))))
                (:utterance . ,utterance))))
          (loop for header in remaining-headers
                for datum in remaining-data
                collect (cons (make-kw header)
                              (if (parse-integer datum :junk-allowed t)
                                (parse-integer datum)
                                datum)))))))))
  

(defun extract-frames-from-corpus (command-line-args)
  ;; get the header of the csv file
  (let ((csv-header
         (with-open-file (in-stream (getf command-line-args 'inputfile)
                                    :direction :input)
           (cl-csv:read-csv-row (read-line in-stream)))))
    ;; process corpus with threads
    ;; pass along the cxn inventory and
    ;; the csv header as keyword arguments
    (process-corpus-with-threads
     :function #'process-csv-line
     :function-kwargs (list :cxn-inventory *restored-300-grammar*
                            :csv-header csv-header)
     :inputfile (getf command-line-args 'inputfile)
     :outputfile (getf command-line-args 'outputfile)
     :number-of-threads (getf command-line-args 'number-of-threads)
     :number-of-lines-per-thread (getf command-line-args 'number-of-lines-per-thread)
     :write-empty-lines-p nil)))
  

;;;; Command line interface
;;;; ----------------------------------------

(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i)
        collect (internal-symb (upcase arg))
        else collect arg))

(defun process-args (args)
  ;; inputfile and outputfile should be
  ;; proper pathnames
  (setf (getf args 'inputfile)
        (parse-namestring (getf args 'inputfile)))
  (setf (getf args 'outputfile)
        (parse-namestring (getf args 'outputfile)))
  (ensure-directories-exist (getf args 'outputfile))
  ;; number-of-threads and number-of-lines-per-thread
  ;; should be integers
  (setf (getf args 'number-of-threads)
        (parse-integer (getf args 'number-of-threads)))
  (setf (getf args 'number-of-lines-per-thread)
        (parse-integer (getf args 'number-of-lines-per-thread)))
  ;; return the processed args
  args)
  

(defun main (args)
  (let ((arg-plist (args->plist args)))
    ;; check the required command line args
    (loop for arg in '(inputfile outputfile
                       number-of-threads
                       number-of-lines-per-thread)
          unless (getf arg-plist arg)
          do (error "Missing command line argument: ~a" arg))
    ;; process the command line args (convert type etc.)
    (setf arg-plist (process-args arg-plist))
    (format t "~%Parsed the following command line arguments:")
    (print arg-plist)
    ;; Call the main function using the parsed arguments
    (extract-frames-from-corpus arg-plist)))

#|
How to call this script:

ccl -l run-parallel.lisp -b
    -- inputfile some-file.csv
       outputfile some-file.json
       number-of-threads some-integer
       number-of-lines-per-thread some-integer

sbcl --load run-parallel.lisp --quit
     inputfile some-file.csv
     outputfile some-file.json
     number-of-threads some-integer
     number-of-lines-per-thread some-integer


NOTE:
This script makes the following assumptions:
- The input file is a .csv file, containing at least 2 columns:
  1. an 'index' column
  2. an 'utterance' column
  The input file should contain a single utterance per line.
  Additional columns can be specified, but will not be processed
  by the script. These columns will simply be copied to the
  output file.
- The output file has a .json extension, but isn't properly
  formatted as such. It contains a json object per line.
- Next to input and output files, please specify the number
  of threads to use (this depends on your machine's hardware)
  and the number of lines per thread (this depends on your corpus).
  For example, if you have 20 threads available and a corpus of
  100000 lines, you could specify 20 threads and 5000 lines per thread.
  However, you could also specify 20 threads and 1000 lines per thread,
  or even 20 threads and 100 lines per thread.
  By reducing the number of lines per thread, you create more batches,
  but the memory requirement per thread will be reduced, as all results
  of these X lines per thread must be stored until the thread is done.
|#

#-lispworks
(main #+ccl ccl:*unprocessed-command-line-arguments*
      #+sbcl (rest sb-ext:*posix-argv*))

