(ql:quickload :seq2seq-fcg)
(in-package :seq2seq-fcg)

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
    (loop for indicator in '(inputfile outputfile)
          unless (getf arg-plist indicator)
          do (error "Missing command line argument: ~a" indicator))
    (prediction-accuracy (parse-namestring (getf arg-plist 'inputfile))
                         (parse-namestring (getf arg-plist 'outputfile)))))

(main ccl:*unprocessed-command-line-argument*)
