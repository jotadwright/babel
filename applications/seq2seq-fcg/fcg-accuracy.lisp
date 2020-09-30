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
    (loop for indicator in '(inputfile outputfile grammar direction)
          unless (getf arg-plist indicator)
          do (error "Missing command line argument: ~a" indicator))
    (prediction-accuracy (parse-namestring (getf arg-plist 'inputfile))
                         (parse-namestring (getf arg-plist 'outputfile))
                         (internal-symb (upcase (getf arg-plist 'direction)))
                         (eval (internal-symb (upcase (getf arg-plist 'grammar))))
                         :skip-lines (when (getf arg-plist 'skip-lines)
                                       (parse-integer (getf arg-plist 'skip-lines)))
                         :max-lines (when (getf arg-plist 'max-lines)
                                      (parse-integer (getf arg-plist 'max-lines))))))

(main ccl:*unprocessed-command-line-arguments*)
