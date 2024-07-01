(in-package :au-benchmark.benchmark)

(defparameter *clevr-data-path*
  (merge-pathnames
   (make-pathname :directory '(:relative :up "data")
                  :name "clevr-au-benchmark-raw" :type "csv")
   *load-pathname*))

(defun clevr-au-data->kswap-benchmark-format (input-file)
  (let* ((input-data
          (with-open-file (stream input-file :direction :input)
            (loop for line = (read-line stream nil nil)
                  while line collect line)))
         (unique-input-data
          (remove-duplicates input-data :test #'string=))
         (output-path
          (merge-pathnames
           (make-pathname :directory '(:relative :up "data")
                          :name "clevr-au-benchmark-v1.1" :type "lisp")
           *load-pathname*)))
    (with-open-file (stream output-path :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create)
      (loop for entry in unique-input-data
            for lisp-entry = (read-from-string entry)
            for G1 = (fresh-variables (first lisp-entry))
            for G2 = (fresh-variables (second lisp-entry))
            for G1-length = (length G1)
            for G2-length = (length G2)
            for G1-vars = (length (remove-duplicates (find-all-anywhere-if #'variable-p G1)))
            for G2-vars = (length (remove-duplicates (find-all-anywhere-if #'variable-p G2)))
            for var-coeff = (variable-coefficient G1 G2)
            for atom-coeff = (atoms-coefficient G1 G2)
            for data-line = (list G1 G2 G1-length G2-length G1-vars G2-vars var-coeff atom-coeff)
            do (reset-id-counters)
            do (unless (or (find 'dummy G1 :key #'first)
                           (find 'dummy G2 :key #'first))
                 (write-line (format nil "~a" data-line) stream))))))

;(clevr-au-data->kswap-benchmark-format *clevr-data-path*)
            