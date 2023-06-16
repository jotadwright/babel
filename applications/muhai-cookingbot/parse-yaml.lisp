;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calls the python ontology generator ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-ontology-from-yaml ()
  (let ((python-program "codegenLisp.py")
        (infile (namestring (babel-pathname :directory
                                            '("applications" "muhai-cookingbot")
                                            :name "ontology"
                                            :type "yaml")))
        (outfile (namestring (babel-pathname :directory
                                             '("applications" "muhai-cookingbot")
                                             :name "ontology-generated"
                                             :type "lisp"))))
    (uiop:with-current-directory ((babel-pathname :directory
                                                  '("applications" "muhai-cookingbot")))
      (multiple-value-bind (output error return-value)
          (asdf::run-program `("python" ,python-program
                               "-i" ,infile
                               "-o" ,outfile)
                             :output :string)
        (format t "~a.py script output: ~a~%error: ~a~%exit-code: ~a~%" python-program output error return-value)
        (when (= return-value 0) (format t "exited successful~%"))))))

(generate-ontology-from-yaml)
