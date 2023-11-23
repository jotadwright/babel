(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calls the python ontology generator ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-ontology-from-yaml ()
  (let ((python-program (namestring (babel-pathname :directory '("applications" "muhai-cookingbot")
                                    :name"codegenLisp"
                                    :type "py")))
        (infile (namestring (babel-pathname :directory
                                            '("applications" "muhai-cookingbot")
                                            :name "ontology"
                                            :type "yaml")))
        (outfile (namestring (babel-pathname :directory
                                             '("applications" "muhai-cookingbot")
                                             :name "ontology-generated"
                                             :type "lisp"))))
    (unless (probe-file outfile)
      (utils:exec-and-return "python3" python-program "-i" infile "-o" outfile))))
(generate-ontology-from-yaml)
