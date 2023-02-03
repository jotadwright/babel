(in-package :propbank-grammar)

(defun export-argm-strings (list-of-propbank-sentences
                            &key (corpus-path (babel-pathname
                                               :directory '("grammars" "propbank-grammar" "raw-data")
                                               :name "argm-phrases"
                                               :type "json")))
  "Write argm type + string to json file, to be used as a corpus for training a classifier."

  (with-open-file (argm-corpus corpus-path :direction :output :if-exists :supersede)
    (loop for sentence in list-of-propbank-sentences
          for rolesets = (all-rolesets sentence)
          for ts-unit-structure = (ts-unit-structure sentence *fcg-constructions*)
          do (loop for roleset in rolesets
                   for gold-frames = (find-all roleset (propbank-frames sentence) :key #'frame-name :test #'equalp)
                   do (loop for gold-frame in gold-frames
                            for argm-units = (loop for unit-w-role in (units-with-role ts-unit-structure gold-frame)
                                                   when (search "ARGM" (role-type (first unit-w-role)))
                                                     collect unit-w-role)
                            do (loop for unit in argm-units
                                     for argm-type = (intern (upcase (role-type (car unit))))
                                     for argm-string = (feature-value (assoc 'string (unit-body (cdr unit))))
                                     do (format argm-corpus "~a~%"
                                                (cl-json:encode-json-alist-to-string (list (cons 'item
                                                                                                 (list (cons 'label argm-type)
                                                                                                       (cons 'phrase argm-string))))))))))))