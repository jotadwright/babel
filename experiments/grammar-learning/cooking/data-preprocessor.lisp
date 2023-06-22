(ql:quickload :xmls)
(ql:quickload :cl-json)
(ql:quickload :utils)
(ql:quickload :muhai-cookingbot)

(defparameter *input-files*
  (directory
   (babel-pathname
    :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark"
                 "data" "gold-standard-solutions" "utterance-and-meaning")
    :name :wild :type "xml")))

(defparameter *output-file*
  (babel-pathname
   :directory '("experiments" "grammar-learning" "cooking" "data")
   :name "benchmark-instructions" :type "jsonl"))

;; execute the steps of each recipe
;; extract accessible entities (except kitchen states)
;; and add bind statements
;; The last element of the bind statement should not be
;; the instance, but the class name of the instance.

;(all-recipe-xml-files->json *input-files* *output-file*)

(defun all-recipe-xml-files->json (list-of-paths output-file)
  (ensure-directories-exist output-file)
  (with-open-file (output-stream output-file :direction :output :if-exists :supersede)
    (loop for path in list-of-paths
          do (recipe-xml-file->json path output-stream))))

(defun recipe-xml-file->json (filepath output-stream)
  (let* ((xml-tree (with-open-file (stream filepath :direction :input)
                     (xmls:parse stream)))
         (ingredients (xml-get-children-with-tag xml-tree "ingredients"))
         (instructions (xml-get-children-with-tag xml-tree "instructions"))
         ;(jsonl-data (append (xml-nodes->json ingredients)
         ;                    (xml-nodes->json instructions)))
         (jsonl-data (xml-nodes->json instructions))
         )
    (loop for line in jsonl-data
          do (write-line (cl-json:encode-json-alist-to-string line) output-stream))))

(defun xml-get-children-with-tag (xml-tree tag)
  (xmls:xmlrep-children
   (xmls::xmlrep-find-child-tag tag xml-tree)))

(defun xml-nodes->json (list-of-xml-nodes)
  (loop for node in list-of-xml-nodes
        for json-node = (xml-node->json node)
        when json-node collect json-node))

(defun xml-node->json (xml-node)
  (let* ((utterance-node (xmls:xmlrep-find-child-tag "utterance" xml-node))
         (utterance (xmls:xmlrep-string-child utterance-node nil))
         (meaning-node (xmls:xmlrep-find-child-tag "meaning" xml-node))
         (meaning (xmls:xmlrep-string-child meaning-node nil)))
    (when (and utterance meaning)
      (let ((processed-utterance
             (utils:remove-spurious-spaces
              (utils:remove-punctuation utterance)))
            (processed-meaning
             (intern-set-of-predicates
              (read-from-string
               (utils:downcase
                (format nil "(~a)"
                        (utils:remove-newlines
                         (utils:remove-spurious-spaces meaning)))))
              :muhai-cookingbot)))
        ;; remove get-kitchen
        (setf processed-meaning
              (remove 'muhai-cookingbot::get-kitchen processed-meaning :key #'first))
        ;; expand contants into bindings!
        (setf processed-meaning
              (constants->bind-statements processed-meaning)) 
        `((:utterance . ,processed-utterance)
          (:meaning . ,(utils:mkstr processed-meaning)))))))

(defun intern-set-of-predicates (set-of-predicates package)
  (loop for predicate in set-of-predicates
        collect (loop for elem in predicate
                      if (numberp elem) collect elem
                      else collect (intern (utils:mkstr elem) package))))

(defun constants->bind-statements (irl-program)
  (loop for predicate in irl-program
        append (constants-in-predicate->bind-statements predicate)))

(defun constants-in-predicate->bind-statements (predicate)
  (let* ((non-var-elements (utils:find-all-if-not #'utils:variable-p (rest predicate)))
         (substitutions (loop for el in non-var-elements
                              collect (cons el (utils:make-var))))
         (new-predicate (substitute-bindings substitutions predicate))
         (bind-statements
          (loop for (el . var) in substitutions
                collect (cond ((numberp el) `(bind muhai-cookingbot::quantity ,var ,el))
                              ((subtypep el 'muhai-cookingbot::ingredient) `(bind muhai-cookingbot::ingredient ,var ,el))
                              ((subtypep el 'muhai-cookingbot::cooking-utensil) `(bind muhai-cookingbot::cooking-utensil ,var ,el))
                              ((subtypep el 'muhai-cookingbot::unit) `(bind muhai-cookingbot::unit ,var ,el))
                              ((subtypep el 'muhai-cookingbot::shape) `(bind muhai-cookingbot::shape ,var ,el))
                              ((subtypep el 'muhai-cookingbot::pattern) `(bind muhai-cookingbot::pattern ,var ,el))
                              ((subtypep el 'muhai-cookingbot::heating-mode) `(bind muhai-cookingbot::heating-mode ,var ,el))
                              ((subtypep el 'muhai-cookingbot::container) `(bind muhai-cookingbot::container ,var ,el))
                              (t (error "Unknown type: ~a" el))))))
    (append (list new-predicate) bind-statements)))

(defun substitute-bindings (bindings x)
  "Substitute all variables in x with their binding as specified in bindings."  
  (labels ((aux (x)
             (cond ((assoc x bindings :test #'eq)
                    (let ((y (assoc x bindings :test #'eq)))
                      (if (and y (not (eq (cdr y) x))) (aux (cdr y)) x)))
                   ((atom x) x)
                   (t (cons (aux (car x)) (aux (cdr x)))))))
    (aux x)))
              












