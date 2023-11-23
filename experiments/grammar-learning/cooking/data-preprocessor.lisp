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

;; execute the steps of each recipe
;; extract accessible entities (except kitchen states)
;; and add bind statements
;; The last element of the bind statement should not be
;; the instance, but the class name of the instance.

;(all-recipe-xml-files->json *input-files* *output-file*)
;(all-recipe-xml-files->prompt *input-files*)

(defun all-recipe-xml-files->prompt (list-of-paths)
  (loop for path in list-of-paths
        append (recipe-xml-file->prompt path)))

(defun recipe-xml-file->prompt (filepath)
  (let* ((xml-tree (with-open-file (stream filepath :direction :input)
                     (xmls:parse stream)))
         (ingredients (xml-get-children-with-tag xml-tree "ingredients"))
         (instructions (xml-get-children-with-tag xml-tree "instructions"))
         (data (append (xml-nodes->prompt ingredients)
                       (xml-nodes->prompt instructions))))
    data))

(defun xml-nodes->prompt (list-of-xml-nodes)
  (loop for node in list-of-xml-nodes
        for prompt = (xml-node->prompt node)
        when prompt collect prompt))

(defun xml-node->prompt (xml-node)
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
        ;; collect prompt string
        (format nil "utterance: ~a; meaning: ~a"
                processed-utterance
                (utils:downcase (utils:mkstr processed-meaning)))))))


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
          (:meaning . ,processed-meaning))))))

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
              








(defparameter *baking-test-files*
  (directory (parse-namestring "/Users/jensnevens/Projects/cooking-llm-baseline/raw-data/test/baking/*.xml")))
(defparameter *salad-test-files*
  (directory (parse-namestring "/Users/jensnevens/Projects/cooking-llm-baseline/raw-data/test/salads/*.xml")))
(defparameter *baking-train-files*
  (directory (parse-namestring "/Users/jensnevens/Projects/cooking-llm-baseline/raw-data/train/baking/*.xml")))
(defparameter *salad-train-files*
  (directory (parse-namestring "/Users/jensnevens/Projects/cooking-llm-baseline/raw-data/train/salads/*.xml")))

(defun parse-recipe-xml-file (filepath)
  (let* ((xml-tree (with-open-file (stream filepath :direction :input)
                     (xmls:parse stream)))
         (ingredients (xml-get-children-with-tag xml-tree "ingredients"))
         (instructions (xml-get-children-with-tag xml-tree "instructions"))
         (alist-data (append (xml-nodes->json ingredients)
                             (xml-nodes->json instructions))))
    alist-data))

(defun find-test-primitives-in-train (train-data test-data)
  (let* ((train-meanings (loop for elem in train-data
                               append (rest (assoc :meaning elem))))
         (train-primitives (remove-duplicates (mapcar #'first train-meanings)))
         (test-meanings (loop for elem in test-data
                              append (rest (assoc :meaning elem))))
         (test-primitives (remove-duplicates (mapcar #'first test-meanings))))
    (loop for primitive in test-primitives
          always (member primitive train-primitives))))

(defparameter *baking-test-data*
  (loop for file in *baking-test-files*
        append (parse-recipe-xml-file file)))
(defparameter *salad-test-data*
  (loop for file in *salad-test-files*
        append (parse-recipe-xml-file file)))
(defparameter *baking-train-data*
  (loop for file in *baking-train-files*
        append (parse-recipe-xml-file file)))
(defparameter *salad-train-data*
  (loop for file in *salad-train-files*
        append (parse-recipe-xml-file file)))

;; do all the primitives that occur in the test recipes
;; also occur in the train recipes?
;(find-test-primitives-in-train *baking-train-data* *baking-test-data*)
;(find-test-primitives-in-train *salad-train-data* *salad-test-data*)


;; write data per line
(defparameter *output-file*
  (parse-namestring "/Users/jensnevens/Projects/cooking-llm-baseline/raw-data/test/baking.csv"))

(with-open-file (stream *output-file* :direction :output)
  (loop for elem in *baking-test-data*
        for str = (format nil "~a; ~a"
                          (utils::downcase (utils::mkstr (rest (assoc :utterance elem))))
                          (utils::downcase (utils::mkstr (rest (assoc :meaning elem)))))
        do (write-line str stream)))
