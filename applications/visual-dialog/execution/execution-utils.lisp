(in-package :visual-dialog)

(defun initialize-agent-ontology-and-world (ontology world silent)
  "sets the world and ontology of an agent in ontology"
  (let ((agent-ontology (make-instance 'blackboard)))
    (loop for field in (data-fields ontology)
          do (set-data agent-ontology (first field) (rest field)))
    (set-data agent-ontology 'world world)
    (set-data agent-ontology 'silent silent)
    (set-data agent-ontology 'server-address (get-configuration world :server-address))
    (set-data agent-ontology 'cookie-jar (get-configuration world :cookie-jar)) ;; default configuration is make-instance so that each time we initialize, a new session is started
  agent-ontology))

(defun get-scene-pathname-by-index (index world)
  "given a scene-index return pathname of scene on that position in scenes of world"
  (nth index (scenes world)))

(defmethod get-dialog-by-index (scene-index dialog-index world (dataset (eql :clevr)))
  "returns a list of questions based on a scene-index and dialog-index"
  (let* ((dialog-set-path (nth scene-index (dialog-sets world)))
         (dialog-set (dialogs (load-object 'clevr-dialog-set dialog-set-path)))
         (dialog (nth dialog-index dialog-set))
         (questions (questions dialog)))
      (push (caption dialog) questions)
    questions))

(defmethod get-dialog-by-index (scene-index dialog-index world (dataset (eql :gqa)))
  "returns a list of questions based on a scene-index and dialog-index"
  (let* ((dialog-set-path (nth scene-index (dialog-sets world)))
         (dialog-set (dialogs (load-object 'gqa-dialog-set dialog-set-path)))
         (dialog (nth dialog-index dialog-set))
         (questions (questions dialog)))
      (push (caption dialog) questions)
    questions))

(defmethod get-dialog-by-index (scene-index dialog-index world (dataset (eql :mnist)))
  "returns a list of questions based on a scene-index and dialog-index"
  (let* ((dialog-set-path (nth scene-index (dialog-sets world)))
         (dialog-set (dialogs (load-object 'mnist-dialog-set dialog-set-path)))
         (dialog (nth dialog-index dialog-set))
         (questions (questions dialog)))
    questions))

(defun compute-number-of-dialogs (world)
  "mnist has 3 dialogs, clevr 5"
  (if (or (eql (get-configuration world :dataset) :clevr)
          (eql (get-configuration world :dataset) :gqa))
    4 2))

(defmethod get-gold-answers-by-index (scene-index dialog-index world (dataset (eql :mnist)))
  "returns a list of answers based on a scene-index and dialog-index"
  (let* ((dialog-set-path (nth scene-index (dialog-sets world)))
         (dialog-set (dialogs (load-object 'mnist-dialog-set dialog-set-path)))
         (dialog (nth dialog-index dialog-set))
         (answers (answers dialog)))
    answers))

(defmethod get-gold-answers-by-index (scene-index dialog-index world (dataset (eql :clevr)))
  "returns a list of answers based on a scene-index and dialog-index"
  (let* ((dialog-set-path (nth scene-index (dialog-sets world)))
         (dialog-set (dialogs (load-object 'clevr-dialog-set dialog-set-path)))
         (dialog (nth dialog-index dialog-set))
         (answers (answers dialog)))
    answers))

(defmethod get-gold-answers-by-index (scene-index dialog-index world (dataset (eql :gqa)))
  "returns a list of answers based on a scene-index and dialog-index"
  (let* ((dialog-set-path (nth scene-index (dialog-sets world)))
         (dialog-set (dialogs (load-object 'gqa-dialog-set dialog-set-path)))
         (dialog (nth dialog-index dialog-set))
         (answers (answers dialog)))
    answers))

(defun extract-scene-unit-variable (cipn)
  "returns scene-variable from resulting cfs of given cipn"
  (let* ((cfs (pole-structure (left-pole (car-resulting-cfs (cipn-car cipn)))))
         (scene-unit (find 'clevr-dialog-grammar::scene-unit cfs :key #'first))
         (scene-var (second (find 'clevr-dialog-grammar::scene (rest scene-unit) :key #'first))))
    scene-var))

(defun extract-memory-unit-variable (cipn)
  "returns memory-variable from resulting cfs of given cipn"
  (let* ((cfs (pole-structure (left-pole (car-resulting-cfs (cipn-car cipn)))))
         (memory-unit (find 'clevr-dialog-grammar::memory-unit cfs :key #'first))
         (memory-var (second (find 'clevr-dialog-grammar::memory (rest memory-unit) :key #'first))))
    memory-var))

(defun find-attributes-of-unique (irl-program unique-target-variable)
  "Find all attributes of filter primitives that are linked to the unique-variable"
  (let* (attribute-variable-list
        (unique-primitive (find unique-target-variable irl-program :test #'equal :key #'second))
        (unique-source-variable (third unique-primitive))
        (source-variables-filters (recursively-find-source-variables-filter irl-program unique-source-variable '()))
        (attributes-list (loop for source-var in source-variables-filters
                               for cat-and-attr = (find-attribute-and-category-of-filter irl-program source-var)
                               when (and (not (equal (cdr cat-and-attr) 'thing))
                                         (not (equal (cdr cat-and-attr) 'number)))
                               collect cat-and-attr)))
    attributes-list))

(defun find-input-attributes-of-set-diff (irl-program unique-target-variable)
  "Find all attributes of filter primitives that are linked to the unique-variable"
  (let* (attribute-variable-list
        (unique-primitive (find unique-target-variable irl-program :test #'equal :key #'second))
        (source-variables-filters (recursively-find-source-variables-filter irl-program unique-target-variable '()))
        (attributes-list (loop for source-var in source-variables-filters
                               for cat-and-attr = (find-attribute-and-category-of-filter irl-program source-var)
                               when (and (not (equal (cdr cat-and-attr) 'thing))
                                         (not (equal (cdr cat-and-attr) 'number)))
                               collect cat-and-attr)))
    attributes-list))

(defun recursively-find-source-variables-filter (irl-program start-variable lst)
  "given a target-variable of a filter-primitive (coming from source-variable of unique), find all source variables of linked filter primitives"
  (let ((source-var (find-source-variable-of-filter irl-program start-variable)))
    (if (not source-var)
      lst
      (recursively-find-source-variables-filter irl-program source-var (cons source-var lst)))))

(defun find-source-variable-of-filter (irl-program filter-target-variable)
  "given a target-variable return source-variable of filter primitive"
  (let ((filter-primitive (find filter-target-variable irl-program :test #'equal :key #'second)))
    (if (equal (first filter-primitive) 'filter-by-attribute)
      (third filter-primitive))))

(defun find-attribute-and-category-of-filter (irl-program filter-source-variable)
  "returns attribute and attribute-category of filter primitive of filter-source-variable"
  (let* ((primitives (find-all filter-source-variable irl-program :test #'equal :key #'third))
         (attribute-variable
          (loop for primitive in primitives
                when (equal (first primitive) 'filter-by-attribute)
                return (fifth primitive)))
         (attribute-category (second (find attribute-variable irl-program :test #'equal :key #'third)))
         (attribute (fourth (find attribute-variable irl-program :test #'equal :key #'third)))
         (attr-cat (intern (string-replace attribute-category "-category" "") "KEYWORD")))
    (cons attr-cat attribute)))

(defun make-new-object-with-attributes (object-id attributes attention)
  (make-instance 'object
                 :id object-id
                 :attributes (loop for (category . attribute) in attributes
                                   for cat = category
                                   for attr = (intern (symbol-name attribute))
                                   when (not (equal attribute (or 'thing 'digit)))
                                   collect (cons cat attribute))
                 :attention attention))

(defun make-new-object-without-attributes (object-id attributes attention)
  (make-instance 'object
                 :id object-id
                 :attention attention))

(defun get-target-value (irl-program list-of-bindings)
  "returns the value/binding of the open variable in irl-program"
  (let* ((target-variable (get-target-var irl-program))
         (target-binding (find target-variable list-of-bindings :key #'var)))
    (when target-binding
      (value target-binding))))

(defun get-target-primitive (irl-program)
  "returns primitive of the open variable in irl-program"
  (let* ((var (get-target-var irl-program))
        (target-primitive (first (find var irl-program :test #'equal :key #'second))))
    target-primitive))

(defun get-third-value-target-primitive (irl-program list-of-bindings)
  "return the binding of the third variable in the primitive that contains the open variable"
  (let* ((attribute-variable (third (find (get-target-var irl-program) irl-program :test #'equal :key #'second)))
         (attribute-value (value (find attribute-variable list-of-bindings :key #'var))))
    attribute-value))

(defun get-fourth-value-target-primitive (irl-program list-of-bindings)
  "this returns the fourth item, so the attribute, in case of a query as the primitive with open variable"
  (let* ((attribute-variable (fifth (find (get-target-var irl-program) irl-program :test #'equal :key #'second)))
         (attribute-value (value (find attribute-variable list-of-bindings :key #'var))))
    attribute-value))

(defun find-topic (source-value)
  (if (equal (type-of source-value) 'object-set)
    (loop for object in (objects source-value)
          collect (id object))
    (loop for object in (collect-objects-from-world-model source-value)
          collect (id object))))

(defun add-conversation-memory (memory)
  (add-element '((h1) "Conversation Memory"))
  (add-element (make-html (copy-object memory))))

(defun get-primitive-inventory (world)
  (let ((mode (get-configuration world :mode)))
    (cond ((eql mode :hybrid)
           *subsymbolic-primitives*)
          ((eql mode :symbolic)
           *symbolic-primitives*))))

;;;;;;;; EVALUATION UTILS

#|(defun check-if-number (answer)
  (cond ((eq answer 'zero) 0)
        ((eq answer 'one) 1)
        ((eq answer 'two) 2)
        ((eq answer 'three) 3)
        ((eq answer 'four) 4)
        ((eq answer 'five) 5)
        ((eq answer 'six) 6)
        ((eq answer 'seven) 7)
        ((eq answer 'eight) 8)
        ((eq answer 'nine) 9)
        ((eq answer 'ten) 10)
        ((eq answer 'eleven) 10)
        ((eq answer 'twelve) 10)
        ((eq answer 'thirteen) 10)
        ((eq answer 'fourteen) 10)
        ((eq answer 'fifteen) 10)
        ((eq answer 'sixteen) 10)
        ((eq answer 'seventeen) 10)
        ((eq answer 'eighteen) 10)
        ((eq answer 'nineteen) 10)
        ((eq answer 'twenty) 10)
        (answer)))|#

(defun check-if-number (answer)
  (let* ((string-number (downcase (symbol-name answer)))
         (int-number (number-string-to-int string-number)))
    (if int-number
      int-number
      answer)))
  
#|(defun check-if-string-is-number (answer)
   (cond ((equal answer "0") 0)
         ((equal answer "1") 1)
         ((equal answer "2") 2)
         ((equal answer "3") 3)
         ((equal answer "4") 4)
         ((equal answer "5") 5)
         ((equal answer "6") 6)
         ((equal answer "7") 7)
         ((equal answer "8") 8)
         ((equal answer "9") 9)
         ((equal answer "10") 10)
         (answer)))|#

(defun check-if-string-is-number (answer)
  (if (number-string-to-int answer)
    (number-string-to-int answer)
    answer))

(defun number-string-to-int (number)
  (let ((numbers 
         (loop for i from 0 to 1000
               for number = (format nil "~r" i)
               collect (cons number i))))
    (rest (assoc  number numbers :test #'string=))))

(defun check-if-bgcolor (answer)
   (cond ((equal answer 'white-bg) "white")
         ((equal answer 'cyan-bg) "cyan")
         ((equal answer 'salmon-bg) "salmon")
         ((equal answer 'yellow-bg) "yellow")
         ((equal answer 'silver-bg) "silver")
         (answer)))

(defun check-answers (gold-answers computed-answers)
  (loop for g-a in gold-answers
        for c-a in computed-answers
        for cleaned-c-a = (if (and (symbolp c-a) (equal (search "GQA-" (symbol-name c-a)) 0))
                            (last-elt (split-sequence::split-sequence #\- (symbol-name c-a))))
        collect (cond
                 ((and (equal g-a "none")
                       (equal c-a 'zero))
                  1)
                 ((and (numberp g-a)
                       (not (equal c-a "IRL-FAILED"))
                       (numberp (check-if-number c-a))
                       (= g-a (check-if-number c-a)))
                  1)
                 ((and (stringp g-a)
                       (not (equal c-a "IRL-FAILED"))
                       (numberp (check-if-number c-a))
                       (equal (check-if-string-is-number g-a)
                              (check-if-number c-a)))
                  1)
                 ((and (stringp g-a)
                       (check-if-bgcolor (symbolp c-a))
                       (equalp g-a (check-if-bgcolor c-a)))
                  1)
                 ((and cleaned-c-a
                       (stringp g-a)
                       (symbolp c-a)
                       (equalp g-a cleaned-c-a))
                  1)
                 ((and (stringp g-a)
                       (symbolp c-a)
                       (equalp g-a (symbol-name c-a)))
                  1)
                 (t
                  0))))

(defun collect-objects-from-world-model (world-model)
  (let* ((set-items (set-items world-model))
         (object-set (if set-items
                       (object-set (first set-items))))
         (objects (if object-set (objects object-set))))
    (if (and set-items object-set objects)
      (loop for object in objects
            collect object))))

(defun collect-objects-id-from-world-model (world-model)
  (let* ((set-items (set-items world-model))
         (object-set (if set-items
                       (object-set (first set-items))))
         (objects (if object-set (objects object-set))))
    (if (and set-items object-set objects)
      (loop for object in objects
            collect (id object)))))

