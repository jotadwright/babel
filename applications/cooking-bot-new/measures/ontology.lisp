(in-package :cooking-bot-new)

(defparameter *persistent-id-table-defaults* (make-hash-table))
(defparameter *persistent-id-table-introduced* (make-hash-table))
(defparameter *persistent-id-table-solved* (make-hash-table))

;;;;; Measure: Record questions introduced by ontology

(define-monitor questions-introduced-by-ontology
                :class 'data-recorder :average-window 1)

(define-event-handler (questions-introduced-by-ontology irl::evaluate-irl-program-finished)
  (let* ((set-slots (calculate-set-slots irl::solution-nodes *persistent-id-table-introduced*)))
    (multiple-value-bind (questions vars)
        (find-introduced-slots set-slots)
      (record-value monitor questions)
     ; (if questions
        (setf (slot-value monitor 'values)
                          (cons (list questions vars) (slot-value monitor 'values)))
        )))

(defun find-introduced-slots (slots)
  (let ((questions (loop for slots-in-node in slots
                         for introduced-slots-in-node = (loop for slots-in-object in slots-in-node
                                                              for introduced-slots-in-object = 
                                                                (sum-nested-list slots-in-object)
                                                              sum introduced-slots-in-object)
                         collect introduced-slots-in-node))
        (vars (loop for slots-in-node in slots
                    for introduced-slots-in-node = (loop for slots-in-object in slots-in-node
                                                         for introduced-slots-in-object = (vars-nested-list slots-in-object)
                                                         collect introduced-slots-in-object)
                    collect introduced-slots-in-node)))
    (values questions vars)))

(define-monitor questions-solved-by-ontology
                :class 'data-recorder :average-window 1)

(define-event-handler (questions-solved-by-ontology irl::evaluate-irl-program-finished)
  (multiple-value-bind (answers vars)
      (find-solved-slots (calculate-set-slots irl::solution-nodes *persistent-id-table-defaults*))
    (print "these are the defaults")
      (print answers)
    (record-value monitor answers)
    (setf (slot-value monitor 'values) (cons (list answers vars) (slot-value monitor 'values)))))

(defun find-solved-slots (slots)
  (let* ((questions (loop for slots-in-node in slots
                         for introduced-slots-in-node = (loop for slots-in-object in slots-in-node
                                                              for introduced-slots-in-object = (sum-nested-list-defaults slots-in-object)
                                                              sum introduced-slots-in-object)
                         collect introduced-slots-in-node))
         (vars (loop for slots-in-node in slots
                     for introduced-slots-in-node = (loop for slots-in-object in slots-in-node
                                                          for introduced-slots-in-object = (vars-nested-list-defaults slots-in-object)
                                                          collect introduced-slots-in-object)
                     collect introduced-slots-in-node)))
    (values questions vars)))

(defun find-solved-slots-by-simulation (slots)
  (let* ((questions (loop for slots-in-node in slots
                         for introduced-slots-in-node = (loop for slots-in-object in slots-in-node
                                                              for introduced-slots-in-object = (sum-nested-list-answers slots-in-object)
                                                              sum introduced-slots-in-object)
                         collect introduced-slots-in-node))
         (vars (loop for slots-in-node in slots
                     for introduced-slots-in-node = (loop for slots-in-object in slots-in-node
                                                          for introduced-slots-in-object = (vars-nested-list-answers slots-in-object)
                                                          collect introduced-slots-in-object)
                     collect introduced-slots-in-node)))
    (values questions vars)))



(defun sum-nested-list (question-answer-list)
  (loop for question-answer in question-answer-list
        for val = (if (not (listp (first question-answer)))
                    (first question-answer)
                    (sum-nested-list question-answer))
          sum val))

(defun vars-nested-list (question-answer-list)
  (loop for question-answer in question-answer-list
        for val = (if (not (listp (first question-answer)))
                    (fifth question-answer)
                    (vars-nested-list question-answer))
        collect val))

(defun sum-nested-list-defaults (question-answer-list)
  (loop for question-answer in question-answer-list
        for val = (if (not (listp (second question-answer)))
                    (second question-answer)
                    (sum-nested-list-defaults question-answer))
          sum val))

(defun vars-nested-list-defaults (question-answer-list)
  (loop for question-answer in question-answer-list
        for val = (if (not (listp (second question-answer)))
                    (cons (fifth question-answer) (last-elt question-answer))
                    (vars-nested-list-defaults question-answer))
          collect val))

(defun sum-nested-list-answers (question-answer-list)
  (loop for question-answer in question-answer-list
        for val = (if (not (listp (second question-answer)))
                    (third question-answer)
                    (sum-nested-list-answers question-answer))
          sum val))

(defun vars-nested-list-answers (question-answer-list)
  (loop for question-answer in question-answer-list
        for val = (if (not (listp (second question-answer)))
                    (cons (fifth question-answer) (last-elt question-answer))
                    (vars-nested-list-answers question-answer))
          collect val))

(defun calculate-set-slots (solution-nodes table)
  (if solution-nodes
    (let* ((all-nodes (reverse (cons (first solution-nodes) (irl::all-parents (first solution-nodes)))))
           (set-slots
            (loop for node in all-nodes
                  for set-slots = (calculate-set-slot (find-new-bindings node) table)
                  if set-slots
                    collect set-slots
                  else
                    collect (list (list (list 0 0 0 nil nil nil))))))
     ; (print set-slots)
      set-slots)
    (values nil nil)))
      
(defun find-new-bindings (node)
  (let* ((bs (irl::bindings node))
         (bs-previous (if (irl::all-parents node)
                        (irl::bindings (first (irl::all-parents node)))
                        nil))
         (bound-bindings (loop for b in bs if (value b) collect b))
         (bound-bindings-previous (if bs-previous (loop for b in bs-previous if (value b) collect b)))
         (new-bound-bindings (if bs-previous (set-difference bound-bindings bound-bindings-previous)
                               bound-bindings))
         (new-bound-bindings (sort new-bound-bindings #'string-lessp :key #'var))
         ;(new-bound-bindings-length (length new-bound-bindings))
         )
    new-bound-bindings))

(defun calculate-set-slot (bindings table)
  (if bindings
    (let* ((vals 
            (loop for binding in bindings
                  for value = (value binding)
                  collect value))
           (sorted-vals  (sort vals #'string-lessp :key #'id))
           (questions-and-answers (calculate-questions-and-answers-from-values sorted-vals table)))
      questions-and-answers)))


(defun calculate-questions-and-answers-from-object-no-parent-value (value slots table)
  (loop for slot in slots
        for slot-name = (intern (symbol-name (first (harlequin-common-lisp:slot-definition-initargs slot))) 'cooking-bot-new)
        for child-slot-value = (slot-value value slot-name)
        if (and (not child-slot-value)
                (not (equal slot-name 'id))
                (not (equal slot-name 'persistent-id))
                (not (equal slot-name 'used))
                (not (equal slot-name 'is-concept))
                (not (equal slot-name 'value)))
          collect (list 1 0 0 value slot-name nil child-slot-value) ; if there is no parent-value and the slot has no value, then there is a question and no answer
        if (and child-slot-value
                (not (equal slot-name 'id))
                (not (equal slot-name 'persistent-id))
                (not (equal slot-name 'used))
                (not (equal slot-name 'is-concept))
                (not (equal slot-name 'value)))
          collect (list 1 1 0 value slot-name nil child-slot-value) ; if there is no parent-value and the slot has a value, then the question is immediately answered by the ontology itself (defaults)
        if (and (not (equal (type-of value) 'kitchen-state))
                (subtypep (type-of child-slot-value) 'kitchen-entity)
                (not (listp child-slot-value)))
          append (calculate-questions-and-answers-from-values (list child-slot-value) table)
        if  (and (not (equal (type-of value) 'kitchen-state))
                 (listp child-slot-value))
                  ;(subtypep (type-of  child-slot-value)) 'kitchen-entity))
          append (loop for val in child-slot-value
                       if (subtypep (type-of val) 'kitchen-entity)
                         append (calculate-questions-and-answers-from-values (list val) table))))

(defun calculate-questions-and-answers-from-object (value parent-value slots table)
  (loop for slot in slots
        for slot-name = (intern (symbol-name (first (harlequin-common-lisp:slot-definition-initargs slot))) 'cooking-bot-new)
        for child-slot-value = (slot-value value slot-name)
        for child-slot-val = (if (subtypep (type-of child-slot-value) 'kitchen-entity)
                               (persistent-id child-slot-value)
                               (if (and (listp child-slot-value)
                                        (subtypep (type-of (first child-slot-value)) 'kitchen-entity))
                                 (persistent-id (first child-slot-value))
                                 child-slot-value)) ; if the slot-value is an object, then compare the persistent ids of the object
        for parent-slot-value = (slot-value parent-value slot-name)
        for parent-slot-val = (if (subtypep (type-of parent-slot-value) 'kitchen-entity)
                                (persistent-id parent-slot-value)
                                (if (and (listp parent-slot-value)
                                         (subtypep (type-of (first parent-slot-value)) 'kitchen-entity))
                                  (persistent-id (first parent-slot-value))
                                  parent-slot-value)) ; if the slot-value is an object, then compare the persistent ids of the object
        if (and child-slot-val parent-slot-val
                (not (equal child-slot-val parent-slot-val))
                (not (equal slot-name 'id))
                (not (equal slot-name 'persistent-id))
                (not (equal slot-name 'used))
                (not (equal slot-name 'is-concept))
                (not (equal slot-name 'value)))
          collect (list 1 0 1 value slot-name parent-slot-val child-slot-val) ; if there is a parent-value and the slot has changed, then there is a question that is answered by mental simulation
        if (and child-slot-value
                (not parent-slot-val)
                (not (equal child-slot-val parent-slot-val))
                (not (equal slot-name 'id))
                (not (equal slot-name 'persistent-id))
                (not (equal slot-name 'used))
                (not (equal slot-name 'is-concept))
                (not (equal slot-name 'value)))
          collect (list 0 0 1 value slot-name parent-slot-val child-slot-val) ; if there is a parent-value with an empty slot, then the question is answeredby mental simulation
          if (and (not (equal (type-of value) 'kitchen-state))
                  (subtypep (type-of child-slot-value) 'kitchen-entity)
                  (not (listp child-slot-value)))
          append (calculate-questions-and-answers-from-values  (list child-slot-value) table)
          if  (and (not (equal (type-of value) 'kitchen-state))
                   (listp child-slot-value))
                  ;(subtypep (type-of  child-slot-value)) 'kitchen-entity))
                  
          append (loop for val in child-slot-value
                       if (subtypep (type-of val) 'kitchen-entity)
                         append (calculate-questions-and-answers-from-values  (list val) table)) ;;reverse important for order in which bindings get processed, not good
         ; append (calculate-questions-and-answers-from-values (list child-slot-value) (closer-mop:class-slots child-slot-value))
; if (subtypep (type-of child-slot-value) 'kitchen-entity)
            ))

(defun calculate-questions-and-answers-from-values (vals table)
  (loop for value in vals
        for slots =  (closer-mop:class-slots (class-of value)) ;; don't count id and persistent-id
        for pers-id =  (persistent-id value) 
        for parent-value = (gethash (persistent-id value) table)
        for new-children-slots = (if (not parent-value)
                                   (calculate-questions-and-answers-from-object-no-parent-value value slots table))
        for changed-children-slots  = (if parent-value
                                        (calculate-questions-and-answers-from-object value parent-value slots table))
        do (if pers-id (setf (gethash (persistent-id value) table) value)) ; only do this if there is a persistent-id, otherwise, no way of tracking entities
        if (and (not (equal (type-of value) 'kitchen-state))
                new-children-slots)
          collect new-children-slots
        if (and (not (equal (type-of value) 'kitchen-state))
                changed-children-slots)
          collect changed-children-slots))
  
  
(defun find-new-and-previous-bindings (node)
  (let* ((bs (irl::bindings node))
         (bs-previous (if (irl::all-parents node)
                        (irl::bindings (first (irl::all-parents node)))
                        nil))
         (bound-bindings (loop for b in bs if (value b) collect b))
         (bound-bindings-previous (if bs-previous (loop for b in bs-previous if (value b) collect b)))
         (new-bound-bindings (if bs-previous (set-difference bound-bindings bound-bindings-previous)
                               bound-bindings)))
    new-bound-bindings))

(defun find-relevant-slots ()
  (let* ((ke (make-instance 'kitchen-entity))
         (subclasses (remove-duplicates (find-all-subclasses (class-of ke) nil)))
         (class-counter 0)
         (slot-counter 0)
         (slots 
          (remove-duplicates
           (loop for subclass in subclasses
                 do (incf class-counter)
                 append (let ((slots (closer-mop:class-slots subclass)))
                          (loop for slot in slots
                                for args = (intern (symbol-name (first (harlequin-common-lisp:slot-definition-initargs slot))))
                                do (incf slot-counter)
                                collect args))))))
    (setf slots (remove 'id slots))
    (setf slots (remove 'persistent-id slots))))

(defun find-all-subclasses (class lst)
  (let ((subclasses (harlequin-common-lisp:class-direct-subclasses class)))
    (if (not subclasses)
      (append subclasses lst)
      (loop for subclass in subclasses
            append (find-all-subclasses subclass (append subclasses lst))))))













;(setf *test* (make-instance 'kitchen-entity))
;(slot-value *test* 'persistent-id)
;(setf (persistent-id *test*) 'iets)

(defun slot-setf-method-factory ()
  (let* ((ke (make-instance 'kitchen-entity))
         (subclasses (remove-duplicates (find-all-subclasses (class-of ke) nil)))
         (class-counter 0)
         (slot-counter 0))
    (loop for subclass in subclasses
          do (incf class-counter)
          do (let ((slots (closer-mop:class-slots subclass)))
               (loop for slot in slots
                     for args = (intern (symbol-name (first (harlequin-common-lisp:slot-definition-initargs slot))))
                     do (incf slot-counter)
                       do (format t "slot ~a in class ~a~%" args subclass)
                     do (eval `(defmethod (setf ,args) :before (new-value (o ,subclass))
                                 (let* ((previous-value (,args o))
                                        (slot-values (if previous-value
                                                       (list 1 1)
                                                       (list 0 1))))
                                   ;(print-kiekeboe)
                                   (notify set-slot slot-values)))))))
    (print class-counter)
    (print slot-counter)))



(defun find-all-subclasses (class lst)
  (let ((subclasses (harlequin-common-lisp:class-direct-subclasses class)))
    (if (not subclasses)
      (append subclasses lst)
      (loop for subclass in subclasses
            append (find-all-subclasses subclass (append subclasses lst))))))


