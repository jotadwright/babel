;;;; json-encoders.lisp

(in-package :clevr-web-service)

;;; make sexpr
(defgeneric make-sexpr (thing &key &allow-other-keys))

(defmethod make-sexpr ((set clevr-object-set) &key substitutions)
  (loop for object in (objects set)
        collect `((:color . ,(color object))
                  (:shape . ,(shape object))
                  (:material . ,(material object))
                  (:size . ,(size object))
                  (:id . ,(downcase
                           (mkstr
                            (rest
                             (assoc (id object) substitutions))))))))

(defmethod make-sexpr ((object clevr-object) &key substitutions)
  `((:color . ,(color object))
    (:shape . ,(shape object))
    (:material . ,(material object))
    (:size . ,(size object))
    (:id . ,(downcase
             (mkstr
              (rest
               (assoc (id object) substitutions)))))))

(defmethod make-sexpr ((bc boolean-category) &key)
  (downcase (mkstr (id bc))))

(defmethod make-sexpr ((mc material-category) &key)
  (downcase (mkstr (material mc))))

(defmethod make-sexpr ((cc color-category) &key)
  (downcase (mkstr (color cc))))

(defmethod make-sexpr ((sc size-category) &key)
  (downcase (mkstr (size sc))))

(defmethod make-sexpr ((sc shape-category) &key)
  (downcase (mkstr (shape sc))))

(defparameter *clevr-predicate-arities*
  '((count! . 1) (equal-integer . 2) (less-than . 2) (greater-than . 2)
    (equal? . 2) (exist . 1) (filter . 1) (get-context . 0) (intersect . 2)
    (query . 1) (relate . 1) (same . 1) (union . 2) (unique . 1)))

;;;; encode-irl-program
;;;; put the irl-program in an s-expr that can be
;;;; easily encoded in a json string
#|
(defun encode-irl-program (irl-program &optional list-of-bindings)
  (let* ((reverse-polish (program->rpn (preprocess-program irl-program))))
    (loop for rpn-predicate in reverse-polish
          for irl-predicates = (find-all (first rpn-predicate) irl-program :key #'first)
          for the-predicate = (if (length= irl-predicates 1)
                                (first irl-predicates)
                                (loop for possible-predicate in irl-predicates
                                      for bind-statements = (linked-bind-statement possible-predicate irl-program)
                                      when (eql (bind-statement-value bind-statement)
                                                (second rpn-predicate))
                                      return possible-predicate))
          for output-var = (second the-predicate)
          for output-value = (when list-of-bindings
                               (value (find output-var list-of-bindings :key #'var)))
          collect `((:name . ,(downcase (mkstr (first predicate))))
                    (:arity . ,(rest (assoc (first predicate) *clevr-predicate-arities*)))
                    (:arg . ,(when (length> predicate 1)
                               (if (eql (first predicate) 'filter)
                                 (downcase (mkstr (third predicate)))
                                 (downcase (mkstr (second predicate))))))
                    (:output . ,(when output-value (make-sexpr output-value)))))))
|#

(defun extract-predicate (possible-predicates rpn-predicate irl-program processed-predicates)
  (if (length= possible-predicates 1)
    (first possible-predicates)
    (or (loop for possible-predicate in possible-predicates
              for bind-statement = (linked-bind-statement possible-predicate irl-program)
              when (eql (bind-statement-value bind-statement)
                        (last-elt rpn-predicate))
              return possible-predicate)
        (loop with best-pos = 100
              with best-p
              for possible-predicate in possible-predicates
              for input = (first (input-vars possible-predicate))
              for all-vars = (find-all-anywhere-if #'variable-p processed-predicates)
              for pos = (position input all-vars)
              when (and pos (< pos best-pos))
              do (setf best-pos pos
                       best-p possible-predicate)
              finally (return best-p)))))

(defun encode-irl-program (irl-program id-subs &optional list-of-nodes)
  (let* ((reverse-polish (program->rpn (preprocess-program-for-web-service irl-program))))
    (loop with processed-predicates = nil
          for rpn-predicate in reverse-polish
          for possible-predicates = (find-all (first rpn-predicate) irl-program :key #'first)
          for the-predicate = (extract-predicate possible-predicates
                                                 rpn-predicate
                                                 irl-program
                                                 processed-predicates)
          for node = (find the-predicate list-of-nodes
                           :key #'(lambda (node)
                                    (first (primitives-evaluated node)))
                           :test #'equal)
          for output-var = (second the-predicate)
          for output-value = (when node
                               (value (find output-var (bindings node) :key #'var)))
          do (push the-predicate processed-predicates)
          collect `((:name . ,(downcase (mkstr (first rpn-predicate))))
                    (:arity . ,(rest (assoc (first rpn-predicate) *clevr-predicate-arities*)))
                    (:arg . ,(when (length> rpn-predicate 1)
                               (if (eql (first rpn-predicate) 'filter)
                                 (downcase (mkstr (third rpn-predicate)))
                                 (downcase (mkstr (second rpn-predicate))))))
                    (:output . ,(when output-value (make-sexpr output-value :substitutions id-subs)))
                    (:status . ,(if node
                                  (downcase (mkstr (irl:status node)))
                                  "not-executed"))))))


;;;; TO DO: write functions for decoding an IRL program from JSON
(defun decode-bind-statement (value var)
  (let* ((all-categories (loop for field in (fields *clevr-ontology*)
                               for field-data = (get-data *clevr-ontology* field)
                               when (listp field-data)
                               append field-data))
         (category (find value all-categories
                         :key #'(lambda (cat) (downcase (mkstr (id cat))))
                         :test #'string=)))
    `(bind ,(type-of category) ,var ,(read-from-string value))))

(defun decode-predicate (json)
  (let* ((name (read-from-string (rest (assoc :name json))))
         (output-var (make-var 'var))
         (input-vars (loop repeat (rest (assoc :arity json))
                           collect (make-var 'var)))
         (bind-var (when (rest (assoc :arg json))
                     (make-var 'binding)))
         (bind-statement
          (when bind-var
            (decode-bind-statement (rest (assoc :arg json)) bind-var))))
    (values (append
             (list name output-var)
             (when input-vars input-vars)
             (when bind-var (list bind-var)))
            bind-statement)))

(defun add-predicate (stack predicate irl-program arity)
  ;; arity = 1; get 1 elem from the stack and link its output to the new predicate's input
  ;; arity = 2; get 2 elem from the stack and link each of their outputs to the new predicate's inputs
  (cond
   ((= arity 0)
    ;; push the predicate on the stack
    ;; do nothing with the IRL program
    (values (append (list predicate) stack) irl-program))
   ((= arity 1)
    ;; get 1 elem from the stack and link its output to the new predicate's input
    ;; remove the previous from the stack and add it to the irl program
    ;; push the new predicate on the stack
    (let* ((prev-predicate (pop stack))
           (output-var (second prev-predicate)))
      (setf (nth 2 predicate) output-var)
      (values (append (list predicate) stack)
              (append (list prev-predicate) irl-program))))
   ((= arity 2)
    ;; get 2 elem from the stack and link their outputs to the new predicate's inputs
    ;; remove the previous predicates from the stack and add them to the irl program
    ;; push the new predicate on the stack
    (let* ((prev-predicate-1 (pop stack))
           (prev-predicate-2 (pop stack))
           (output-var-1 (second prev-predicate-1))
           (output-var-2 (second prev-predicate-2)))
      (setf (nth 2 predicate) output-var-1
            (nth 3 predicate) output-var-2)
      (values (append (list predicate) stack)
              (append (list prev-predicate-1 prev-predicate-2) irl-program))))))

(defun decode-irl-program (json)
  ;; the decoder expects a list of json objects, representing
  ;; the IRL program in reverse polish notation. 
  (let ((stack (list (decode-predicate (first json))))
        irl-program
        bind-statements)
    (loop for i from 1
          while stack
          for obj = (nth i json)
          for arity = (rest (assoc :arity obj))
          if (null obj)
          do (push (pop stack) irl-program)
          else
          do (multiple-value-bind (predicate bind-statement)
                 (decode-predicate obj)
               (multiple-value-bind (new-stack new-irl-program)
                   (add-predicate stack predicate irl-program arity)
                 (when bind-statement
                   (push bind-statement bind-statements))
                 (setf stack new-stack
                       irl-program new-irl-program))))
    (append irl-program bind-statements)))

#|
(decode-irl-program 
 '(((:name . "get-context") (:arity . 0) (:arg . nil))
   ((:name . "filter") (:arity . 1) (:arg . "red"))
   ((:name . "unique") (:arity . 1) (:arg . nil))
   ((:name . "exist") (:arity . 1) (:arg . nil))))

(decode-irl-program
 '(((:name . "get-context") (:arity . 0) (:arg . nil))
   ((:name . "filter") (:arity . 1) (:arg . "red"))
   ((:name . "get-context") (:arity . 0) (:arg . nil))
   ((:name . "filter") (:arity . 1) (:arg . "blue"))
   ((:name . "union!") (:arity . 2) (:arg . nil))))
|#
