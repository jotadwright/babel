;;;; json-encoders.lisp

(in-package :clevr-web-service)

;;; make sexpr
(defmethod make-sexpr ((set clevr-object-set))
  (loop for object in (objects set)
        collect `((:color . ,(color object))
                  (:shape . ,(shape object))
                  (:material . ,(material object))
                  (:size . ,(size object))
                  (:id . ,(id object)))))

(defmethod make-sexpr ((object clevr-object))
  `((:color . ,(color object))
    (:shape . ,(shape object))
    (:material . ,(material object))
    (:size . ,(size object))
    (:id . ,(id object))))

(defmethod make-sexpr ((bc boolean-category))
  (downcase (mkstr (id bc))))

(defmethod make-sexpr ((mc material-category))
  (downcase (mkstr (material mc))))

(defmethod make-sexpr ((cc color-category))
  (downcase (mkstr (color cc))))

(defmethod make-sexpr ((sc size-category))
  (downcase (mkstr (size sc))))

(defmethod make-sexpr ((sc shape-category))
  (downcase (mkstr (shape sc))))

(defparameter *clevr-predicate-arities*
  '((count! . 1) (equal-integer . 2) (less-than . 2) (greater-than . 2)
    (equal? . 2) (exist . 1) (filter . 1) (get-context . 0) (intersect . 2)
    (query . 1) (relate . 1) (same . 1) (union . 2) (unique . 1)))

;;;; encode-irl-program
;;;; put the irl-program in an s-expr that can be
;;;; easily encoded in a json string
(defun encode-irl-program (irl-program &optional list-of-bindings)
  (let* ((reverse-polish (program->rpn irl-program)))
    (loop for predicate in reverse-polish
          for output-var = (second (find (first predicate) irl-program :key #'first))
          for output-value = (when list-of-bindings
                               (value (find output-var list-of-bindings :key #'var)))
          collect `((:name . ,(downcase (mkstr (first predicate))))
                    (:arity . ,(rest (assoc (first predicate) *clevr-predicate-arities*)))
                    (:arg . ,(when (length> predicate 1)
                               (if (eql (first predicate) 'filter)
                                 (downcase (mkstr (third predicate)))
                                 (downcase (mkstr (second predicate))))))
                    (:output . ,(when output-value (make-sexpr output-value)))))))

;;;; TO DO: write functions for decoding an IRL program from JSON
(defun decode-irl-program (sexpr)
  sexpr)

