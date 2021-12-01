
(in-package :irl)

;; ############################################################################
;; evaluation-spec definition:
;; ----------------------------------------------------------------------------

(export '(bind ontology binding-objects))

(defclass evaluation-spec ()
  ((pattern :type list :initform nil :initarg :pattern :accessor pattern
            :documentation "The original pattern as defined in the primitive")
   (bound-slot-names :type list :initform nil :initarg :bound-slot-names :accessor bound-slot-names
                     :documentation "A list of slot names for bound slots")
   (unbound-slot-names :type list :initform nil :initarg :unbound-slot-names :accessor unbound-slot-names
                       :documentation "A list of slot names for unbound slots")
   (function :type function :initform nil :initarg :function :accessor evaluation-spec-function
             :documentation "The body of the evaluation spec")
   (bound-slots-pattern :type list :initform nil :initarg :bound-slots-pattern :accessor bound-slots-pattern
                        :documentation "List of booleans, one for each slot. When set to t,
                                        the corresponding slot is bound"))
  (:documentation "The primitive's evaluation spec (i.e. the primitive body)"))

(defmethod copy-object-content ((source evaluation-spec)
                                (target evaluation-spec))
  (setf (pattern target) (copy-object (pattern source)))
  (setf (bound-slot-names target) (copy-object (bound-slot-names source)))
  (setf (unbound-slot-names target) (copy-object (unbound-slot-names source)))
  (setf (evaluation-spec-function target) (copy-object (evaluation-spec-function source)))
  (setf (bound-slots-pattern target) (copy-object (bound-slots-pattern source))))


(defun check-evaluation-spec-syntax (evaluation-spec-defs slot-specs)
  (assert (listp evaluation-spec-defs) ()
          "The evaluation-spec-defs should be a list, got:~%  ~a"
          evaluation-spec-defs)
  (loop for evaluation-spec-def in evaluation-spec-defs
        for pattern = (progn 
                        (assert (listp evaluation-spec-def) ()
                                "A evaluation-spec-def should be a list, got:~%  ~a"
                                evaluation-spec-def)
                        (car evaluation-spec-def))
        do (assert (listp pattern) ()
             "The pattern of a evaluation-spec-def should be a ~ list, got:~% ~a"
             (car evaluation-spec-def))
        (assert (find '=> pattern) ()
          "Couldn't find the '=> in pattern~% ~:w" pattern)
        (loop for x in pattern
              do (assert (symbolp x) ()
                   "Expected a slot name or '=> ,~%got ~:w" x)
              (assert (or (eq x '=>)
                          (find x slot-specs :key #'car)) ()
                "Slot name ~a in pattern ~a~% should be one of ~a"
                x pattern (mapcar #'car slot-specs))
              (assert (= 1 (length (find-all x pattern))) ()
                "~a in pattern ~a occurs more than once" x pattern))
        do (loop for s in (mapcar #'car slot-specs)
                 do (assert (find s pattern) ()
                      "Could not find slot ~a in pattern ~a" s pattern))))


(defun check-bindings-syntax (bindings unbound-slot-names primitive pattern)
  (assert (listp bindings) ()
          "The bindings should be a list, got:~%  ~a"
          bindings)
  (loop for binding in bindings
        do (assert (and (listp binding) (or (= 3 (length binding))
                                            (= 4 (length binding)))) ()
                   "Primitive ~a. Pattern ~a.~
                   ~%A binding should be a list of three or four elements, as in ~
                   (slot-name 0.8 binding-spec) got:~%  ~a"
                   primitive pattern binding)
        do (assert (member (car binding) unbound-slot-names) ()
                   "Primitive ~a. Pattern ~a.~
                   ~%The first element of the binding spec~%  ~a~
                   ~%should be the name of an unbound slot, i.e. one of~%  ~a."
                   primitive pattern binding unbound-slot-names))
  (let ((names (mapcar #'car bindings)))
    (assert (is-set names) ()
            "Primitive ~a. Pattern ~a.~
             ~%There seem to be two or more bindings for the same slot in~%  ~a."
            primitive pattern bindings)
    (assert (equal-sets names unbound-slot-names) ()
            "Primitive ~a. Pattern ~a.
             ~%The set of new bindings in~%  ~a~
             ~%does not seem to match the set of unbound slots, i.e.~%  ~a"
            primitive pattern bindings
            unbound-slot-names)))

(defun expand-evaluation-spec (primitive evaluation-spec-def slot-spec-defs)
  (destructuring-bind (pattern &body body) evaluation-spec-def
    (let ((slot-names (mapcar #'first slot-spec-defs))
          (slot-types (mapcar #'second slot-spec-defs))
          (bound-slot-names nil)
          (unbound-slot-names nil))
      (loop with bound-slot? = t
            for x in pattern
            do (cond ((eq x '=>) (setf bound-slot? nil))
                     (bound-slot? (push x bound-slot-names))
                     (t (push x unbound-slot-names))))
      `(make-instance 'evaluation-spec
                      :pattern ',pattern
                      :bound-slot-names ',(reverse bound-slot-names)
                      :unbound-slot-names ',(reverse unbound-slot-names)
                      :bound-slots-pattern ',(loop for x in slot-spec-defs
                                                   collect (if (find (car x) bound-slot-names) t nil))
                      :function
                      ,(if (= 0 (length unbound-slot-names))
                         ;; if no unbound slots -> return what body returns
                         `(lambda (ontology binding-objects . ,slot-names)
                            (declare (ignorable ontology binding-objects . ,slot-names))
                            ,@body)
                         ;; if bound slots -> return weighted-value-set,
                         ;; which is nil if bind has not been called
                         (let ((weighted-value-sets (gensym)))
                           `(macrolet ((bind (&rest bindings)
                                         ;; push a list with an entry for each slot, holding
                                         ;; the value-specs for the unbound one, and nil for
                                         ;; the others on the value-sets list that is returned
                                         (check-bindings-syntax bindings ',unbound-slot-names ',primitive ',pattern)
                                         `(push
                                           (list . ,(loop for slot-name in ',slot-names
                                                          for binding = (assq slot-name bindings)
                                                          collect `(list ,(second binding) ,(third binding) ,(fourth binding))))
                                           ,',weighted-value-sets)))
                              ;; Here also all slot-names are given as formal parameters.
                              ;; The concrete parameters for bound slots are the bindings
                              ;; under consideration, whereas the concrete parameters for
                              ;; unbound slots are the vars linked to those slots. These
                              ;; could be used to inspect the new bindings resulting from
                              ;; previously considered binding combinations.
                              (lambda (ontology binding-objects . ,slot-names)
                                (declare (ignorable ontology binding-objects . ,slot-names))
                                (let ((,weighted-value-sets '()))
                                  ,@body
                                  ;; check the bindings
                                  (loop for value-set in ,weighted-value-sets
                                        do (loop for (score value) in value-set
                                                 for type in ',slot-types
                                                 for name in ',slot-names
                                                 when (not (or (numberp score) (null score)))
                                                 do (error "Primitive ~a. Pattern ~a.~
                                                            ~%The provided score ~a for slot ~a is not of ~
                                                            expected type number."
                                                           ',primitive ',pattern score name)
                                                 when (and (find name ',unbound-slot-names)
                                                           (not (typep value type)))
                                                 do (error "Primitive ~a. Pattern ~a.~
                                                            ~%The bound value ~a for slot ~a is not of ~
                                                            expected type ~a."
                                                           ',primitive ',pattern value name type)))
                                  ;; return the weighted-value-sets
                                  ,weighted-value-sets)))))))))


(defun expand-evaluation-specs (primitive evaluation-spec-defs slot-spec-defs)
  "When defining a primitive, expand the evaluation-spec-defs using
   the slot-spec-defs, such that they can be executed. Before doing so,
   check the syntax."
  (check-evaluation-spec-syntax evaluation-spec-defs slot-spec-defs)
  `(list . ,(loop for evaluation-spec-def in evaluation-spec-defs
                  collect (expand-evaluation-spec primitive
                                                  evaluation-spec-def
                                                  slot-spec-defs))))