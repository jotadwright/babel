(in-package :irl)

;; ############################################################################
;; binding definition:
;; ----------------------------------------------------------------------------

(export '(binding var score value available-at))

(defclass binding ()
  ((variable :accessor var :initarg :var)
   (score :accessor score :initarg :score :initform nil)
   (value :accessor value :initarg :value :initform nil)
   (available-at :accessor available-at :initarg :available-at :initform nil)))


(defmethod print-object ((binding binding) stream)
  (format stream "<binding: ~a ~a (~a)~a>"
          (var binding)
          (score binding)
          (value binding)
          (if (available-at binding)
            (format nil " [~a]" (available-at binding))
            "")))


(defgeneric evaluate-bind-statement (class var value available-at ontology &key))

(defmethod evaluate-bind-statement (class var (value entity) available-at ontology
                                          &key &allow-other-keys)
  "Evaluates a bind statement by searching for values in the
   ontology or by binding the value in the bind statement."
  (declare (ignorable class ontology))
  (assert (typep value class))
  (make-instance 'binding :var var
                 :score 1.0 :value value
                 :available-at available-at))

(defmethod evaluate-bind-statement (class var value available-at
                                    ontology &key (assert-exists t))
  "Evaluates a bind statement by searching for values in the
   ontology or by binding the value in the bind statement."
  (declare (ignore class))
  (let ((entity (find-entity-by-id ontology value class)))
    (cond (entity (progn (assert (typep entity class))
                    (make-instance 'binding :var var
                                   :score 1.0 :value entity
                                   :available-at available-at)))
          (assert-exists (error "Could not find ~a in ontology." 
                                value))
          (t nil))))


(defun evaluate-bind-statements (bind-statements ontology)
  "Evaluates a set of bind statements by searching for values in the
   ontology or by binding the value in the bind statement."
  (loop for bind-statement in bind-statements
        collect (evaluate-bind-statement (second bind-statement)
                                         (third bind-statement)
                                         (fourth bind-statement)
                                         (fifth bind-statement)
                                         ontology)))


;; #############################################
;; Simple helpers for external users
;; #############################################

(export '(make-bind-statement make-binding
          make-bindings merge-bindings
          get-value))

(defgeneric make-bind-statement (x &key))

(defmethod make-bind-statement ((id symbol) &key var use-value ontology)
  (assert (or (not use-value) ontology))
  (let ((entity (find-entity-by-id ontology id)))    
    `(bind ,(or (type-of entity) 'entity)
           ,(if var var (make-id '?var-))
           ,(if use-value entity id))))

(defmethod make-bind-statement ((entity entity) &key var use-value)
  `(bind ,(type-of entity) ,(if var var (make-id '?var-))
         ,(if use-value entity (id entity))))


(defun make-binding (b &key ontology)
  "takes a bind-statement or lists of the form (var score value),
   or of the form (var . value) and turns it into a binding"
  (assert (listp b))
  (cond
   ((eq (first b) 'bind)
    (evaluate-bind-statement (second b) (third b) (fourth b) (fifth b) ontology))
   ((and (variable-p (first b)) (length= 3 b))
    (make-instance 'binding :var (first b)
                   :score (second b) :value (third b)))
   ((and (variable-p (first b)) (length= 2 b))
    (make-instance 'binding :var (first b)
                   :score 1.0 :value (second b)))
   (t (error "could not turn ~a into binding(s)" b))))


(defun make-bindings (bindings &key ontology)
  "takes a list of bind-statement or lists of the form (var score value),
   or of the form (var . value) and turns into into a binding"
  (loop for b in bindings
        collect (make-binding b :ontology ontology)))


(defun merge-bindings (b1 b2 &key (test #'equal-entity))
  "merges two bindings making sure that values are consistent.
   returns success/failure and the merged bindings"
  (loop with bindings = (copy-list b1)
        for b in b2
        for b-same-var = (find (var b) bindings :key #'var)
        when (and b-same-var
                  (not (funcall test (value b)
                                (value b-same-var))))
        return (values nil nil)
        when (not b-same-var)
        do (push b bindings)
        finally (return (values t bindings))))


(defun get-value (var bindings)
  "returns value and score for the variable"
  (assert (variable-p var))
  (let ((b (find var bindings :key #'var)))
    (when b (values (value b) (score b)))))