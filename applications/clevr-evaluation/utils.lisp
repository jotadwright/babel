;;;; utils.lisp

(in-package :clevr-evaluation)

;;;; predicate->clevr-program-node
(defgeneric predicate->clevr-program-node (predicate bind-statement node-id input-ids type)
  (:documentation "Make a clevr program node from the given predicate"))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'get-context)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-program-node
                 :id node-id
                 :function 'scene
                 :inputs input-ids))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'filter)))
  (let* ((category (first (split (mkstr (bind-statement-type bind-statement)) #\-)))
         (filter-type (internal-symb (upcase (string-append "filter_" category)))))
    (make-instance 'clevr-program-node
                   :id node-id
                   :function filter-type
                   :inputs input-ids
                   :value-inputs (list (internal-symb (bind-statement-value bind-statement))))))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'unique)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-program-node
                 :id node-id
                 :function 'unique
                 :inputs input-ids))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'relate)))
  (make-instance 'clevr-program-node
                 :id node-id
                 :function 'relate
                 :inputs input-ids
                 :value-inputs (list (internal-symb (bind-statement-value bind-statement)))))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'union!)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-program-node
                 :id node-id
                 :function 'union
                 :inputs input-ids))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'intersect)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-program-node
                 :id node-id
                 :function 'intersect
                 :inputs input-ids))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'count!)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-program-node
                 :id node-id
                 :function 'count
                 :inputs input-ids))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'query)))
  (let ((query-type (internal-symb (upcase (string-append "query_" (mkstr (bind-statement-value bind-statement)))))))
    (make-instance 'clevr-program-node
                   :id node-id
                   :function query-type
                   :inputs input-ids)))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'exist)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-program-node
                 :id node-id
                 :function 'exist
                 :inputs input-ids))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'equal?)))
  (let ((equal-type (internal-symb (upcase (string-append "equal_" (mkstr (bind-statement-value bind-statement)))))))
    (make-instance 'clevr-program-node
                   :id node-id
                   :function equal-type
                   :inputs input-ids)))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'equal-integer)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-program-node
                 :id node-id
                 :function 'equal_integer
                 :inputs input-ids))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'less-than)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-program-node
                 :id node-id
                 :function 'less_than
                 :inputs input-ids))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'greater-than)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-program-node
                 :id node-id
                 :function 'greater_than
                 :inputs input-ids))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'same)))
  (let ((same-type (internal-symb (upcase (string-append "same_" (mkstr (bind-statement-value bind-statement)))))))
    (make-instance 'clevr-program-node
                   :id node-id
                   :function same-type
                   :inputs input-ids)))