;;;; utils.lisp

(in-package :clevr-evaluation)

;;;; predicate->clevr-program-node
(defgeneric predicate->clevr-program-node (predicate bind-statement node-id input-ids type)
  (:documentation "Make a clevr program node from the given predicate"))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'get-context)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-function :id node-id
                 :function-name 'scene))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'filter)))
  (let* ((category (first (split (mkstr (bind-statement-type bind-statement)) #\-)))
         (filter-type (internal-symb (upcase (string-append "filter_" category)))))
    (make-instance 'clevr-function :id node-id
                   :function-name filter-type
                   :args (list (internal-symb (bind-statement-value bind-statement))))))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'unique)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-function :id node-id
                 :function-name 'unique))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'relate)))
  (make-instance 'clevr-function :id node-id
                 :function-name 'relate
                 :args (list (internal-symb (bind-statement-value bind-statement)))))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'union!)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-function :id node-id
                 :function-name 'union))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'intersect)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-function :id node-id
                 :function-name 'intersect))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'count!)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-function :id node-id
                 :function-name 'count))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'query)))
  (let ((query-type (internal-symb (upcase (string-append "query_" (mkstr (bind-statement-value bind-statement)))))))
    (make-instance 'clevr-function :id node-id
                   :function-name query-type)))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'exist)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-function :id node-id
                 :function-name 'exist))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'equal?)))
  (let ((equal-type (internal-symb (upcase (string-append "equal_" (mkstr (bind-statement-value bind-statement)))))))
    (make-instance 'clevr-function :id node-id
                   :function-name equal-type)))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'equal-integer)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-function :id node-id
                 :function-name 'equal_integer))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'less-than)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-function :id node-id
                 :function-name 'less_than))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'greater-than)))
  (declare (ignorable predicate bind-statement))
  (make-instance 'clevr-function :id node-id
                 :function-name 'greater_than))

(defmethod predicate->clevr-program-node (predicate bind-statement node-id input-ids (type (eql 'same)))
  (let ((same-type (internal-symb (upcase (string-append "same_" (mkstr (bind-statement-value bind-statement)))))))
    (make-instance 'clevr-function :id node-id
                   :function-name same-type)))