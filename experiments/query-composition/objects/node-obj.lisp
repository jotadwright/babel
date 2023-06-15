(in-package :qc)

(defclass node (tree-node)
  ((depth :type integer
          :initarg :depth
          :initform nil
          :accessor depth
          :documentation "Depth of the node.")
   (tble :type table
         :initarg :tble
         :initform nil
         :accessor tble
         :documentation "Table referred in SELECT form.")
   (attributes-selected :type list
                        :initarg :attributes-selected
                        :initform '()
                        :accessor attributes-selected
                        :documentation "Attribute used for Selection part of the query")
   (ref-tbles :type list
              :initarg :ref-tbles
              :initform '()
              :accessor ref-tbles 
              :documentation "Set of tables referred to in the query.")
   (selection :type list
              :initarg :selection
              :initform '()
              :accessor selection
              :documentation "Selection part of the query used for children.")
   (conditions :type list
               :initarg :conditions
               :initform '()
               :accessor conditions
               :documentation "Condition part of the request used for children.")
   (attrs :type list
          :initarg :attrs
          :initform '()
          :accessor attrs
          :documentation "attributes reffered in condition form.")
   (time-result :type float
                :initarg :time-result
                :initform nil
                :accessor time-result
                :documentation "Execution time of the query.")
   (group-by-clause :type list
                    :initarg :group-by-clause
                    :initform nil
                    :accessor group-by-clause
                    :documentation "Goup-by list correspond to clause, list is not null if a function are present in the selection part")
   (q :type list
      :initarg :q
      :initform nil
      :accessor q
      :documentation "Query generated"))
  (:documentation "Object representing a node of the tree. This object is represented by a set of attributes that allows it to obtain and store all the information necessary for its SQL query."))

;; #########################################
;; init-node
;; --------------------------------------------------------------------

(defun init-node (node attributes table &key join)
  "function that create a node with the SELECT .. FROM .. clause and return the newly created node with its associated parent."
    (let* ((q '(:select))
            (table-name (intern (string-upcase (name table))))
            (att nil)
            (group-by nil)
            (function? nil))
      (dolist (att-n attributes)
        (if (equal (length att-n) 2)
          (setf function? t)))
      (dolist (att-n attributes)            
          (progn
            (if (typep (first att-n) 'attribute)
                (setf att (intern (string-upcase (concatenate 'string (symbol-name table-name) "." (name (first att-n))))))
              (setf att (intern (string-upcase (concatenate 'string (symbol-name table-name) "." (first att-n)))))))
          (progn
            (if (typep (first att-n) 'attribute)
              (setf att (intern (string-upcase (name (first att-n)))))
              (setf att (intern (string-upcase (first att-n)))))))
        (if (equal (length att-n) 2)
          ;; function
          (progn
            (setf q (pushend (list (second att-n) att) q))
            (setf function? t))
          ;; not a function
          (progn
            (if function?
              (progn
                (if group-by
                  (progn
                    (setf group-by (pushend att group-by))
                    (setf q (pushend att q)))
                  (setf group-by (list :group-by att)))
                (setf q (pushend att q)))
              
              (setf q (pushend att q))))))
      (setf q (append q (list :from table-name)))
      (if (not function?)
        (setf group-by nil))
      (make-instance 'node
                     :id (make-id)
                     :parent node
                     :depth (+ (depth node) 1)
                     :q q
                     :selection q
                     :attributes-selected attributes
                     :tble table
                     :group-by-clause group-by
                     :ref-tbles (list table))))

;; #########################################
;; join-node
;; --------------------------------------------------------------------

(defun join-node (node ref-info table-obj &key foreign-ref)
  "function that create a node with the ... INNER JOIN || OUTER JOIN ... ON ... =  ... clause and return the newly created node."
  (let* ((join :inner-join))
    (let* ((foreign-t (intern (string-upcase (concatenate 'string (foreign-table ref-info) "." (foreign-column ref-info)))))
            (main-t (intern (string-upcase (concatenate 'string (table-name ref-info) "." (column-name ref-info)))))
           (join-query (list join (intern (string-upcase (foreign-table ref-info))) :on (list := foreign-t main-t))))
  (make-instance 'node
                 :id (make-id)
                 :parent node
                 :depth (depth node)
                 :tble (tble node)
                 :ref-tbles (push-end table-obj (ref-tbles node))
                 :q (append (q node) join-query)
                 :selection (append (q node) join-query)))))

;; #########################################
;; where-node
;; --------------------------------------------------------------------

(defun where-node (node ref-table attribute operator value)
  "function that creates a node with the WHERE clause and returns the newly created node with its associated parent."
  (let* ((val-att (change-type value))
           (condition nil)
           (q nil))
    (if (equal (length (ref-tbles node)) 1)
      (setf condition (list operator (intern (string-upcase (name attribute))) val-att))
      (setf condition (list operator (intern (string-upcase (concatenate 'string (name ref-table) "." (name attribute)))) val-att)))
    (if (group-by-clause node)
      (setf q (append (append (q node) (list :where condition)) (group-by-clause node)))
      (setf q (append (q node) (list :where condition))))
    (make-instance 'node
                               :id (make-id)
                               :parent node
                               :depth (+ (depth node) 1)
                               :q q
                               :attrs (list attribute)
                               :tble (tble node)
                               :ref-tbles (ref-tbles node)
                               :selection (selection node)
                               :conditions condition)))

;; #########################################
;; condition-node
;; --------------------------------------------------------------------

(defmethod condition-node ((composer query-composer) parent ref-table attribute operator value condition-operator)
  (let ((val (change-type value))
         (condition nil))
    (if (equal (length (ref-tbles parent)) 1)
      (setf condition (list operator (intern (string-upcase (name attribute))) val))
      (setf condition (list  operator (intern (string-upcase (concatenate 'string (name ref-table) "." (name attribute)))) val)))
    (setf condition (list condition-operator (conditions parent) condition))
    ;;instanciate node object
    (if (group-by-clause node)
      (setf q (append (append (q node) (list :where condition)) (group-by-clause node)))
      (setf q (append (q node) (list :where condition))))
    (make-instance 'node
                   :id (make-id)
                   :parent parent
                   :depth (+ (depth parent) 1)
                   :q q
                   :attrs (push-end attribute (attrs parent))
                   :tble (tble parent)
                   :selection (selection parent)
                   :conditions condition)))
              