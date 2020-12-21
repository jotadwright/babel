(in-package :cooking-bot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; This file contains utility functions for the cooking-bot environment.      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; copy-ontology-object ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric copy-ontology-object (object)
  (:documentation "Makes a deep copy of an object in the ontology."))

(defmethod copy-ontology-object ((object symbol))
  object)

(defmethod copy-ontology-object ((object number))
  object)

(defmethod copy-ontology-object ((object string))
  (copy-seq object))

(defmethod copy-ontology-object ((object standard-object))
  (loop with class = (class-of object)
        with copy = (make-instance class)
        for slot in (harlequin-common-lisp:class-slots class)
        for slot-name = (harlequin-common-lisp:slot-definition-name slot)
        do (setf (slot-value copy slot-name) (copy-ontology-object (slot-value object slot-name)))
        finally return copy))

(defmethod copy-ontology-object ((object list))
  (cond
   ((null object) nil)
   ((not (consp object)) (copy-ontology-object object))
   (t
    (cons (copy-ontology-object (first object))
          (copy-ontology-object (rest object))))))

(defmethod copy-ontology-object ((object hash-table))
  (let ((copy (make-hash-table
	       :test (hash-table-test object)
	       :size (hash-table-size object)
	       :rehash-size (hash-table-rehash-size object)
	       :rehash-threshold (hash-table-rehash-threshold object))))
    (maphash #'(lambda (key val)
		 (setf (gethash key copy) val))
	     object)
    copy))


;; equal-ontology-objects ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric equal-ontology-objects (object-1 object-2)
  (:documentation "Returns t if object-1 and object-2 are equal in ontological terms."))

(defmethod equal-ontology-objects ((object-1 symbol) (object-2 symbol))
  (eql object-1 object-2))

(defmethod equal-ontology-objects ((object-1 number) (object-2 number))
  (= object-1 object-2))

(defmethod equal-ontology-objects ((object-1 string) (object-2 string))
  (equalp object-1 object-2))

(defmethod equal-ontology-objects ((object-1 standard-object) (object-2 standard-object))
  (and (equal (class-of object-1) (class-of object-2))
       (loop for o1-slot in (harlequin-common-lisp:class-slots (class-of object-1))
             for o2-slot in (harlequin-common-lisp:class-slots (class-of object-2))
             always (and (eql (harlequin-common-lisp:slot-definition-name o1-slot)
                              (harlequin-common-lisp:slot-definition-name o2-slot))
                         (equal-ontology-objects (slot-value object-1 (harlequin-common-lisp:slot-definition-name o1-slot))
                                                 (slot-value object-2 (harlequin-common-lisp:slot-definition-name o2-slot)))))))

(defmethod equal-ontology-objects ((object-1 list) (object-2 list))
  (and (= (length object-1) (length object-2))
       (loop for el-1 in object-1
             for el-2 in object-2
             always (equal-ontology-objects el-1 el-2))))


