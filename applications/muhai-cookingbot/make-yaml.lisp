(ql:quickload :muhai-cookingbot)
(ql:quickload :alexandria)
(ql:quickload :closer-mop)
(ql:quickload :cl-yaml)
(in-package :muhai-cookingbot)

(defun get-superclasses (child)
   (loop for class in (closer-mop:class-direct-superclasses (find-class child))
      collect (class-name class)))

(defun get-subclasses (parent)
   (loop for class in (closer-mop:class-direct-subclasses (find-class parent))
      collect (class-name class)))

(defun traverse (root)
  (let ((children (get-subclasses root)))
    (if (null children)
        root
        (loop for c in children
              collect (traverse c)))))

(defun get-all-classes (class)
  (loop for c in (remove-duplicates (alexandria:flatten (traverse class)))
        collect (list c
                      `(:documentation . ,(documentation (find-class c) t))
                      `(:superclasses . ,(get-superclasses  c)))))



;; Script that generated yaml ontology from lisp
;; still misses some things like OWL key, dispositions, dataproperties, ...

(with-open-file (*standard-output* "ontology-from-lisp.yaml" :direction :output :if-exists :overwrite :if-does-not-exist :create)
  (loop for class in (get-all-classes 'kitchen-entity)
        do (format *standard-output* "  ~a: ~%" (string-downcase (car class)))
        do (format *standard-output* "    documentation: \"~a\"~%" (cdr (assoc :documentation (cdr class))))
        do (format *standard-output* "    superclasses:~%")
        do (loop for superclass in (cdr (assoc :superclasses (cdr class)))
                 do (format *standard-output* "      - ~a ~%" (string-downcase superclass)))))
