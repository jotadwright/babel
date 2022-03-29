;;;; equal.lisp

(in-package :mwm-evaluation)

;; -----------------
;; EQUAL primtive ;;
;; -----------------
;; Check if two categories are the same

;(export '(equal?))

(defgeneric equal-attribute-p (category-1 category-2 attribute-category)
  (:documentation "Check if cat-1 and cat-2 are equal."))

(defmethod equal-attribute-p :before ((category-1 concept-entity)
                                      (category-2 concept-entity)
                                      (attribute-category attribute-category))
  (assert (eql (type-of category-1) (type-of category-2)))
  (assert (case (attribute attribute-category)
            (shape (eql (type-of category-1) 'shape-concept))
            (size (eql (type-of category-1) 'size-concept))
            (color (eql (type-of category-1) 'color-concept))
            (material (eql (type-of category-1) 'material-concept)))))

(defmethod equal-attribute-p ((shape-1 shape-concept)
                              (shape-2 shape-concept)
                              (attribute attribute-category))
  (declare (ignorable attribute))
  (equal-entity shape-1 shape-2))

(defmethod equal-attribute-p ((size-1 size-concept)
                              (size-2 size-concept)
                              (attribute attribute-category))
  (declare (ignorable attribute))
   (equal-entity size-1 size-2))

(defmethod equal-attribute-p ((color-1 color-concept)
                              (color-2 color-concept)
                              (attribute attribute-category))
  (declare (ignorable attribute))
   (equal-entity color-1 color-2))

(defmethod equal-attribute-p ((material-1 material-concept)
                              (material-2 material-concept)
                              (attribute attribute-category))
  (declare (ignorable attribute))
   (equal-entity material-1 material-2))

(defprimitive equal? ((target-bool boolean-category)
                      (source-1 concept-entity)
                      (source-2 concept-entity)
                      (attribute attribute-category))
  ;; first case; given the sources and the attribute, compute the target
  ((source-1 source-2 attribute => target-bool)
   (let ((equal-p (equal-attribute-p source-1 source-2 attribute)))
     (bind (target-bool 1.0 (find-entity-by-id ontology (if equal-p 'yes 'no))))))

  #|
  ;; second case; given all, check for consistency
  ((source-1 source-2 attribute target-bool =>)
   (let* ((equal-p (equal-attribute-p source-1 source-2 attribute))
          (bool-category (find-entity-by-id ontology (if equal-p 'yes 'no))))
     (equal-entity target-bool bool-category)))
  |#
  :primitive-inventory *mwm-primitives*)