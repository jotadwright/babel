;;;; equal.lisp

(in-package :clevr-primitives)

;; -----------------
;; EQUAL primtive ;;
;; -----------------

;(export '(equal?))

(defgeneric equal-attribute-p (category-1 category-2 attribute-category)
  (:documentation "Check if cat-1 and cat-2 are equal."))

(defmethod equal-attribute-p :before ((category-1 category)
                                      (category-2 category)
                                      (attribute-category attribute-category))
  (assert (eql (type-of category-1) (type-of category-2)))
  (assert (case (attribute attribute-category)
            (shape (eql (type-of category-1) 'shape-category))
            (size (eql (type-of category-1) 'size-category))
            (color (eql (type-of category-1) 'color-category))
            (material (eql (type-of category-1) 'material-category)))))

(defmethod equal-attribute-p ((shape-1 shape-category)
                              (shape-2 shape-category)
                              (attribute attribute-category))
  (declare (ignorable attribute))
  (eq (shape shape-1) (shape shape-2)))

(defmethod equal-attribute-p ((size-1 size-category)
                              (size-2 size-category)
                              (attribute attribute-category))
  (declare (ignorable attribute))
  (eq (size size-1) (size size-2)))

(defmethod equal-attribute-p ((color-1 color-category)
                              (color-2 color-category)
                              (attribute attribute-category))
  (declare (ignorable attribute))
  (eq (color color-1) (color color-2)))

(defmethod equal-attribute-p ((material-1 material-category)
                              (material-2 material-category)
                              (attribute attribute-category))
  (declare (ignorable attribute))
  (eq (material material-1) (material material-2)))

(defprimitive equal? ((target-bool boolean-category)
                      (source-1 category)
                      (source-2 category)
                      (attribute attribute-category))
  ;; first case; given the sources and the attribute, compute the target
  ((source-1 source-2 attribute => target-bool)
   (let ((equal-p (equal-attribute-p source-1 source-2 attribute)))
     (bind (target-bool 1.0 (find-entity-by-id ontology (if equal-p 'yes 'no))))))

  ;; second case; given all, check for consistency
  ((source-1 source-2 attribute target-bool =>)
   (let* ((equal-p (equal-attribute-p source-1 source-2 attribute))
          (bool-category (find-entity-by-id ontology (if equal-p 'yes 'no))))
     (equal-entity target-bool bool-category)))
  :primitive-inventory *clevr-primitives*)