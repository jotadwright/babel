(in-package :irl)

;; ############################################################################
;; primitive definition:
;; ----------------------------------------------------------------------------

(export '(primitive-p slot-count defprimitive => *irl-primitives*))

(defparameter *irl-primitives* nil)

(defclass primitive ()
  ((id :documentation "The identifier"
       :type symbol :initarg :id :reader id
       :initform (error "primitive requires :id"))
   (slot-specs :type list :initarg :slot-specs :reader slot-specs
               :initform (error "primitive requires :slot-specs")
               :documentation "The list of <slot-spec> specified for the primitive.
                               Each of these specifies the details of one of the slots of
                               the primitive.")
   (evaluation-specs :type list :initarg :evaluation-specs :reader evaluation-specs
                     :initform (error ":evaluation-specs are required")
                     :documentation "The evaluation-spec instances associated with this type."))
   (:documentation "Represents the 'type' of a primitive, i.e. all the information that is passed to defprimitive"))


(defmethod print-object ((p primitive) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
	(format stream "(~~primitive~~~:_ id: ~(~a~))" (id p)))
      (call-next-method)))

(defmethod copy-object ((primitive primitive))
  (let ((copy (make-instance
               'primitive :id (id primitive)
               :slot-specs (copy-object (slot-specs primitive))
               :evaluation-specs (copy-object (evaluation-specs primitive)))))
    copy))

(defun primitive-p (obj)
  (typep obj 'primitive))


(defun slot-count (primitive)
  "Return the number of slots of the primitive."
  (declare (type primitive primitive))
  (length (slot-specs primitive)))

#|
(defmacro defprimitive (id slot-spec-defs evaluation-spec-defs
                           &key (primitive-inventory '*irl-primitives*))
  "Creates a parameter for the primitive, which is instantiated as
   concrete primitives in irl programs."
  `(let ((p (make-instance 'primitive :id ',id
                           :slot-specs (make-slot-specs ,slot-spec-defs)
                           :evaluation-specs ,(expand-evaluation-specs id
                                                                       evaluation-spec-defs
                                                                       slot-spec-defs))))
     (add-primitive p ,primitive-inventory)))
|#

(defmacro defprimitive (id slot-spec-defs &body body)
  (let* ((inventory (if (find :primitive-inventory body)
                      (nth (1+ (position :primitive-inventory body)) body)
                      '*irl-primitives*))
         (evaluation-spec-defs
          (if (find :primitive-inventory body)
            (subseq body 0 (- (length body) 2))
            body)))
    `(let ((p (make-instance 'primitive :id ',id
                             :slot-specs (make-slot-specs ,slot-spec-defs)
                             :evaluation-specs ,(expand-evaluation-specs id
                                                                         evaluation-spec-defs
                                                                         slot-spec-defs))))
       (add-primitive p ,inventory))))

