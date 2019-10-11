(in-package :robot-concept-learning)

;; --------------
;; + MWM object +
;; --------------
(defclass mwm-object (entity)
  ((attributes
    :documentation "the attributes of the object (a-list)"
    :type list :accessor attributes :initarg :attributes)
   (description
    :documentation "symbolic description of the original clevr object"
    :type list :accessor description :initarg :description))
  (:documentation "A continuous-valued CLEVR object"))

(defmethod get-attr-val ((object mwm-object) attr)
  (rest (assoc attr (attributes object))))

(defmethod set-attr-val ((object mwm-object) attr val)
  (if (assoc attr (attributes object))
    (setf (rest (assoc attr (attributes object))) val)
    (push (cons attr val) (attributes object)))
  nil)

(defmethod get-object-position ((object mwm-object))
  (list (get-attr-val object :xpos)
        (get-attr-val object :ypos)))

;; ------------------
;; + MWM object set +
;; ------------------
(defclass mwm-object-set (entity)
  ((objects
    :documentation "the objects in the set"
    :type list :accessor objects :initarg :objects)
   (image
    :documentation "path of the image of this set"
    :type pathname :accessor image :initarg :image :initform nil))
  (:documentation "A set of mww-objects"))

;; -----------------
;; + Observe world +
;; -----------------

(defun observe-and-process-world (agent)
  "The robot observes and processes the world. It extracts data
   from an image and processes this into mwm-objects"
  (multiple-value-bind (observations analysis-image)
      (observe-world (robot agent) :open nil)
    (values (make-instance 'mwm-object-set
                           :objects (loop for observation in observations
                                          for object = (first observation)
                                          collect (make-instance 'mwm-object :id (first object)
                                                                 :attributes (rest object))))
            analysis-image)))
          
    