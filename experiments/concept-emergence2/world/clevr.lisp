(in-package :cle)

;; ------------------
;; + CLE object set +
;; ------------------
(defclass cle-context (entity)
  ((objects
    :documentation "The objects in the set."
    :type list :accessor objects :initarg :objects
    :initform nil)
   (image
    :documentation "Path of the image of this set."
    :type pathname :accessor image :initarg :image))
  (:documentation "A set of cle-objects."))

;; constructor
(defmethod clevr->simulated ((scene clevr-scene) feature-channels &key)
  "Transforms a symbolic clevr scene to a list of simulated cle-objects."
  (make-instance 'cle-context :id (id scene)
                 :image (image scene)
                 :objects (loop for obj in (objects scene)
                                collect (clevr->simulated obj feature-channels))))

;; --------------
;; + CLE-Object +
;; --------------
(defclass cle-object (entity)
  ((attributes
    :documentation "The attributes of the object (a-list)."
    :type list :accessor attributes :initarg :attributes)
   (description
    :documentation "Symbolic description of the original clevr object."
    :type list :accessor description :initarg :description))
  (:documentation "A continuous-valued CLEVR object."))

;; constructor
(defmethod clevr->simulated ((object clevr-object) feature-channels &key)
  (make-instance 'cle-object :id (id object)
                 :attributes (get-mapped-attributes object feature-channels)
                 :description (object->alist object)))

;; helper functions
(defmethod get-attr-val ((object cle-object) attr)
  (rest (assoc attr (attributes object))))

(defmethod set-attr-val ((object cle-object) attr val)
  (if (assoc attr (attributes object))
    (setf (rest (assoc attr (attributes object))) val)
    (push (cons attr val) (attributes object)))
  nil)

(defmethod object->alist ((object clevr-object))
  `((:color . ,(color object))
    (:size . ,(clevr-world::size object))
    (:shape . ,(shape object))
    (:material . ,(material object))
    (:xpos . ,(if (> (x-pos object) 240) 'right 'left))
    (:zpos . ,(if (> (z-pos object) 11) 'behind 'front))))
