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

(defun flatten-and-convert-colors (attributes)
  (let ((color (rest (assoc :color attributes))))
    (push (cons :mean-h (* (/ (first color) 179) 360)) attributes)
    (push (cons :mean-s (* (/ (second color) 255) 100)) attributes)
    (push (cons :mean-v (* (/ (third color) 255) 100)) attributes)
    ;; here we manually remove a number of features
    ;; THIS IS HARD CODED
    (setf attributes (remove :color attributes :key #'car))
    (setf attributes (remove :p-whites attributes :key #'car))
    (setf attributes (remove :p-blacks attributes :key #'car))
    (setf attributes (remove :angle attributes :key #'car))))

;; opencv uses a different HSV color range:
;; H: [0-179]
;; S: [0-255]
;; V: [0-255]
;; A common color range for HSV is:
;; H: [0-360]
;; S: [0-100]
;; V: [0-100]

(defun observe-and-process-world (agent &key num-objects-to-detect)
  "The robot observes and processes the world. It extracts data
   from an image and processes this into mwm-objects.
   IMPORTANT: the features returned by the robot interface can
   be multi-dimensional (e.g. color is 3d). This is flattened
   here."
  (multiple-value-bind (observations analysis-image)
      (observe-world (robot agent) :open nil)
    (if num-objects-to-detect
      (if (= (length observations) num-objects-to-detect)
        (values (make-instance 'mwm-object-set
                               :objects (loop for observation in observations
                                              for object = (first observation)
                                              for flat-attributes = (flatten-and-convert-colors (rest object))
                                              collect (make-instance 'mwm-object :id (first object)
                                                                     :attributes flat-attributes)))
                analysis-image)
        (progn (notify detection-error agent (length observations) num-objects-to-detect)
          (capi:popup-confirmer nil "The robot could not detect the right amount of objects.
Please re-organise the scene. Click 'OK' to try again.")
          (observe-and-process-world agent :num-objects-to-detect num-objects-to-detect)))
      (values (make-instance 'mwm-object-set
                             :objects (loop for observation in observations
                                            for object = (first observation)
                                            for flat-attributes = (flatten-and-convert-colors (rest object))
                                            collect (make-instance 'mwm-object :id (first object)
                                                                   :attributes flat-attributes)))
              analysis-image))))
      
    