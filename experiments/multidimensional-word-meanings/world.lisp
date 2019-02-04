(in-package :mwm)

;;;; Generate objects
;;;; ----------------

;;;; The objects should have a number of features
;;;; Each of these features is represent by one or multiple feature channels:

;;;; shape: number of sides, width/height-ratio, number of corners, ...?
;;;; color: mean rgb
;;;; size: area
;;;; material: stdev rgb (?)

;;;; The objects have some additional feature channels:
;;;; x-pos, y-pos, width, height

;;;; When generating objects, we have to make sure that the object will
;;;; belong to particular categories, one for every feature.
;;;; Therefore, we define a number of categories on beforehand:

;;;; shape: square, triangle, circle, rectangle (?)
;;;; color: red, green, blue
;;;; size: small, medium, large
;;;; material: shiny, matte

(defclass mwm-object (entity)
  ((x-pos
    :documentation "the position on x-axis"
    :type number :initarg :x-pos :accessor x-pos)
   (y-pos
    :documentation "the position on y-axis"
    :type number :initarg :y-pos :accessor y-pos)
   (width
    :documentation "the width of the object"
    :type number :initarg :width :accessor width)
   (height
    :documentation "the height of the object"
    :type number :initarg :height :accessor height)
   (area
    :documentation "the area"
    :type number :initarg :area :accessor area)
   (wh-ratio
    :documentation "width-height ratio"
    :type number :initarg :wh-ratio :accessor wh-ratio)
   (mean-color
    :documentation "the mean rgb value"
    :type list :initarg :mean-color :accessor mean-color)
   (stdev-color
    :documentation "the stdev of the rgb"
    :type list :initarg :stdev-color :accessor stdev-color)
   (number-of-sides
    :documentation "the number of sides"
    :type number :initarg :nr-of-sides :accessor nr-of-sides)
   (number-of-corners
    :documentation "the number of corners"
    :type number :initarg :nr-of-corners :accessor nr-of-corners)
   (description
    :documentation "a description of the object"
    :type list :initarg :description :accessor description)))

(defmethod scale-object ((object mwm-object))
  (make-instance 'mwm-object
                 :x-pos (scale-channel :x-y-pos (x-pos object))
                 :y-pos (scale-channel :x-y-pos (y-pos object))
                 :width (scale-channel :width-height (width object))
                 :height (scale-channel :width-height (height object))
                 :area (scale-channel :area (area object))
                 :wh-ratio (wh-ratio object)
                 :mean-color (scale-channel :mean-color (mean-color object))
                 :stdev-color (scale-channel :stdev-color (stdev-color object))
                 :nr-of-sides (scale-channel :nr-of-sides (nr-of-sides object))
                 :nr-of-corners (scale-channel :nr-of-corners (nr-of-corners object))
                 :description (description object)))

(defun object-features->values (object-features)
  ;; nr-of-sides, nr-of-corners
  ;; mean-rgb, stdev-rgb
  ;; width, height, area, wh-ratio
  ;; x-pos, y-pos
  (let* ((x-pos (feature->value :x-pos (rest (assoc :x-position object-features))))
         (y-pos (feature->value :y-pos (rest (assoc :y-position object-features))))
         (width (feature->value :width-height (rest (assoc :size object-features))))
         (height (if (eql (rest (assoc :shape object-features)) 'rectangle)
                   (feature->value :width-height (rest (assoc :size object-features)))
                   width))
         (area (* width height))
         (wh-ratio (float (/ (min width height) (max width height))))
         (mean-rgb (feature->value :mean-rgb (rest (assoc :color object-features))))
         (stdev-rgb (feature->value :stdev-rgb (rest (assoc :material object-features))))
         (nr-of-sides (feature->value :nr-of-sides (rest (assoc :shape object-features))))
         (nr-of-corners (feature->value :nr-of-corners (rest (assoc :shape object-features))))
         raw-object)
    (setf raw-object
          (make-instance 'mwm-object :x-pos x-pos :y-pos y-pos
                         :width width :height height :area area :wh-ratio wh-ratio
                         :mean-color mean-rgb :stdev-color stdev-rgb
                         :nr-of-sides nr-of-sides :nr-of-corners nr-of-corners
                         :description object-features))
    (scale-object raw-object)))

(defclass mwm-object-set (entity)
  ((entities
    :documentation "the entities in the set"
    :type list :initarg :entities :accessor entities))
  (:documentation "a set of objects"))

(defun generate-context (n)
  (let ((object-features (sample-random-objects n)))
    (make-instance 'mwm-object-set
                   :entities (mapcar #'object-features->values object-features))))