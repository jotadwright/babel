(in-package :mwm)

;; --------------
;; + MWM object +
;; --------------
(defclass mwm-object (entity)
  ((x-pos
    :documentation "position on x-axis"
    :type number :accessor x-pos :initarg :x-pos)
   (y-pos
    :documentation "position on y-axis"
    :type number :accessor y-pos :initarg :y-pos)
   (area
    :documentation "area of the object"
    :type number :accessor area :initarg :area)
   (width
    :documentation "width of the object"
    :type number :accessor width :initarg :width)
   (height
    :documentation "height of the object"
    :type number :accessor height :initarg :height)
   (wh-ratio
    :documentation "width/height ratio"
    :type number :accessor wh-ratio :initarg :wh-ratio)
   (mean-rgb
    :documentation "mean rgb value"
    :type list :accessor mean-rgb :initarg :mean-rgb)
   (rgb-variance
    :documentation "rgb variance"
    :type list :accessor rgb-variance :initarg :rgb-variance)
   (nr-of-sides
    :documentation "number of sides"
    :type number :accessor nr-of-sides :initarg :nr-of-sides)
   (nr-of-corners
    :documentation "number of corners"
    :type number :accessor nr-of-corners :initarg :nr-of-corners)
   (description
    :documentation "description of the original clevr object"
    :type list :accessor description :initarg :description))
  (:documentation "A continuous valued object"))

(defmethod get-attr-val ((object mwm-object) attr)
  (funcall (symbol-function attr) object))

;; ------------------
;; + MWM object set +
;; ------------------
(defclass mwm-object-set (entity)
  ((objects
    :documentation "the objects in the set"
    :type list :accessor objects :initarg :objects))
  (:documentation "A set of mww-objects"))

;; ----------------
;; + CLEVR -> MWM +
;; ----------------

;;;; Converting discrete clevr objects to continuous mwm objects
;;;; For now, these are always exactly the same value
;;;; No noise is present, yet!

;;;; The world is 500x300
;;;; We take large objects to have an area of 200 and small ones 50
;;;; The wh-ratio of a cylinder should be 0.5, while that of cubes and spheres should be 1
;;;; Therefore, we put the width and height of large cylinders to 10x20 and small cylinders 5x10
;;;; For large spheres and cubes, we use 14x14. For small ones 7x7.

;;;; For the moment, sensory-scaling is being used. This takes into account the min/max boundaries
;;;; of the world. Another option would be to use context-scaling (for some attributes), taking
;;;; only into account the current context.

(defun scale-value (value min max)
  (float
   (min
    (max (/ (- value min)
            (- max min))
         0.0)
    1.0)))

(defun add-noise (value min-noise max-noise &key min-bound max-bound)
  (let* ((func (random-elt (list #'+ #'-)))
         (noise (random-from-range min-noise max-noise))
         (new-value (funcall func value noise)))
    (when min-bound
      (if (< value min-bound)
        (setf new-value min-bound)))
    (when max-bound
      (if (> value max-bound)
        (setf new-value max-bound)))
    new-value))

(defun object->width-and-height (object)
  (let* ((w (if (eql (shape object) 'cylinder)
              (if (eql (size object) 'large)
                (add-noise 10.0 0.0 2.0)
                (add-noise 5.0 0.0 2.0))
              (if (eql (size object) 'large)
                (add-noise 14.0 0.0 2.0)
                (add-noise 7.0 0.0 2.0))))
         (h (if (eql (shape object) 'cylinder)
              (if (eql (size object) 'large)
                (add-noise 20.0 0.0 2.0)
                (add-noise 10.0 0.0 2.0))
              w)))
    (values w h)))

(defun object->sides-and-corners (object)
  "Return the number of sides (= vlakken)
   and the number of corners"
  (case (shape object)
    (cube (values 6 8))
    (sphere (values 1 0))
    (cylinder (values 3 0))))

(defun object->color (object)
  "Return the color of the object"
  (let ((color (case (color object)
                 (gray '(87 87 87))
                 (red '(173 35 35))
                 (blue '(42 75 215))
                 (green '(29 105 20))
                 (brown '(129 74 25))
                 (purple '(129 38 192))
                 (cyan '(41 208 208))
                 (yellow '(255 238 51)))))
    (mapcar #'(lambda (c)
                (add-noise c 0.0 10.0 :min-bound 0.0 :max-bound 255.0))
            color)))

(defun object->color-variance (object)
  "Return the variance of the object"
  (case (material object)
    (metal (loop repeat 3
                 collect (add-noise 1.0 0.0 0.2)))
    (rubber (loop repeat 3
                  collect (add-noise 0.0 0.0 0.2 :min-bound 0.0)))))

(defmethod clevr->mwm ((set clevr-object-set))
  (make-instance 'mwm-object-set
                 :id (id set)
                 :objects (loop for obj in (objects set)
                                collect (clevr->mwm obj))))

(defmethod clevr->mwm ((object clevr-object))
  "Create the object and scale at the same time."
  (multiple-value-bind (width height) (object->width-and-height object)
    (multiple-value-bind (sides corners) (object->sides-and-corners object)
      (make-instance 'mwm-object
                     :id (id object) ;; !!!
                     :x-pos (first (coordinates object)) ;(/ (first (coordinates object)) 500.0)
                     :y-pos (second (coordinates object)) ;(/ (second (coordinates object)) 300.0)
                     :width width ;(scale-value width 5 14) ;(/ (- width 5.0) (- 14.0 5.0))
                     :height height; (scale-value height 7 20) ;(/ (- height 7.0) (- 20.0 7.0))
                     :area (* width height) ;(scale-value (* width height) 50 200) ;(/ (- (* width height) 50.0) (- 200.0 50.0))
                     :wh-ratio (/ width height) ;(float (/ width height))
                     :mean-rgb (object->color object) ;(mapcar #'(lambda (c) (/ c 255.0)) (object->color object))
                     :rgb-variance (object->color-variance object)
                     :nr-of-sides sides ;(scale-value sides 1 6) ;(/ (- sides 1.0) (- 6.0 1.0))
                     :nr-of-corners corners ;(/ corners 8.0)
                     :description `(,(shape object)
                                    ,(color object)
                                    ,(size object)
                                    ,(material object))))))
   
(defmethod object->alist ((object clevr-object))
  (let ((x-pos (first (coordinates object)))
        (y-pos (second (coordinates object))))
    (list (cons :color (color object))
          (cons :size (size object))
          (cons :shape (shape object))
          (cons :material (material object))
          (cons :x-pos (if (> x-pos 240) 'right 'left))
          (cons :y-pos (if (> y-pos 160) 'front 'behind)))))