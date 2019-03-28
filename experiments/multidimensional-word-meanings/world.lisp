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
   (wh-ratio
    :documentation "width/height ratio"
    :type number :accessor wh-ratio :initarg :wh-ratio)
   (R
    :documentation "The lightness value"
    :type number :accessor R :initarg :R)
   (G
    :documentation "The red-green value"
    :type number :accessor G :initarg :G)
   (B
    :documentation "The yellow-blue value"
    :type number :accessor B :initarg :B)
   (roughness
    :documentation "The roughness of the texture"
    :type number :accessor roughness :initarg :roughness)
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

(defun add-random-value-from-range (value min-var max-var &key min-bound max-bound)
  (let* ((func (random-elt (list #'+ #'-)))
         (variance (random-from-range min-var max-var))
         (new-value (funcall func value variance)))
    (when min-bound
      (if (< new-value min-bound)
        (setf new-value min-bound)))
    (when max-bound
      (if (> new-value max-bound)
        (setf new-value max-bound)))
    new-value))

(defun object->x-pos (object noise)
  (let* ((x-pos (first (coordinates object)))
         (scaled-x-pos (float (/ (- x-pos 31) (- 460 31)))))
    (if noise
      (if (< scaled-x-pos 0.5)
        (max (min (random-from-range 0.0 (+ 0.5 noise)) 1.0) 0.0)
        (max (min (random-from-range (- 0.5 noise) 1.0) 1.0) 0.0))
      scaled-x-pos)))

(defun object->y-pos (object noise)
  (let* ((y-pos (second (coordinates object)))
         (scaled-y-pos (float (/ (- y-pos 58) (- 296 58)))))
    (if noise
      (if (< scaled-y-pos 0.5)
        (max (min (random-from-range 0.0 (+ 0.5 noise)) 1.0) 0.0)
        (max (min (random-from-range (- 0.5 noise) 1.0) 1.0) 0.0))
      scaled-y-pos)))

(defun object->area (object noise)
  (let* ((area (case (size object)
                 (small 30) (large 70)))
         (area-w-variance (add-random-value-from-range area 0 16))
         (scaled-area (float (/ area-w-variance 100))))
    (if noise
      (add-random-value-from-range scaled-area 0.0 noise :min-bound 0.0 :max-bound 1.0)
      scaled-area)))

(defun object->sides-and-corners (object noise)
  "Return the number of sides (= vlakken)
   and the number of corners"
  (let ((sides (case (shape object)
                 (cube 6) (sphere 1) (cylinder 3)))
        (corners (case (shape object)
                   (cube 8) (sphere 0) (cylinder 0))))
    (values (/ (- sides 1) (- 6 1))
            (/ corners 8))))

(defun object->color (object noise)
  "Return the color of the object"
  (let* ((rgb-color (case (color object)
                      (gray   '(87  87  87))
                      (red    '(173 34  35))
                      (blue   '(44  76  215))
                      (green  '(29  105 20))
                      (brown  '(126 72  25))
                      (purple '(130 39  192))
                      (cyan   '(40  208 208))
                      (yellow '(255 238 51))))
         (rgb-with-variance
          (mapcar #'(lambda (c)
                      (add-random-value-from-range c 0.0 2.0 :min-bound 0.0 :max-bound 255.0))
                  rgb-color))
         (scaled-rgb
          (mapcar #'(lambda (c)
                      (/ c 255.0))
                  rgb-with-variance)))
    (if noise
      (mapcar #'(lambda (c)
                  (add-random-value-from-range c 0.0 noise :min-bound 0.0 :max-bound 1.0))
              scaled-rgb)
      scaled-rgb)))

(defun object->roughness (object noise)
  "Return the variance of the object"
  (let* ((roughness (case (material object)
                      (metal 8)
                      (rubber 2)))
         (with-variance (add-random-value-from-range roughness 0.0 2.5 :min-bound 0.0 :max-bound 10.0))
         (scaled (/ with-variance 10.0)))
    (if noise
      (add-random-value-from-range scaled 0.0 noise :min-bound 0.0 :max-bound 1.0)
      scaled)))

(defun object->wh-ratio (object noise)
  (let* ((ratio (case (shape object)
                  (cube 1.0)
                  (sphere 1.0)
                  (cylinder 0.5)))
         (with-variance (add-random-value-from-range ratio 0.0 0.25 :min-bound 0.0 :max-bound 1.0)))
    (if noise
      (add-random-value-from-range with-variance 0.0 noise :min-bound 0.0 :max-bound 1.0)
      with-variance)))

(defmethod clevr->mwm ((set clevr-object-set) &key (noise nil))
  (make-instance 'mwm-object-set
                 :id (id set)
                 :objects (loop for obj in (objects set)
                                collect (clevr->mwm obj :noise noise))))

(defmethod clevr->mwm ((object clevr-object) &key (noise nil))
  "Create the object"
  (multiple-value-bind (sides corners) (object->sides-and-corners object noise)
    (let ((rgb-color (object->color object noise)))
      (make-instance 'mwm-object
                     :id (id object) ;; !!!
                     :x-pos (object->x-pos object noise)
                     :y-pos (object->y-pos object noise)
                     :area (object->area object noise)
                     :wh-ratio (object->wh-ratio object noise)
                     :R (first rgb-color)
                     :G (second rgb-color)
                     :B (third rgb-color)
                     :roughness (object->roughness object noise)
                     :nr-of-sides sides
                     :nr-of-corners corners
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
          (cons :x-pos (if (> x-pos 245.5) 'right 'left))
          (cons :y-pos (if (> y-pos 177) 'front 'behind)))))

