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

(defun add-random-value-from-range (value min-var max-var
                                    &key (min-bound 0.0) (max-bound 1.0))
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

;; when x-pos and y-pos are exactly equal to 0.5
;; they are considered to be left!
(defmethod to-value ((object clevr-object) (attr (eql 'x-pos)) scale-p)
  (let* ((x-pos (first (coordinates object)))
         (scaled-x-pos (float (/ (- x-pos 31) (- 460 31)))))
    (if scale-p
      scaled-x-pos
      x-pos)))

(defmethod to-value ((object clevr-object) (attr (eql 'y-pos)) scale-p)
  (let* ((y-pos (second (coordinates object)))
         (scaled-y-pos (float (/ (- y-pos 58) (- 296 58)))))
    (if scale-p
      scaled-y-pos
      y-pos)))

(defmethod to-value ((object clevr-object) (attr (eql 'area)) scale-p)
  (let* ((area (case (size object)
                 (small 30) (large 70)))
         (area-w-variance (add-random-value-from-range area 0 16 :max-bound 100))
         (scaled-area (float (/ area-w-variance 100))))
    (if scale-p
      scaled-area
      area-w-variance)))

(defmethod to-value ((object clevr-object) (attr (eql 'sides-and-corners)) scale-p)
  (let ((sides (case (shape object)
                 (cube 6) (sphere 1) (cylinder 3)))
        (corners (case (shape object)
                   (cube 8) (sphere 0) (cylinder 0))))
    (if scale-p
      (values (/ (- sides 1) (- 6 1))
              (/ corners 8))
      (values sides corners))))

(defmethod to-value ((object clevr-object) (attr (eql 'color)) scale-p)
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
                      (add-random-value-from-range c 0.0 2.0 :max-bound 255.0))
                  rgb-color))
         (scaled-rgb
          (mapcar #'(lambda (c)
                      (/ c 255.0))
                  rgb-with-variance)))
    (if scale-p
      scaled-rgb
      rgb-with-variance)))

(defmethod to-value ((object clevr-object) (attr (eql 'roughness)) scale-p)
  (let* ((roughness
          (case (material object)
            (metal 8)
            (rubber 2)))
         (roughness-with-variance
          (add-random-value-from-range roughness 0.0 2.5 :max-bound 10.0))
         (scaled-roughness (/ roughness-with-variance 10.0)))
    (if scale-p
      scaled-roughness
      roughness-with-variance)))

(defmethod to-value ((object clevr-object) (attr (eql 'wh-ratio)) scale-p)
  (let* ((ratio (case (shape object)
                  (cube 1.0)
                  (sphere 1.0)
                  (cylinder 0.5)))
         (ratio-with-variance (add-random-value-from-range ratio 0.0 0.25)))
    (if scale-p
      (min ratio-with-variance 1.0)
      ratio-with-variance)))

(defmethod clevr->mwm ((set clevr-object-set)
                       &key (scale nil))
  (make-instance 'mwm-object-set :id (id set)
                 :objects (loop for obj in (objects set)
                                collect (clevr->mwm obj :scale scale))))

(defmethod clevr->mwm ((object clevr-object)
                       &key (scale nil))
  (multiple-value-bind (sides corners) (to-value object 'sides-and-corners scale)
    (let ((rgb-color (to-value object 'color scale))
          (roughness (to-value object 'roughness scale))
          (wh-ratio (to-value object 'wh-ratio scale)))
      (make-instance 'mwm-object :id (id object) ;; !!!
                     :x-pos (to-value object 'x-pos scale)
                     :y-pos (to-value object 'y-pos scale)
                     :area (to-value object 'area scale)
                     :wh-ratio wh-ratio
                     :R (first rgb-color)
                     :G (second rgb-color)
                     :B (third rgb-color)
                     :roughness roughness
                     :nr-of-sides sides
                     :nr-of-corners corners
                     :description `(,(shape object)
                                    ,(color object)
                                    ,(size object)
                                    ,(material object))))))

(defmethod add-noise-to-value ((object mwm-object) (attr (eql 'x-pos))
                               probability amount)
  (when (< (random 1.0) probability)
    (setf (x-pos object)
          (add-random-value-from-range (x-pos object) 0.0 amount))))

(defmethod add-noise-to-value ((object mwm-object) (attr (eql 'y-pos))
                               probability amount)
  (when (< (random 1.0) probability)
    (setf (y-pos object)
          (add-random-value-from-range (y-pos object) 0.0 amount))))

(defmethod add-noise-to-value ((object mwm-object) (attr (eql 'area))
                               probability amount)
  (when (< (random 1.0) probability)
    (setf (area object)
          (add-random-value-from-range (area object) 0.0 amount))))

(defmethod add-noise-to-value ((object mwm-object) (attr (eql 'wh-ratio))
                               probability amount)
  (when (< (random 1.0) probability)
    (setf (wh-ratio object)
          (add-random-value-from-range (wh-ratio object) 0.0 amount))))

(defmethod add-noise-to-value ((object mwm-object) (attr (eql 'roughness))
                               probability amount)
  (when (< (random 1.0) probability)
    (setf (roughness object)
          (add-random-value-from-range (roughness object) 0.0 amount))))

(defmethod add-noise-to-value ((object mwm-object) (attr (eql 'color))
                               probability amount)
  (loop for attr in '(R G B)
        when (< (random 1.0) probability)
        do (setf (slot-value object attr)
                 (add-random-value-from-range (slot-value object attr) 0.0 amount))))

(defmethod add-noise ((set mwm-object-set) probability amount)
  (loop for object in (objects set)
        do (add-noise object probability amount)))

(defmethod add-noise ((object mwm-object) probability amount)
  (add-noise-to-value object 'x-pos probability amount)
  (add-noise-to-value object 'y-pos probability amount)
  (add-noise-to-value object 'area probability amount)
  (add-noise-to-value object 'wh-ratio probability amount)
  (add-noise-to-value object 'color probability amount)
  (add-noise-to-value object 'roughness probability amount))
  
(defmethod object->alist ((object clevr-object))
  (let ((x-pos (first (coordinates object)))
        (y-pos (second (coordinates object))))
    (list (cons :color (color object))
          (cons :size (size object))
          (cons :shape (shape object))
          (cons :material (material object))
          (cons :x-pos (if (> x-pos 245.5) 'right 'left))
          (cons :y-pos (if (> y-pos 177) 'front 'behind)))))

;;;; copy object

(defmethod copy-object ((object mwm-object))
  (make-instance 'mwm-object :id (id object)
                 :x-pos (x-pos object) :y-pos (y-pos object)
                 :area (area object) :wh-ratio (wh-ratio object)
                 :roughness (roughness object)
                 :R (R object) :G (G object) :B (B object)
                 :nr-of-sides (nr-of-sides object)
                 :nr-of-corners (nr-of-corners object)
                 :description (copy-object (description object))))

(defmethod copy-object ((set mwm-object-set))
  (make-instance 'mwm-object-set :id (id set)
                 :objects (copy-object (objects set))))
