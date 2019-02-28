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

(defun object->width-and-height (object)
  "The width and the height depend
   on 2 aspects of the object:
   the shape and the size:
   large cylinder = 7x10 (area 70, ratio 0.7)
   small cylinder = 5x7 (area 35, ratio 0.71)
   large cube/sphere = 8.5x8.5 (area 72.25, ratio 1)
   small cube/sphere = 6x6 (area 36, ratio 1)"
  (let* ((w (if (eql (shape object) 'cylinder)
              (if (eql (size object) 'large)
                7
                5)
              (if (eql (size object) 'large)
                8.5
                6)))
         (h (if (eql (shape object) 'cylinder)
              (if (eql (size object) 'large)
                10
                7)
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
  (case (color object)
    (gray '(87 87 87))
    (red '(173 35 35))
    (blue '(42 75 215))
    (green '(29 105 20))
    (brown '(129 74 25))
    (purple '(129 38 192))
    (cyan '(41 208 208))
    (yellow '(255 238 51))))

(defun object->color-variance (object)
  "Return the variance of the object"
  (case (material object)
    (metal '(1 1 1))
    (rubber '(0.5 0.5 0.5))))

(defmethod clevr->mwm ((set clevr-object-set))
  (make-instance 'mwm-object-set
                 :id (id set)
                 :objects (loop for obj in (objects set)
                                collect (clevr->mwm obj))))

(defmethod clevr->mwm ((object clevr-object))
  "Create the object and scale at the same time.
   x = [0;320]
   y = [0;240]
   width = [0;8.5]
   height = [0;10]
   area = [0;75]
   wh-ratio = already scaled
   mean-rgb = [0;255]
   rgb-variance = already scaled
   nr-of-sides = [0;6]
   nr-of-corners = [0;8]"
  (multiple-value-bind (width height) (object->width-and-height object)
    (multiple-value-bind (sides corners) (object->sides-and-corners object)
      (make-instance 'mwm-object
                     :id (id object) ;; !!!
                     :x-pos (/ (first (coordinates object)) 320.0)
                     :y-pos (/ (second (coordinates object)) 240.0)
                     :width (/ width 8.5)
                     :height (/ height 10.0)
                     :area (/ (* width height) 75.0)
                     :wh-ratio (float (/ width height))
                     :mean-rgb (mapcar #'(lambda (c) (/ c 255.0)) (object->color object))
                     :rgb-variance (object->color-variance object)
                     :nr-of-sides (/ sides 6.0)
                     :nr-of-corners (/ corners 8.0)
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
          (cons :x-pos (if (> x-pos 160) 'right 'left))
          (cons :y-pos (if (> y-pos 120) 'front 'behind)))))