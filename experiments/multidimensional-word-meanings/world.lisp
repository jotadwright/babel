(in-package :mwm)

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

;; ------------------
;; + MWM object set +
;; ------------------
(defclass mwm-object-set (entity)
  ((objects
    :documentation "the objects in the set"
    :type list :accessor objects :initarg :objects)
   (image
    :documentation "path of the image of this set"
    :type pathname :accessor image :initarg :image))
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
  (let* ((x-pos (x-pos object))
         (scaled-x-pos (float (/ (- x-pos 31) (- 460 31)))))
    `((x-pos . ,(if scale-p scaled-x-pos x-pos)))))

(defmethod to-value ((object clevr-object) (attr (eql 'y-pos)) scale-p)
  (let* ((y-pos (y-pos object))
         (scaled-y-pos (float (/ (- y-pos 58) (- 296 58)))))
    `((y-pos . ,(if scale-p scaled-y-pos y-pos)))))

(defmethod to-value ((object clevr-object) (attr (eql 'area)) scale-p)
  (let* ((area (case (size object)
                 (small 30) (large 70)))
         (area-w-variance (add-random-value-from-range area 0 16 :max-bound 100))
         (scaled-area (float (/ area-w-variance 100))))
    `((area . ,(if scale-p scaled-area area-w-variance)))))

(defmethod to-value ((object clevr-object) (attr (eql 'sides-and-corners)) scale-p)
  (let ((sides (case (shape object)
                 (cube 6) (sphere 1) (cylinder 3)))
        (corners (case (shape object)
                   (cube 8) (sphere 0) (cylinder 0))))
    `((nr-of-sides . ,(if scale-p (/ (- sides 1) 5) sides))
      (nr-of-corners . ,(if scale-p (/ corners 8) corners)))))

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
    `((r . ,(if scale-p (first scaled-rgb) (first rgb-with-variance)))
      (g . ,(if scale-p (second scaled-rgb) (second rgb-with-variance)))
      (b . ,(if scale-p (third scaled-rgb) (third rgb-with-variance))))))

(defmethod to-value ((object clevr-object) (attr (eql 'roughness)) scale-p)
  (let* ((roughness
          (case (material object)
            (metal 8)
            (rubber 2)))
         (roughness-with-variance
          (add-random-value-from-range roughness 0.0 2.5 :max-bound 10.0))
         (scaled-roughness (/ roughness-with-variance 10.0)))
    `((roughness . ,(if scale-p scaled-roughness roughness-with-variance)))))

(defmethod to-value ((object clevr-object) (attr (eql 'wh-ratio)) scale-p)
  (let* ((ratio (case (shape object)
                  (cube 1.0)
                  (sphere 1.0)
                  (cylinder 0.5)))
         (ratio-with-variance (add-random-value-from-range ratio 0.0 0.25)))
    `((wh-ratio . ,(if scale-p (min ratio-with-variance 1.0) ratio-with-variance)))))

;;;; clevr -> mwm
(defmethod clevr->simulated ((scene clevr-scene)
                             &key (scale nil))
  (make-instance 'mwm-object-set :id (id scene)
                 :image (image scene)
                 :objects (loop for obj in (objects scene)
                                collect (clevr->mwm obj :scale scale))))

(defmethod clevr->simulated ((object clevr-object)
                             &key (scale nil))
  (make-instance 'mwm-object :id (id object) ;; !!!
                 :attributes (append (to-value object 'x-pos scale)
                                     (to-value object 'y-pos scale)
                                     (to-value object 'area scale)
                                     (to-value object 'wh-ratio scale)
                                     (to-value object 'color scale)
                                     (to-value object 'roughness scale)
                                     (to-value object 'sides-and-corners scale))
                 :description (object->alist object)))

;; ---------
;; + NOISE +
;; ---------
(defmethod add-noise ((set mwm-object-set) probability amount)
  (loop for object in (objects set)
        do (add-noise object probability amount)))

(defmethod add-noise ((object mwm-object) probability amount)
  (loop for (attr . val) in (attributes object)
        unless (member attr '(nr-of-sides nr-of-corners))
        do (when (< (random 1.0) probability)
             (set-attr-val object attr
                           (add-random-value-from-range val 0.0 amount)))))

;;; object -> a-list
(defmethod object->alist ((object clevr-object))
  `((:color . ,(color object))
    (:size . ,(size object))
    (:shape . ,(shape object))
    (:material . ,(material object))
    (:x-pos . ,(if (> (x-pos object) 245.5) 'right 'left))
    (:y-pos . ,(if (> (y-pos object) 177) 'front 'behind))))

;; ------------------------
;; + Continous CLEVR data +
;; ------------------------

(defparameter *continuous-clevr-boundaries*
  '((xpos . ((min . 12) (max . 464)))
    (ypos . ((min . 43) (max . 304)))
    (width . ((min . 10) (max . 260)))
    (height . ((min . 11) (max . 247)))
    (size-ratio . nil)
    (angle . ((min . 0) (max . 90)))
    (corners . ((min . 3) (max . 16)))
    (hamming-distance . ((min . 0.5) (max . 1)))
    (area . ((min . 5) (max . 26414)))
    (relative-area . nil)
    (bb-area . ((min . 100) (max . 45314)))
    (area-ratio . nil)
    (white-level . ((min . 0) (max . 3)))
    (mean-h . ((min . 0) (max . 180)))
    (mean-s . ((min . 0) (max . 255)))
    (mean-v . ((min . 0) (max . 255)))
    (std-h . ((min . 0) (max . 85)))
    (std-s . ((min . 0) (max . 85)))
    (std-v . ((min . 0) (max . 85)))))

(defun scale-continuous-clevr (object)
  (loop for (key . value) in (attributes object)
        for boundaries = (rest (assoc key *continuous-clevr-boundaries*))
        when boundaries
        do (setf (cdr (assoc key (attributes object)))
                 (/ (- value (cdr (assoc 'min boundaries)))
                    (- (cdr (assoc 'max boundaries)) (cdr (assoc 'min boundaries)))))))

(defun extracted->mwm-object (alist)
  "Load a single object"
  (let ((mean-color (rest (assoc :color-mean alist)))
        (std-color (rest (assoc :color-std alist))))
    ;; create an alist
    (setf alist
          (mapcar #'(lambda (pair)
                      (cons (internal-symb (car pair)) (cdr pair)))
                  alist))
    ;; split the color channels
    (setf alist
          (append `((mean-h . ,(first mean-color))
                    (mean-s . ,(second mean-color))
                    (mean-v . ,(third mean-color))
                    (std-h . ,(first std-color))
                    (std-s . ,(second std-color))
                    (std-v . ,(third std-color))) alist))
    (setf alist (remove 'color-mean alist :key #'car))
    (setf alist (remove 'color-std alist :key #'car))
    ;; flip the sign for angle
    (setf (cdr (assoc 'angle alist))
          (- (cdr (assoc 'angle alist))))
    ;; create an object
    (make-instance 'mwm-object :id (make-id 'object)
                   :attributes alist)))

(defmethod clevr->extracted ((scene clevr-scene) &key directory (scale nil))
  ;; take the name of the scene
  ;; look it up in 'directory'
  ;; and load the data
  (let* ((path (merge-pathnames
                (make-pathname :name (name scene) :type "json")
                directory))
         (objects (with-open-file (stream path :direction :input)
                    (mapcar #'continuous-clevr->mwm-object
                            (mapcar #'decode-json-from-string
                                    (stream->list stream))))))
    (when scale
      (loop for object in objects
            do (scale-continuous-clevr object)))
    (make-instance 'mwm-object-set :id (make-id 'scene)
                   :objects objects)))
