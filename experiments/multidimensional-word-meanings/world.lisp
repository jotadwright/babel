(in-package :mwm)

(export '(mwm-object))

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

(defmethod object->alist ((object clevr-object))
  `((:color . ,(color object))
    (:size . ,(clevr-world::size object))
    (:shape . ,(shape object))
    (:material . ,(material object))
    (:xpos . ,(if (> (x-pos object) 240) 'right 'left))
    (:ypos . ,(if (> (y-pos object) 160) 'front 'behind))
    ))

;; ------------------
;; + MWM object set +
;; ------------------
(defclass mwm-object-set (entity)
  ((objects
    :documentation "the objects in the set"
    :type list :accessor objects :initarg :objects
    :initform nil)
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

;; when xpos and ypos are exactly equal to 0.5
;; they are considered to be left!
(defmethod to-value ((object clevr-object) (attr (eql 'xpos)))
  `((xpos . ,(x-pos object))))

(defmethod to-value ((object clevr-object) (attr (eql 'ypos)))
  `((ypos . ,(y-pos object))))

(defmethod to-value ((object clevr-object) (attr (eql 'area)))
  (let* ((area
          (case (clevr-world::size object)
            (small 30) (large 70)))
         (area-w-variance
          (add-random-value-from-range area 0 16 :max-bound 100)))
    `((area . ,area-w-variance))))

(defmethod to-value ((object clevr-object) (attr (eql 'sides-and-corners)))
  (let ((sides
         (case (shape object)
           (cube 6) (sphere 1) (cylinder 3)))
        (corners
         (case (shape object)
           (cube 8) (sphere 0) (cylinder 2))))
    `((nr-of-sides . ,sides)
      (nr-of-corners . ,corners))))

(defmethod to-value ((object clevr-object) (attr (eql 'color)))
  (let* ((rgb-color
          (case (color object)
            (gray   '(87  87  87))
            (red    '(173 34  35))
            (blue   '(44  76  215))
            (green  '(29  105 20))
            (brown  '(126 72  25))
            (purple '(130 39  192))
            (cyan   '(40  208 208))
            (yellow '(255 238 51))))
         (rgb-with-variance
          (loop for channel in rgb-color
                collect (add-random-value-from-range channel 0.0 2.0 :max-bound 255.0))))
    `((r . ,(first rgb-with-variance))
      (g . ,(second rgb-with-variance))
      (b . ,(third rgb-with-variance)))))

(defmethod to-value ((object clevr-object) (attr (eql 'roughness)))
  (let* ((roughness
          (case (material object)
            (metal 8)
            (rubber 2)))
         (roughness-with-variance
          (add-random-value-from-range roughness 0.0 2.5 :max-bound 10.0)))
    `((roughness . ,roughness-with-variance))))

(defmethod to-value ((object clevr-object) (attr (eql 'wh-ratio)))
  (let* ((ratio
          (case (shape object)
            (cube 1.0)
            (sphere 1.0)
            (cylinder 0.5)))
         (ratio-with-variance
          (add-random-value-from-range ratio 0.0 0.25)))
    `((wh-ratio . ,ratio-with-variance))))

;;;; clevr -> mwm
(defmethod clevr->simulated ((scene clevr-scene))
  (make-instance 'mwm-object-set :id (id scene)
                 :image (image scene)
                 :objects (loop for obj in (objects scene)
                                collect (clevr->simulated obj))))

(defmethod clevr->simulated ((object clevr-object))
  (make-instance 'mwm-object :id (id object) ;; !!!
                 :attributes (append (to-value object 'xpos)
                                     (to-value object 'ypos)
                                     (to-value object 'area)
                                     (to-value object 'wh-ratio)
                                     (to-value object 'color)
                                     (to-value object 'roughness)
                                     (to-value object 'sides-and-corners))
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

;; ------------------------
;; + Continous CLEVR data +
;; ------------------------

(defun extracted->mwm-object (alist)
  "Load a single object"
  (let* ((mean-color (rest (assoc :color-mean alist)))
         (lab (hsv->lab mean-color)))
    ;; create an alist
    (setf alist
          (mapcar #'(lambda (pair)
                      (cons (intern (upcase (mkstr (car pair))) :mwm)
                            (cdr pair)))
                  alist))
    ;; split the color channels
    (setf alist
          (append `((mean-l . ,(first lab))
                    (mean-a . ,(second lab))
                    (mean-b . ,(third lab)))
                  alist))
    (setf alist (remove 'color-mean alist :key #'car))
    (setf alist (remove 'color-std alist :key #'car))
    (setf alist (remove 'bb-area alist :key #'car))
    ;; flip the sign for angle
    (setf (cdr (assoc 'angle alist))
          (- (cdr (assoc 'angle alist))))
    ;; create an object
    (make-instance 'mwm-object
                   :id (make-id 'object)
                   :attributes alist)))

(defmethod clevr->extracted ((scene clevr-scene) &key directory)
  ;; take the name of the scene
  ;; look it up in 'directory'
  ;; and load the data
  (let* ((path
          (merge-pathnames
           (make-pathname :name (name scene) :type "json")
           directory))
         (objects
          (with-open-file (stream path :direction :input)
            (mapcar #'extracted->mwm-object
                    (mapcar #'decode-json-from-string
                            (stream->list stream))))))
    (make-instance 'mwm-object-set
                   :id (make-id 'scene)
                   :objects objects)))
