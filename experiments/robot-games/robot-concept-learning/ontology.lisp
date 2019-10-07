(in-package :mwm)

;;;; Category classes

(defclass mwm-category (entity)
  ((attribute
    :documentation "The attribute this category operates on"
    :accessor attribute :initarg :attribute :type symbol))
  (:documentation "base class for all categories"))

(defclass min-max-category (mwm-category)
  ((lower-bound
    :accessor lower-bound :initarg :lower-bound :initform 0)
   (upper-bound
    :accessor upper-bound :initarg :upper-bound :initform 1)))

(defclass prototype-category (mwm-category)
  ((prototype
    :accessor prototype :initarg :prototype)
   (nr-samples
    :accessor nr-samples :initarg :nr-samples :initform 0)
   (M2
    :accessor M2 :initarg :M2 :initform nil)))

(defclass prototype-min-max-category (min-max-category prototype-category)
  ((lower-m
    :documentation "The slope of the function between lower-bound and prototype"
    :accessor lower-m :initarg :lower-m)
   (lower-b
    :documentation "The y-intercept of the function between lower-bound and prototype"
    :accessor lower-b :initarg :lower-b)
   (upper-m
    :documentation "The slope of the function between upper-bound and prototype"
    :accessor upper-m :initarg :upper-m)
   (upper-b
    :documentation "The y-intercept of the function between upper-bound and prototype"
    :accessor upper-b :initarg :upper-b))
  (:documentation "First order function y=mx+b; both left and right of the prototype"))

(defclass exponential-category (prototype-category)
  ((left-sigma
    :documentation "Sigma determines the strength of the exponential decay"
    :accessor left-sigma :initarg :left-sigma)
   (right-sigma
    :documentation "Sigma determines the strength of the exponential decay"
    :accessor right-sigma :initarg :right-sigma))
  (:documentation "test category"))

;;;; Make category from object
(defgeneric make-category (attr value representation)
  (:documentation "Make a category based on an attribute and its value.
   The category is suited for use with strategy."))

(defmethod make-category (attr value (representation (eql :min-max)))
  (make-instance 'min-max-category :attribute attr
                 :id (make-id (string-append (mkstr attr) "-min-max-category"))
                 :lower-bound value
                 :upper-bound value))

(defmethod make-category (attr value (representation (eql :prototype)))
  (make-instance 'prototype-category :attribute attr
                 :id (make-id (string-append (mkstr attr) "-prototype-category"))
                 :prototype value
                 :nr-samples 1
                 :M2 0.05))

(defmethod make-category (attr value (representation (eql :prototype-min-max)))
  (make-instance 'prototype-min-max-category
                 :id (make-id (string-append (mkstr attr) "-prototype-min-max-category"))
                 :attribute attr :prototype value :nr-samples 1
                 :lower-bound value :upper-bound value
                 ;; cannot yet determine a linear function with a single point
                 :lower-m nil :lower-b nil
                 :upper-m nil :upper-b nil))

(defmethod make-category (attr value (representation (eql :exponential)))
  (make-instance 'exponential-category :id (make-id (string-append (mkstr attr) "-exponential-category"))
                 :attribute attr :prototype value
                 :nr-samples 1
                 :left-sigma 10 :right-sigma 10))

;;;; Update category based on object
(defgeneric update-category (category object &key &allow-other-keys)
  (:documentation "Update the category based on the object"))

(defun closest-boundary (value category)
  (if (= (lower-bound category) (upper-bound category))
    (if (< value (lower-bound category))
      'lower-bound 'upper-bound)
    (if (< (abs (- value (lower-bound category)))
           (abs (- value (upper-bound category))))
      'lower-bound 'upper-bound)))

(defmethod new-boundary (c i o (b (eql 'lower-bound)))
  ;; c = current-boundary
  ;; i = interpreted-value
  ;; o = topic-value
  (cond (; | i t  -> i | t (reduce)
         (and (>= i c) (>= o c)) o)
         ; | t i (remain)
        ((and (>= o c) (>= i c) (>= i o)) c)
         ; i | t (remain)
        ((and (< i c) (>= o c)) c)
         ; t | i -> | t i (extend)
        ((and (<= o c) (>= i c)) o)
         ; i t | -> i | t (extend)
        ((and (< i c) (<= o c) (< i o)) o)
         ; t i | -> | t i (extend)
        ((and (< o c) (<= i c) (< o i)) o)))

(defmethod new-boundary (c i o (b (eql 'upper-bound)))
  ;; c = current-boundary
  ;; i = interpreted-value
  ;; o = topic-value
  (cond (; t i | -> t | i (reduce)
         (and (<= i c) (<= o c)) o)
         ; i t | (remain)
        ((and (<= o c) (<= i c) (<= i o)) c)
         ; t | i (remain)
        ((and (<= o c) (> i c)) c)
         ; i | t -> i t | (extend)
        ((and (<= i c) (>= o c)) o)
         ; | t i -> t | i (extend)
        ((and (>= o c) (> i c) (>= i o)) o)
         ; | i t -> i t | (extend)
        ((and (>= i c) (> o c) (> o i)) o)))

(defun update-boundaries (category topic-value object-value)
  (let ((boundaries-updated nil))
    ;; if interpreted object, use it to change the boundary
    (when object-value
      (let ((topic-boundary (closest-boundary topic-value category))
            (object-boundary (closest-boundary object-value category)))
        ;; get the boundary closest to both the topic and the interpreted object
        ;; if the same, update the boundary according to the rules above
        (when (eql topic-boundary object-boundary)
          (let ((new (new-boundary (slot-value category topic-boundary)
                                   object-value topic-value topic-boundary)))
            ;; here we check if the rules actually return something
            ;; it should always return something, otherwise there are
            ;; cases missing
            (when new
              (setf boundaries-updated t)
              (setf (slot-value category topic-boundary) new))))))
    ;; if all of the above fail, simply update the boundaries as before
    (unless boundaries-updated
      (let ((new-lower-bound (min (lower-bound category) topic-value))
            (new-upper-bound (max (upper-bound category) topic-value)))
        (setf (lower-bound category) new-lower-bound
              (upper-bound category) new-upper-bound)))))

#|
(defmethod update-category ((category min-max-category)
                            (object mwm-object)
                            &key success
                            interpreted-object)
  (unless success
    (update-boundaries category (get-attr-val object (attribute category))
                       (when interpreted-object
                         (get-attr-val interpreted-object (attribute category)))))
  category)
|#

(defmethod update-category ((category min-max-category)
                            (object mwm-object)
                            &key success
                            interpreted-object)
  (declare (ignorable success interpreted-object))
  (let ((topic-value (get-attr-val object (attribute category))))
    (setf (lower-bound category) (min (lower-bound category) topic-value)
          (upper-bound category) (max (upper-bound category) topic-value)))
  category)
                                   

               
(defmethod update-category ((category prototype-category)
                            (object mwm-object)
                            &key success
                            interpreted-object)
  (declare (ignorable success interpreted-object))
  ;; take the object pointed to by the tutor
  ;; and estimate the mean and variance of the category
  (incf (nr-samples category))
  (let* ((value (get-attr-val object (attribute category)))
         (delta-1 (- value (prototype category)))
         (new-prototype (+ (prototype category) (/ delta-1 (nr-samples category))))
         (delta-2 (- value new-prototype))
         (new-M2 (+ (M2 category) (* delta-1 delta-2))))
    (setf (prototype category) new-prototype
          (M2 category) new-M2)
    category))
         

(defun solve-linear-function-equation (prototype bound)
  ;; Get the slope and y-intercept of the linear function
  ;; between prototype and boundary. On the y-axis, the
  ;; prototype gets value 1. The boundary gets value 0.
  ;; m = (y2-y1)/(x2-x1)
  ;; b = y - mx (using x1,y1 or x2,y2)
  (if (/= prototype bound)
    (let* ((p1 (cons prototype 1))
           (p2 (cons bound 0))
           (m (/ (- (cdr p2) (cdr p1))
                 (- (car p2) (car p1))))
           (b (- (cdr p1) (* m (car p1)))))
      (values m b))
    (values nil nil)))

(defmethod update-category ((category prototype-min-max-category)
                            (object mwm-object)
                            &key success
                            interpreted-object)
  ;; shift the prototype towards the object
  ;; update lower-bound and upper-bound
  ;; recompute m and b for both sides
  (incf (nr-samples category))
  (let* ((topic-value (get-attr-val object (attribute category)))
         (delta (- topic-value (prototype category)))
         (new-prototype (+ (prototype category)
                           (/ delta (nr-samples category)))))
    (setf (prototype category) new-prototype)
    (unless success
      (update-boundaries category topic-value
                         (when interpreted-object
                           (get-attr-val interpreted-object (attribute category))))
      (multiple-value-bind (new-lower-m new-lower-b)
          (unless (= new-prototype (lower-bound category))
            (solve-linear-function-equation new-prototype (lower-bound category)))
        (multiple-value-bind (new-upper-m new-upper-b)
            (unless (= new-prototype (upper-bound category))
              (solve-linear-function-equation new-prototype (upper-bound category)))
          (setf (lower-m category) new-lower-m
                (lower-b category) new-lower-b
                (upper-m category) new-upper-m
                (upper-b category) new-upper-b)))))
  category)         

#|
(defmethod update-category ((category exponential-category)
                            (object mwm-object)
                            &key (alpha 0.05)
                            success
                            interpreted-object)
  (let ((value (get-attr-val object (attribute category))))
    ;; shift the prototype towards the true topic
    (setf (prototype category)
          (shift (prototype category) value :alpha alpha))
    ;; Update the appropriate sigma (left or right) only when the game failed!
    ;; The update is based on the distance between the prototype and the value
    ;; of the interpreted object(!!)
    ;; s' = s + 1/(1-d) if failure
    ;; How larger the sigma, how steeper the function is
    ;; Special case when the distance is 1.0 -> causes division by zero error
    ;; When the game failed and the interpreted value lies within the range of
    ;; the function, then it needs to be steeper (increase sigma), so it gets outside.
    ;; Otherwise, if the game failed and the value lies outside the range of the function,
    ;; it needs to be less steep (decrease sigma) so it gets inside.
    (when (and interpreted-object (not success))
      (let* ((interpreted-value (get-attr-val interpreted-object (attribute category)))
             (sigma-slot (if (> interpreted-value (prototype category)) 'right-sigma 'left-sigma))
             (distance (abs (- (prototype category) interpreted-value)))
             (delta (if (= distance 1.0)
                      (/ 1.0 0.1)
                      (/ 1.0 (- 1.0 distance)))))
        (if (> (apply-exponential-function interpreted-value (prototype category)
                                           (slot-value category sigma-slot))
               0.05)
          (setf (slot-value category sigma-slot)
                (min (+ (slot-value category sigma-slot) delta) 100))
          (setf (slot-value category sigma-slot)
                (- (slot-value category sigma-slot) delta))))))
  category)
|#

(defmethod update-category ((category exponential-category)
                            (object mwm-object)
                            &key success
                            interpreted-object)
  (incf (nr-samples category))         
  (let* ((value (get-attr-val object (attribute category)))
         (delta (- value (prototype category)))
         (new-prototype (+ (prototype category)
                           (/ delta (nr-samples category)))))
    ;; shift the prototype towards the topic
    (setf (prototype category) new-prototype)
    ;; update the slope
    ;; if you were not successful, make the slope less steep
    ;; as to include the value of the topic.
    (unless success
      (let* ((sigma-slot (if (> value (prototype category)) 'right-sigma 'left-sigma))
             (sim-value (apply-exponential-function value (prototype category)
                                                    (slot-value category sigma-slot))))
        (when (< sim-value 0.5)
          (unless (<= (slot-value category sigma-slot) 1.0)
            (setf (slot-value category sigma-slot)
                  (decf (slot-value category sigma-slot) 0.5)))))))
  category)


;;;; Compute similarity
(defgeneric similarity (object category)
  (:documentation "Compute the similarity between object and category"))

(defmethod similarity ((object mwm-object) (category min-max-category))
  (let ((value (get-attr-val object (attribute category)))
        (lower-bound (lower-bound category))
        (upper-bound (upper-bound category)))
    (if (and (>= value lower-bound)
             (<= value upper-bound))
      1 -1)))

(defmethod similarity ((object mwm-object) (category prototype-category))
  (let* ((value (get-attr-val object (attribute category)))
         (prototype (prototype category))
         (variance (/ (M2 category) (nr-samples category)))
         (stdev (sqrt variance))
         (z-score (abs (/ (- value prototype) stdev))))
    (max (+ (/ (- z-score) 2) 1) -1.0)))

(defun apply-linear-function (value category)
  ;; Apply the linear function. If the value falls outside the triangle,
  ;; this will return a negative number. If the value falls within the
  ;; triangle, this will return a positive number. Threshold the negative
  ;; values at -1. When the category is not yet a triangle (single line)
  ;; than compute similarity as 1 - |distance|.
  (if (< value (prototype category))
    (if (and (lower-m category) (lower-b category))
      (max (+ (* (lower-m category) value) (lower-b category)) -1)
      (- 1 (abs (- value (prototype category)))))
    (if (and (upper-m category) (upper-b category))
      (max (+ (* (upper-m category) value) (upper-b category)) -1)
      (- 1 (abs (- value (prototype category)))))))

(defmethod similarity ((object mwm-object) (category prototype-min-max-category))
  ;; use the linear function to determine similarity.
  (let ((value (get-attr-val object (attribute category))))
    (apply-linear-function value category)))

(defun apply-exponential-function (value prototype sigma)
  (let* ((distance (abs (- value prototype)))
         (power (min (* (- sigma) distance) 100)))
    (exp power)))

(defmethod similarity ((object mwm-object) (category exponential-category))
  (let ((value (get-attr-val object (attribute category))))
    (cond ((>= value (prototype category))
           (apply-exponential-function value (prototype category) (right-sigma category)))
          ((< value (prototype category))
           (apply-exponential-function value (prototype category) (left-sigma category))))))


;;;; Weighted Similarity
(defmethod weighted-similarity ((object mwm-object) meaning)
  (loop for (category . certainty) in meaning
        collect (* certainty (similarity object category)) into wsim
        finally (return (average wsim))))

;;;; Copy Object Content
(defmethod copy-object-content ((source entity) (destination entity))
  (setf (id destination) (make-id (get-base-name (mkstr (id source))))))

(defmethod copy-object-content ((source mwm-category) (destination mwm-category))
  (setf (attribute destination) (copy-object (attribute source))))

(defmethod copy-object-content ((source min-max-category) (destination min-max-category))
  (setf (lower-bound destination) (copy-object (lower-bound source))
        (upper-bound destination) (copy-object (upper-bound source))))

