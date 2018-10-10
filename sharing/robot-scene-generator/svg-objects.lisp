(in-package :scene-generator)

(defparameter *basic-colors*
  '((0 0 255)
    (0 255 0)
    (255 0 0)
    (255 255 0)
    (0 255 255)
    (255 0 255)))

(defparameter *used-colors* nil)

(defun pick-color ()
  (loop for color = (random-elt *basic-colors*)
        unless (member color *used-colors* :test #'equal)
        do (push color *used-colors*)
        (return color)))

(defun reset-colors ()
  (setf *used-colors* nil))

;;;; svg-object base class
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass svg-object ()
  ((id :accessor id :initarg :id :initform (make-id 'svg-object)
       :documentation "ID of the object")
   (color :accessor color :initarg :color :initform (pick-color) ;; (randint :end 255 :dimensions '(3))
          :documentation "RGB color of the object")
   (xpos :accessor xpos :initarg :xpos :initform (randint :start (first *xrange*) :end (rest *xrange*))
         :documentation "Position of the object in x-axis")
   (ypos :accessor ypos :initarg :ypos :initform (randint :start (first *yrange*) :end (rest *yrange*))
         :documentation "Position of the object in y-axis"))
  (:documentation "Base class of svg-object"))

(defgeneric to-html (svg-object)
  (:documentation "Create html from an svg-object"))

(defgeneric bounding-box (svg-object)
  (:documentation "Compute the upper-left and lower-right coordinates of the bounding box of the svg-object"))

(defgeneric new-location (svg-object)
  (:documentation "Set a new location for the object; to avoid overlaps"))

(defmethod new-location ((o svg-object))
  (setf (xpos o) (randint :start (first *xrange*) :end (rest *xrange*)))
  (setf (ypos o) (randint :start (first *yrange*) :end (rest *yrange*)))
  o)

;;;; bounding box
;;;;;;;;;;;;;;;;;

(defclass bbox (svg-object)
  ((ul :accessor ul :initarg :ul :initform nil
       :documentation "Upper left coordinate")
   (lr :accessor lr :initarg :lr :initform nil
       :documentation "Lower right coordinates"))
  (:documentation "Bounding box represented by upper left and lower right coordinates"))

(defmethod to-html ((bb bbox))
  "Create the svg element in html"
  (let ((width (- (car (lr bb)) (car (ul bb))))
        (height (- (cdr (lr bb)) (cdr (ul bb)))))
    (with-html-output-to-string (*standard-output*)
      (htm (:rect :id (format nil "~a" (id bb))
                  :width (format nil "~a" width)
                  :height (format nil "~a" height)
                  :x (format nil "~a" (car (ul bb)))
                  :y (format nil "~a" (cdr (ul bb)))
                  :style (format nil "stroke:rgb(~{~a~^,~});stroke-width:5;fill:none" (color bb)))))))


;;;; rect svg object
;;;;;;;;;;;;;;;;;;;

(defclass svg-rect (svg-object)
  ((width :accessor width :initarg :width :initform (randint :start 50 :end 300)
          :documentation "Width of the rectangle")
   (height :accessor height :initarg :height :initform (randint :start 50 :end 300)
           :documentation "Height of the rectangle"))
  (:documentation "An SVG rectangle"))

(defmethod to-html ((r svg-rect))
  "Create the svg element in html"
  (with-html-output-to-string (*standard-output*)
    (htm (:rect :id (format nil "~a" (id r))
                :width (format nil "~a" (width r))
                :height (format nil "~a" (height r))
                :x (format nil "~a" (xpos r))
                :y (format nil "~a" (ypos r))
                :style (format nil "fill:rgb(~{~a~^,~})" (color r))
                :onclick (format nil "$(this).remove();")))))

(defmethod bounding-box ((r svg-rect))
  "Compute the upper-left and lower-right corners of the bounding box"
  (let ((upper-left (cons (xpos r)
                          (ypos r)))
        (lower-right (cons (+ (xpos r) (width r))
                           (+ (ypos r) (height r)))))
    (make-instance 'bbox
                   :ul upper-left
                   :lr lower-right
                   :color (color r))))

(defun make-svg-rect ()
  (make-instance 'svg-rect))


;;;; square svg object
;;;;;;;;;;;;;;;;;;;;;;

(defclass svg-square (svg-rect)
  ()
  (:documentation "An SVG square"))

(defmethod initialize-instance :after ((s svg-square) &rest args)
  "Set the width equal to height to obtain a square"
  (setf (height s) (width s)))

(defun make-svg-square ()
  (make-instance 'svg-square))

;;;; circle svg object
;;;;;;;;;;;;;;;;;;;;;;

(defclass svg-circle (svg-object)
  ((radius :accessor radius :initarg :radius :initform (randint :start 25 :end 150)
           :documentation "Radius of the circle"))
  (:documentation "An SVG circle"))

(defmethod to-html ((c svg-circle))
  "Create the svg element in html"
  (with-html-output-to-string (*standard-output*)
    (htm (:circle :id (format nil "~a" (id c))
                  :r (format nil "~a" (radius c))
                  :cx (format nil "~a" (xpos c))
                  :cy (format nil "~a" (ypos c))
                  :style (format nil "fill:rgb(~{~a~^,~})" (color c))
                  :onclick (format nil "$(this).remove();")))))

(defmethod bounding-box ((c svg-circle))
  "Compute the upper-left and lower-right corners of the bounding box"
  (let ((upper-left (cons (- (xpos c) (radius c))
                          (- (ypos c) (radius c))))
        (lower-right (cons (+ (xpos c) (radius c))
                           (+ (ypos c) (radius c)))))
    (make-instance 'bbox
                   :ul upper-left
                   :lr lower-right
                   :color (color c))))

(defun make-svg-circle ()
  (make-instance 'svg-circle))

;;;; ellipse svg object
;;;;;;;;;;;;;;;;;;;;;;;

(defclass svg-ellipse (svg-object)
  ((xradius :accessor xradius :initarg :xradius :initform (randint :start 25 :end 150)
            :documentation "Radius in the x-axis")
   (yradius :accessor yradius :initarg :yradius :initform (randint :start 25 :end 150)
            :documentation "Radius in the y-axis")
   (rotation :accessor rotation :initarg :rotation :initform (random 180)
             :documentation "Rotation of the ellipse"))
  (:documentation "An SVG ellipse"))

(defmethod to-html ((e svg-ellipse))
  "Create the svg element in html"
  (with-html-output-to-string (*standard-output*)
    (htm (:ellipse :id (format nil "~a" (id e))
                   :rx (format nil "~a" (xradius e))
                   :ry (format nil "~a" (yradius e))
                   :cx (format nil "~a" (xpos e))
                   :cy (format nil "~a" (ypos e))
                   ; :transform (format nil "rotate(~a)" (rotation e))
                   :style (format nil "fill:rgb(~{~a~^,~})" (color e))
                   :onclick (format nil "$(this).remove();")))))

#|
(defmethod bounding-box ((e svg-ellipse))
  "Compute the upper-left and lower-right corners of the bounding box"
  ;; This is not yet correct!
  (let* ((phi (deg-to-rad (rotation e)))
         (ux (* (xradius e) (cos phi)))
         (uy (* (xradius e) (sin phi)))
         (vx (* (yradius e) (cos (+ phi (/ pi 2)))))
         (vy (* (yradius e) (sin (+ phi (/ pi 2)))))
         (bbox-halfwidth (sqrt (+ (* ux ux) (* vx vx))))
         (bbox-halfheight (sqrt (+ (* uy uy) (* vy vy))))
         (upper-left (cons (round (- (xpos e) bbox-halfwidth))
                           (round (- (ypos e) bbox-halfheight))))
         (lower-right (cons (round (+ (xpos e) bbox-halfwidth))
                            (round (+ (ypos e) bbox-halfheight)))))
    (make-instance 'bbox
                   :ul upper-left
                   :lr lower-right)))
|#

(defmethod bounding-box ((e svg-ellipse))
  "Compute the upper-left and lower-right corners of the bounding box"
  (let ((upper-left (cons (- (xpos e) (xradius e))
                          (- (ypos e) (yradius e))))
        (lower-right (cons (+ (xpos e) (xradius e))
                           (+ (ypos e) (yradius e)))))
    (make-instance 'bbox
                   :ul upper-left
                   :lr lower-right
                   :color (color e))))
         

(defun make-svg-ellipse ()
  (make-instance 'svg-ellipse))

;;;; triangle svg object
;;;;;;;;;;;;;;;;;;;;;;;;

(defclass svg-triangle (svg-object)
  ((width :accessor width :initarg :width :initform (randint :start 100 :end 350)
          :documentation "Width of the triangle")
   (height :accessor height :initarg :height :initform nil
           :documentation "Height of the triangle")
   (orientation :accessor orientation :initarg :orientation :initform (utils:random-elt '(up down left right))
                :documentation "Orientation of the triangle"))
  (:documentation "An SVG triangle"))

(defmethod initialize-instance :after ((tr svg-triangle) &rest args)
  "Set height to width; we get equilateral triangles"
  (setf (height tr) (width tr)))
  
(defmethod compute-coordinates ((tr svg-triangle))
  "Compute 3 end points of the triangle"
  (let ((x-base (xpos tr))
        (y-base (ypos tr))
        (width (width tr))
        (height (height tr))
        (coordinates nil))
    (case (orientation tr)
      (up (let* ((half-width (round (/ width 2))))
            (push (list (- x-base half-width) y-base) coordinates)
            (push (list (+ x-base half-width) y-base) coordinates)
            (push (list x-base (- y-base height)) coordinates)))
      (down (let* ((half-width (round (/ width 2))))
              (push (list (- x-base half-width) y-base) coordinates)
              (push (list (+ x-base half-width) y-base) coordinates)
              (push (list x-base (+ y-base height)) coordinates)))
      (left (let* ((half-height (round (/ height 2))))
              (push (list x-base (- y-base half-height)) coordinates)
              (push (list x-base (+ y-base half-height)) coordinates)
              (push (list (- x-base width) y-base) coordinates)))
      (right (let* ((half-height (round (/ height 2))))
               (push (list x-base (- y-base half-height)) coordinates)
               (push (list x-base (+ y-base half-height)) coordinates)
               (push (list (+ x-base width) y-base) coordinates))))
    coordinates))

(defmethod to-html ((tr svg-triangle))
  "Create the svg element in html"
  (let ((coordinates (compute-coordinates tr)))
    (with-html-output-to-string (*standard-output*)
      (htm (:polygon :id (format nil "~a" (id tr))
                     :points (format nil "~{ ~{~a~^,~} ~}" coordinates)
                     :style (format nil "fill:rgb(~{~a~^,~})" (color tr))
                     :onclick (format nil "$(this).remove();"))))))

(defmethod bounding-box ((tr svg-triangle))
  (let* ((coordinates (compute-coordinates tr))
         (xs (mapcar #'first coordinates))
         (ys (mapcar #'second coordinates))
         (upper-left (cons (apply #'min xs)
                           (apply #'min ys)))
         (lower-right (cons (apply #'max xs)
                            (apply #'max ys))))
    (make-instance 'bbox
                   :ul upper-left
                   :lr lower-right
                   :color (color tr))))

(defun make-svg-triangle ()
  (make-instance 'svg-triangle))

;;;; create random svgs
;;;;;;;;;;;;;;;;;;;;;;;

(defun random-svgs (n &key (to-html t) (draw-bbox t))
  (let ((funcs (list ;#'make-svg-rect
                     ;#'make-svg-square
                     #'make-svg-circle
                     ;#'make-svg-ellipse
                     ;#'make-svg-triangle
                     ))
        (results nil)
        (bboxes nil))
    (loop repeat n
          for svg = (funcall (random-elt funcs))
          for bbox = (bounding-box svg)
          for overlap = (member t (mapcar (lambda (bb) (overlap-p bb bbox)) bboxes))
          do (while overlap
               (setf svg (new-location svg))
               (setf bbox (bounding-box svg))
               (setf overlap (member t (mapcar (lambda (bb) (overlap-p bb bbox)) bboxes))))
          do (push svg results)
          do (push bbox bboxes))
    (when to-html
      (setf results (mapcar #'to-html results)))
    (when draw-bbox
      (setf results (append results (mapcar #'to-html bboxes))))
    results))
               

;;;; compute overlap
;;;;;;;;;;;;;;;;;;;;

(defun overlap-p (bbox-1 bbox-2)
  "Compute the area of the intersection of 2 bboxes"
  ;; First, determine the coordinates of the intersection bbox
  (let ((x-left (max (car (ul bbox-1))
                     (car (ul bbox-2))))
        (y-top (max (cdr (ul bbox-1))
                    (cdr (ul bbox-2))))
        (x-right (min (car (lr bbox-1))
                      (car (lr bbox-2))))
        (y-bottom (min (cdr (lr bbox-1))
                       (cdr (lr bbox-2)))))
    ;; If this coordinates form a valid bbox; there is overlap
    ;; If not; there is no overlap
    (not (or (< x-right x-left)
             (< y-bottom y-top)))))
