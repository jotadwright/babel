;;;;; --------------------------------------------------------------------------------------
;;;;; Copyright Sony Computer Science Laboratories, Paris Lab
;;;;;           Remi van Trijp
;;;;; --------------------------------------------------------------------------------------

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :geom-world)

(defun geom-select-shape (&optional geom-world)
  "Randomly select a shape to generate."
  (if geom-world
    (random-elt (shapes geom-world))
    (random-elt '(triangle circle rectangle square))))

(defun geom-color-pixel (image x y &rest rgb)
  "Color a pixel in an  image."
  (ignore-errors (setf (opticl:pixel image y x) (values-list rgb))))

(defun inside-triangle-p (x0 y0 x1 y1 x2 y2 x y triangle-area)
  "Check whether a point is inside a triangle or not."
  (let ((area-1 (geom-area 'triangle :x0 x :y0 y :x1 x1 :y1 y1 :x2 x2 :y2 y2))
        (area-2 (geom-area 'triangle :x0 x0 :y0 y0 :x1 x :y1 y :x2 x2 :y2 y2))
        (area-3 (geom-area 'triangle :x0 x0 :y0 y0 :x1 x1 :y1 y1 :x2 x :y2 y)))
    (= triangle-area (+ area-1 area-2 area-3))))

(defun calculate-midposition (max-bounding-box)
  "Return the hpos and vpos of a bounding  box."
  (let* ((width (- (third max-bounding-box) (first max-bounding-box)))
         (height (- (fourth max-bounding-box) (second max-bounding-box))))
    (values (+ (floor (/ width 2)) (first max-bounding-box)) ;; X
            (+ (floor (/ height 2)) (second max-bounding-box))))) ;; Y

(defun ensure-minimal-length (l max &optional (min 30))
  "Ensure that objects do not appear too small in the image."
  (cond ((> l min) l)
        ((< max min) (1- max))
        (t
         min)))

(defun image-width (image)
  "Get the width of an image represented as an array."
  (second (array-dimensions image)))

(defun image-height (image)
  "Get the height of an image represented as an array."
  (first (array-dimensions image)))

;;;;; Calculate the area of a shape.
;;;;; --------------------------------------------------------------------------------------
(defgeneric geom-area (shape &key &allow-other-keys))

(defmethod geom-area ((shape (eql 'triangle))
                      &key x0 x1 x2 y0 y1 y2 &allow-other-keys)
  (abs (/ (+ (* x0 (- y1 y2)) (* x1 (- y2 y0)) (* x2 (- y0 y1))) 2)))

(defmethod geom-area ((shape (eql 'circle))
                      &key radius &allow-other-keys)
  (* pi (expt radius 2)))

(defmethod geom-area ((shape (eql 'rectangle))
                      &key width height &allow-other-keys)
  (* width height))

(defmethod geom-area ((shape (eql 'square))
                      &key width height &allow-other-keys)
  (geom-area 'rectangle :width width :height height))
