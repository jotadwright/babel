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

(export '(geom-display-scene geom-generate-scene geom-generate-scenes
                             geom-generate-object geom-draw-shape))

;;;;; --------------------------------------------------------------------------------------
;;;;; Generating and Drawing Shapes
;;;;; --------------------------------------------------------------------------------------

(defgeneric geom-generate-object (shape max-bounding-box world))

(defmethod geom-generate-object ((shape (eql 'square))
                                 (max-bounding-box list)
                                 (world geom-world))
  (multiple-value-bind (hpos vpos)
      (calculate-midposition max-bounding-box)
    (let* ((max-l (first (sort (list (- (third max-bounding-box) hpos) 
                                     (- (fourth max-bounding-box) vpos)) #'<)))
           (actual-l (ensure-minimal-length (random max-l) max-l)))
      (make-instance 'geom-square
                     :area (* actual-l actual-l)
                     :color (random-elt (colors world))
                     :hpos hpos
                     :vpos vpos
                     :edge-count 4
                     :bounding-box (list (- hpos actual-l)
                                         (- vpos actual-l)
                                         (+ hpos actual-l)
                                         (+ vpos actual-l))))))

(defmethod geom-generate-object ((shape (eql 'rectangle))
                                 (max-bounding-box list)
                                 (world geom-world))
  (multiple-value-bind (hpos vpos)
      (calculate-midposition max-bounding-box)
    (let* ((max-h-distance (- hpos (first max-bounding-box)))
           (max-v-distance (- vpos (second max-bounding-box)))
           (actual-h-distance (ensure-minimal-length (random max-h-distance) max-h-distance))
           (actual-v-distance (ensure-minimal-length (random max-v-distance) max-v-distance)))
      (make-instance 'geom-rectangle
                     :area (* (* 2 actual-h-distance) (* 2 actual-v-distance))
                     :color (random-elt (colors world))
                     :hpos hpos
                     :vpos vpos
                     :edge-count 4
                     :bounding-box (list (- hpos actual-h-distance)
                                         (- vpos actual-v-distance)
                                         (+ hpos actual-h-distance)
                                         (+ vpos actual-v-distance))))))

(defmethod geom-generate-object ((shape (eql 'triangle))
                                 (max-bounding-box list)
                                 (world geom-world))
  (multiple-value-bind (hpos vpos)
      (calculate-midposition max-bounding-box)
    (let* ((max-h-distance (- hpos (first max-bounding-box)))
           (max-v-distance (- vpos (second max-bounding-box)))
           (actual-h-distance (ensure-minimal-length (random max-h-distance) max-h-distance))
           (actual-v-distance (ensure-minimal-length (random max-v-distance) max-v-distance))
           ;; Get the three edges of the triangle. We will limit the kind of triangles that 
           ;; can be generated so we won't get "weird" triangles
           (edges (list (list hpos (- vpos actual-v-distance))
                        (list (- hpos actual-h-distance) (+ vpos actual-v-distance))
                        (list (+ hpos actual-h-distance) (+ vpos actual-v-distance))))
           ;; Now calculate the bounding box
           (sorted-x (sort (mapcar #'first edges) #'>))
           (sorted-y (sort (mapcar #'second edges) #'>))
           (bbox (list (last-elt sorted-x)
                       (last-elt sorted-y)
                       (first sorted-x)
                       (first sorted-y)))
           ;; Calculate the area
           (area (geom-area 'triangle
                            :x0 (first (first edges))
                            :x1 (first (second edges))
                            :x2 (first (third edges))
                            :y0 (second (first edges))
                            :y1 (second (second edges))
                            :y2 (second (third edges)))))
      (make-instance 'geom-triangle
                     :area area
                     :color (random-elt (colors world))
                     :hpos hpos
                     :vpos vpos
                     :edges edges
                     :edge-count 3
                     :bounding-box bbox))))

(defmethod geom-generate-object ((shape (eql 'circle))
                                 (max-bounding-box list)
                                 (world geom-world))
  (multiple-value-bind (hpos vpos)
      (calculate-midposition max-bounding-box)
    (let* ((max-radius (first (sort (list (- hpos (first max-bounding-box))
                                          (- (third max-bounding-box) hpos)
                                          (- vpos (second max-bounding-box))
                                          (- (fourth max-bounding-box) vpos))
                                    #'<)))
           (actual-radius (ensure-minimal-length (random max-radius) max-radius))
           (bounding-box (list (- hpos actual-radius)
                               (- vpos actual-radius)
                               (+ hpos actual-radius)
                               (+ vpos actual-radius))))
      (make-instance 'geom-circle
                     :area (geom-area 'circle :radius actual-radius)
                     :bounding-box bounding-box
                     :color (random-elt (colors world))
                     :hpos hpos
                     :vpos vpos
                     :edge-count 0
                     :radius actual-radius))))

(defgeneric geom-draw-shape (object image))

(defmethod geom-draw-shape ((object geom-circle)
                            (image t))
  (opticl:fill-circle image (vpos object) (hpos object) (radius object) (first (color object)) (second (color object)) (third (color object))))

(defmethod geom-draw-shape ((object geom-rectangle)
                            (image t))
  (let ((bbox (bounding-box object)))
    (opticl:fill-rectangle image (second bbox) (first bbox) (fourth bbox) (third bbox)
                           (first (color object)) (second (color object)) (third (color object)))))

(defmethod geom-draw-shape ((object geom-square)
                            (image t))
  (call-next-method))

(defmethod geom-draw-shape ((object geom-triangle)
                            (image t))
  (let* ((points (edges object))
         (x0 (first (first points)))
         (x1 (first (second points)))
         (x2 (first (third points)))
         (y0 (second (first points)))
         (y1 (second (second points)))
         (y2 (second (third points)))
         (red (first (color object)))
         (green (second (color object)))
         (blue (third (color object))))
    ;; We first "fill" the triangle by coloring every pixel inside of it:
    (loop for y from (second (bounding-box object)) to (fourth (bounding-box object)) ; from min-y to max-y
          do (loop for x from (first (bounding-box object)) to (third (bounding-box object)) ; from min-x to max-x
                   when (inside-triangle-p x0 y0 x1 y1 x2 y2 x y (area object))
                   do (geom-color-pixel image x y red green blue)))
      ;; Then we actually draw the triangle (in order to get the correct pathname)
      (opticl:draw-triangle image y0 x0 y1 x1 y2 x2 red green blue)))

;;;;; --------------------------------------------------------------------------------------
;;;;; Geom-Generate-Scene
;;;;; --------------------------------------------------------------------------------------

(defun geom-generate-scene-aux (&key experiment (min-nr-of-objects 2) (max-nr-of-objects 5)
                                     (base-image-name "base-image"))
  "Helper function for generating a new scene."
  (let* ((base-image (opticl:read-png-file (base-image-path base-image-name)))
         (scene-width (image-width base-image))
         (scene-height (image-height base-image))
         (number-of-objects  (let ((n (1+ (random max-nr-of-objects))))
                               (if (< n min-nr-of-objects)
                                 min-nr-of-objects
                                 n)))
         (grids (list (list 0 0 scene-width scene-height)))) ;; x1 y1 x2 y2
    ;; We divide the image into a grid
    (loop until (= (length grids) number-of-objects)
          do (let* ((box (random-elt grids))
                    (partition-method (random-elt '(horizontal vertical)))
                    (width (- (third box) (first box)))
                    (height (- (fourth box) (second box)))
                    (mid-x (+ (floor (/ width 2)) (first box)))
                    (mid-y (+ (floor (/ height 2)) (second box)))
                    (box1 (if (eql partition-method 'horizontal)
                            `(,(first box) ,(second box) ,mid-x ,(fourth box))
                            `(,(first box) ,(second box) ,(third box) ,mid-y)))
                    (box2 (if (eql partition-method 'horizontal)
                            `(,mid-x ,(second box) ,(third box) ,(fourth box))
                            `(,(first box) ,mid-y ,(third box) ,(fourth box)))))
               (setf grids (append (list box1 box2) (remove box grids :test #'equal)))))
    ;; Now we create the scene and populate it one object per box in the grid.
    (values (make-instance 'geom-scene
                           :area (* scene-width scene-height)
                           :width scene-width
                           :height scene-height
                           :objects (loop for box in grids
                                          for shape = (if experiment
                                                        (geom-select-shape (world experiment))
                                                        (geom-select-shape))
                                          collect (if experiment
                                                    (geom-generate-object shape box (world experiment))
                                                    (geom-generate-object shape box (make-instance 'geom-world)))))
            base-image)))

(defun geom-display-scene (scene &optional (reset nil))
  "Display a scene in the web interface. Do not reset for integration in other information."
  (when reset
    (web-interface::reset))
  (add-element `((a :href "javascript:ajax_nextscene();")
                 ((img :src ,(format nil "/geom-images/~a" (file-namestring (image scene)))
                       :style "border:3px solid black")))))

(defun geom-generate-scene (&key experiment (file-prefix (gensym)) (display? t) (reset nil))
  "Generate a new scene with the option to display it in the web interface."
  (let ((min-nr-of-objects (if experiment (get-configuration experiment :min-nr-of-objects) 2))
        (max-nr-of-objects (if experiment (get-configuration experiment :max-nr-of-objects) 5)))
    (multiple-value-bind (scene base-image)
        (geom-generate-scene-aux :experiment experiment
                                 :min-nr-of-objects min-nr-of-objects
                                 :max-nr-of-objects max-nr-of-objects)
      ;; Now draw the objects:
      (loop for object in (objects scene)
            do (geom-draw-shape object base-image))
      (setf (image scene) 
            (opticl:write-png-file 
             (ensure-directories-exist 
              (geom-image-path (format nil "~a-~a"  file-prefix 
                                       (utils::get-next-id-number 'file-prefix))))
             base-image))
      (when display? (geom-display-scene scene reset))
      scene)))
;; (geom-generate-scene (make-instance 'geom-th-experiment) :file-prefix 'test)

(defun geom-generate-scenes (&key experiment number-of-scenes (file-prefix (gensym)))
  "Generate a number of scenes."
  (loop for i from 1 to number-of-scenes
        collect (geom-generate-scene :experiment experiment
                                     :file-prefix file-prefix
                                     :display? nil)))
;; sensory-scaling: transform all values between 0.0 and 1.0
;; saliency: smallest distance between perceived values of the topic, and the other segments
