;;;;; --------------------------------------------------------------------------------------
;;;;; Copyright: Sony Computer Science Laboratories, Paris Lab
;;;;; Author:    Remi van Trijp
;;;;; 
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

(export '(geom-world geom-scene geom-object
                     geom-circle geom-rectangle geom-square geom-triangle))

(defclass geom-world ()
  ((shapes :initarg shapes :accessor shapes :initform '(circle triangle rectangle square))
   (colors :initarg colors :accessor colors :initform '((255 0  0 "Red")
                                                        (0 139 0 "Green")
                                                        (0 0 255 "Blue")
                                                        (238 238 0 "Yellow"))))
  (:documentation "Inspired by the Talking Heads book."))

(defclass geom-scene ()
  ((area :initarg :area :accessor area)
   (image :initarg :image :accessor image)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (objects :initarg :objects :accessor objects))
  (:documentation "A scene in the geom world."))

(defclass geom-object (entity)
  ((area :initarg :area :accessor area)
   (bounding-box :initarg :bounding-box :accessor bounding-box)
   (hpos :initarg :hpos :accessor hpos) ;; mid-x-position
   (vpos :initarg :vpos :accessor vpos) ;; mid-y-position
   (color :initarg :color :accessor color)
   (edge-count :initarg :edge-count :accessor edge-count :initform 0))
  (:documentation "Base class of all objects in the geom world."))

(defclass geom-circle (geom-object)
  ((radius :initarg :radius :accessor radius))
  (:documentation "A circle object."))

(defclass geom-rectangle (geom-object)
  ((edge-count :initarg :edge-count :accessor edge-count :initform 4))
  (:documentation "A rectangle object."))

(defclass geom-square (geom-rectangle)
  ()
  (:documentation "A square object."))

(defclass geom-triangle (geom-object)
  ((edge-count :initarg :edge-count :accessor edge-count :initform 3)
   (edges :initarg :edges :accessor edges))
  (:documentation "A Triangle object."))

;; H-POS = x-midposition
;; V-POS = y-midposition

;; - Area (surface area of a segment)
;; - HPOS (x-value)
;; - VPOS (y-value)
;; - HEIGHT (height of bounding box)
;; - WIDTH (width of bounding box)
;; - BB-AREA (area of the bounding box)
;; - GRAY (average grey-scale)
;; - R-G-B (average Red, Green, Blue values in a segment)
;; - EDGE-COUNT (number of edges in a segment)
;; - ANGLE-COUNT (number of angles, calculated based on junctions)
;; - RATIO (ratio between the area of the segment and the area of the bb-box)
