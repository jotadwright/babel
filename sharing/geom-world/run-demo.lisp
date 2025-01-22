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

(ql:quickload :geom-world)

(in-package :geom-world)

;; 1. Scene generation and display
;; ------------------------------------------------------------------------
;; Open your browser at "http://localhost:8000"
;; To generate and display a scene:
(progn
  (wi::reset)
  (geom-generate-scene :display? t))

;; Your scene will be saved as a PNG-file. You can evaluate the following
;; to know in which directory to find it:
(geom-image-path nil :type nil)

;; 2. Generating many scenes at once.
;; ------------------------------------------------------------------------
;; Note that this may take some time, because each object is colored by 
;; coloring its surface pixel by pixel. Especially for triangles, this can 
;; be a relatively slow endeavour. A hundred scenes should not take more than 
;; a minute though.
(geom-generate-scenes :number-of-scenes 100
                      :file-prefix 'my-scenes)

;; 3. Using experiment configurations.
;; ------------------------------------------------------------------------
;; By default, a scene has a minimum of 2 and maximum of 5 objects. This can 
;; be modified by using an experiment object from Babel's experiment-framework.

(defparameter *my-experiment* (make-instance 'experiment
                                             :world (make-instance 'geom-world)))

;; Setting the parameters:
(define-configuration-default-value :min-nr-of-objects 7)
(define-configuration-default-value :max-nr-of-objects 7)

;; See the crowded result:
(progn
  (wi::reset)
  (geom-generate-scene :experiment *my-experiment*
                       :display? t))

