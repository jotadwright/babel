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

(export '(geom-image-path))

(defun geom-image-path (name &key (type "png"))
  "Returns the correct path of an image."
  ;; If you want the scenes/images to be saved to a different directory, you need
  ;; to change the body of this function. Importantly:
  ;; 1- The folder of the images MUST be called "geom-images" for proper display
  ;;    in the web interface.
  ;; 2- The function must retain the same call pattern, as it is used in the code.
  ;; 3- The function must return a pathspec.
  (babel-pathname :directory '("sharing" "geom-world" "geom-images")
                  :name name
                  :type type))

(defun base-image-path (name &key (type "png"))
  "Locates the base image."
  (babel-pathname :directory '("sharing" "geom-world")
                  :name (or name "base-image")
                  :type type))

;;;;; --------------------------------------------------------------------------------------
;;;;; Informing the web interface about where to access images and perceived-data
;;;;; --------------------------------------------------------------------------------------

(setf hunchentoot:*dispatch-table*
      (append hunchentoot:*dispatch-table*
              (list (hunchentoot:create-folder-dispatcher-and-handler
                     "/geom-images/"
                     (geom-image-path nil :type nil)))))
