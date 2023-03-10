;; Copyright AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

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

(in-package :inn)

(export '(inn-format-node))

(defun inn-format-node (node) ;;(id &key type label (opacity 1.0))
  "Format a node."
  (let ((shape (inn-shape node))
        (color (inn-color node))
        (label (inn-label node)))
    (if label 
      (format nil "{ id: '~a', label: '~a', shape: '~a', color: '~a'}" id label shape color)
      (format nil "{ id: '~a', shape: '~a', color: '~a'}" id shape color))))
