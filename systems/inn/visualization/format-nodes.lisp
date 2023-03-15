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

;; For formatting nodes in a string that is understood by vis.js.

(export '(inn-format-node))

(defgeneric inn-format-node (inn-node))

(defmethod inn-format-node ((node inn-node))
  (let ((id (graph-utils::node-id node))
        (color (inn-node-color node))
        (shape (inn-node-shape node))
        (label (inn-node-label node)))
    (if label 
      (format nil "{ id: '~a', label: '~a', shape: '~a', color: '~a'}" id label shape color)
      (format nil "{ id: '~a', shape: '~a', color: '~a'}" id shape color))))
;; (inn-format-node (make-inn-node))
;; (inn-format-node (make-narrative-question))

(defmethod inn-format-node ((node t))
  (error (format nil "The object of type ~a is not an inn-node." (type-of node))))
;; (inn-format-node (make-instance 'integrative-narrative-network))
