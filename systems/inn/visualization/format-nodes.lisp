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
(export '(inn-format-node inn-format-nodes))

(defgeneric inn-format-node (inn-node))

(defmethod inn-format-node ((id integer))
  (format nil "{ id: '~a'}" id))

(defmethod inn-format-node ((node inn-node))
  (let ((label (inn-node-label node))
        (cluster-ids (inn-node-cluster-ids node)))
    (format nil "{ ~{~a~} }"
            `(,(format nil "id: '~a', " (inn-node-id node))
              ,(format nil "label: '~a', " label "")
              ,(format nil "color: '~a', " (inn-node-color node))
              ,(format nil "shape: '~a', " (inn-node-shape node))
              ,(if cluster-ids
                 (format nil "~{~a~^, ~}"
                         (loop for key-value in cluster-ids
                              collect (format nil "~a: '~a'"
                                              (first key-value)
                                              (second key-value))))
                 "")))))
;; (inn-format-node (make-inn-node))
;; (inn-format-node (make-narrative-question))

(defmethod inn-format-node ((node narrative-question))
  (call-next-method))

(defmethod inn-format-node ((node t))
  (error (format nil "The object of type ~a is not an inn-node." 
                 (type-of node))))
;; (inn-format-node (make-instance 'integrative-narrative-network))

(defun inn-format-nodes (nodes)
  (mapcar #'inn-format-node nodes))
