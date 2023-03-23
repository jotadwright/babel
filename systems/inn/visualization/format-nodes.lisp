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

(defun format-cluster-ids (cluster-ids)
  (if cluster-ids
    (format nil ", ~{~(~a~)~^, ~} "
                      (loop for key-value in cluster-ids
                            collect (format nil "~a: '~a'"
                                            (first key-value)
                                            (second key-value))))
    " "))

(defmethod inn-format-node ((node inn-node))
    (format nil "{ id: '~a', label: '~a', color: '~a', shape: '~a'~a}"
            (inn-node-id node)
            (or (inn-node-label node) "")
            (inn-node-color node)
            (inn-node-shape node)
            (format-cluster-ids (inn-node-cluster-ids node))))
;; (inn-format-node (make-inn-node))
;; (inn-format-node (make-inn-node :cluster-ids '((cid "1")(pid "2"))))

(defmethod inn-format-node ((node narrative-question))
  (call-next-method))
;; (inn-format-node (make-narrative-question))

(defmethod inn-format-node ((node inn-image))
  (format nil "{ id: '~a', label: '~a', title: '~a', shape: '~a', image: '~a'~a}"
          (inn-node-id node)
          (or (inn-image-label node) "")
          (or (inn-image-description node) (inn-image-label node) "")
          (or (inn-image-shape node) "image")
          (inn-image-url node)
          (format-cluster-ids (inn-node-cluster-ids node))))
;; (inn-format-node (make-inn-image))
;; (inn-format-node (make-inn-image :cluster-ids '((cid "1")(pid "2"))))

(defmethod inn-format-node ((node t))
  (error (format nil "The object of type ~a is not an inn-node." 
                 (type-of node))))
;; (inn-format-node (make-instance 'integrative-narrative-network))

(defun inn-format-nodes (nodes)
  (mapcar #'inn-format-node nodes))
