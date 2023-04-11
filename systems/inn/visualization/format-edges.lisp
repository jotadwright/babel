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

(export '(inn-format-edge))

(defgeneric inn-format-edge (node1 node2 &key &allow-other-keys))

(defmethod inn-format-edge ((node1 integer) 
                            (node2 integer)
                            &key (id (gensym "edge")) label &allow-other-keys)
  (let ((formatted-edge (if label
                          (format nil "{ id: '~a', from: '~a', to: '~a', label: '~a' }"
                                  id node1 node2 label)
                          (format nil "{ id: '~a', from: '~a', to: '~a' }"
                                  id node1 node2))))
    (values formatted-edge (list node1 node2))))
;; (inn-format-edge 1 2)
;; (inn-format-edge 1 2 :label "test")

(defmethod inn-format-edge ((node1 inn-node)
                            (node2 inn-node)
                            &key (id (gensym "edge")) label &allow-other-keys)
  (inn-format-edge (inn-node-id node1) (inn-node-id node2)
                   :id id
                   :label label))
;; (inn-format-edge (make-inn-node) (make-inn-node))