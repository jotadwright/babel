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

;; ------------------------------------------------------------------------
;; 1. Grounding the methods in :graph-utils
;; ------------------------------------------------------------------------

(in-package :graph-utils)

(defmethod add-node ((graph inn:integrative-narrative-network)
                     (node inn:inn-node)
                     &key &allow-other-keys)
  (let* ((existing-node-p (lookup-node graph (inn:inn-node-id node)))
         (id (if existing-node-p 
               (inn:inn-node-id node) 
               (setf (inn:inn-node-id node) (incf (last-id graph)))))
         (vis-node (inn:inn-format-node node)))
    ;; If the node already exists:
    (if existing-node-p
      ;; Then we simply update the vis.js-network:
      (wi:vis-update-node vis-node)
      ;; Else we add a new one and expand our inn:
      (progn
        (incf-sarray-dimensions (matrix graph))
        (wi:vis-add-node vis-node)))
    ;; Now store or modify the node in the INN:
    (setf (gethash id (degree-table graph)) 0
          (gethash node (nodes graph)) id
          (gethash id (ids graph)) node)
    id))

(defmethod add-edge ((graph inn:integrative-narrative-network)
                     (n1 integer) 
                     (n2 integer)
                     &key (weight 1) edge-type &allow-other-keys)
  (declare (ignorable weight edge-type))
  (unless (or (= n1 n2)
              (edge-exists? graph n1 n2))
    (wi:vis-add-edge (wi::format-vis-js-edge n1 n2))
    (call-next-method)))

;; ------------------------------------------------------------------------
;; 2. The public INN interface.
;; ------------------------------------------------------------------------

(in-package :inn)

(export '(inn-add-node 
          inn-add-nodes
          inn-delete-node
          inn-delete-nodes
          inn-add-edge
          inn-delet-edge))

(defgeneric inn-add-node (graph node &key &allow-other-keys))

(defmethod inn-add-node ((graph integrative-narrative-network)
                         (node inn-node)
                         &key &allow-other-keys)
  (graph-utils:add-node graph node))

(defgeneric inn-add-nodes (inn list-of-nodes &key &allow-other-keys))

(defmethod inn-add-nodes ((graph integrative-narrative-network)
                          (nodes list)
                          &key &allow-other-keys)
  (dolist (node nodes graph)
    (inn-add-node graph node)))
  
(defgeneric inn-delete-node (graph node &key &allow-other-keys))

(defmethod inn-delete-node ((graph integrative-narrative-network)
                            (id integer)
                            &key &allow-other-keys)
  (wi:vis-remove-node (inn:inn-format-node id))
  (graph-utils::delete-node graph id))

(defmethod inn-delete-node ((graph integrative-narrative-network)
                            (node inn-node)
                             &key &allow-other-keys)
  (inn-delete-node graph (inn-node-id node)))

(defgeneric inn-delete-nodes (graph nodes &key &allow-other-keys))

(defmethod inn-delete-nodes ((graph integrative-narrative-network)
                             (nodes list)
                             &key &allow-other-keys)
  (dolist (node nodes graph)
    (inn-delete-node graph node)))

(defgeneric inn-add-edge (graph node1 node2 &key weight edge-type &allow-other-keys))

(defmethod inn-add-edge ((graph integrative-narrative-network)
                         (node1 integer)
                         (node2 integer)
                         &key (weight 1) edge-type &allow-other-keys)
  (graph-utils::add-edge graph node1 node2 :weight weight :edge-type edge-type))

(defmethod inn-add-edge ((graph integrative-narrative-network)
                         (node1 inn-node)
                         (node2 inn-node)
                         &key (weight 1) edge-type &allow-other-keys)
  (inn-add-edge graph (inn-node-id node1) (inn-node-id node2) :weight weight :edge-type edge-type))

(defgeneric inn-add-edges (graph edges &key weight edge-type &allow-other-keys))

(defmethod inn-add-edges ((graph integrative-narrative-network)
                          (edges list)
                          &key (weight 1) edge-type &allow-other-keys)
  (dolist (edge edges graph)
    (inn-add-edge graph (first edge) (second edge)
                  :weight weight 
                  :edge-type edge-type)))

(defgeneric inn-delete-edge (graph node1 node2 &optional edge-type))

(defmethod inn-delete-edge ((graph integrative-narrative-network)
                            (n1 integer)
                            (n2 integer)
                            &optional edge-type)
  (vis-remove-edge (wi::format-vis-js-edge n1 n2))
  (graph-utils::delete-edge graph n1 n2 edge-type))

(defmethod inn-delete-edge ((graph integrative-narrative-network)
                            (node1 inn-node)
                            (node2 inn-node)
                            &optional edge-type)
  (inn-delete-edge graph (inn-node-id node1) (inn-node-id node2) edge-type))

(defgeneric inn-delete-edges (graph edges &optional edge-type))

(defmethod inn-delete-edges ((graph integrative-narrative-network)
                             (edges list)
                             &optional edge-type)
  (dolist (edge edges graph)
    (inn-delete-edge graph (first edge) (second edge) edge-type)))
