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

(export '(inn-add-node 
          inn-update-node
          inn-add-nodes
          inn-delete-node
          inn-delete-nodes
          inn-add-edge
          inn-add-edges
          inn-delete-edge
          inn-delete-edges))

(defgeneric inn-add-node (graph node &key &allow-other-keys))

(defun inn-update-node (graph node)
  ;; Update the vis network:
  (vis-update-node (inn-format-node node))
  ;; Update the INN network:
  (let ((id (inn-node-id node)))
    (setf (gethash id (graph-utils::degree-table graph)) 0
          (gethash node (graph-utils::nodes graph)) id
          (gethash id (graph-utils::ids graph)) node)
    id))

(defmethod inn-add-node ((graph integrative-narrative-network)
                         (node inn-node)
                         &key &allow-other-keys)
  (let ((existing-node? (graph-utils::lookup-node graph (inn-node-id node))))
    (if existing-node? ;; If the node already exists:
      ;; Then update it:
      (inn-update-node graph node)
      ;; Otherwise we add it. We first ensure we have a compatible ID:
      (let ((id (setf (inn-node-id node) (incf (graph-utils::last-id graph)))))
        ;; We add the node to our vis network:
        (vis-add-node (inn-format-node node))
        ;; And we add the node to the INN:
        (graph-utils::incf-sarray-dimensions (graph-utils::matrix graph))
        (setf (gethash id (graph-utils::degree-table graph)) 0
	      (gethash node (graph-utils::nodes graph)) id
	      (gethash id (graph-utils::ids graph)) node)
        id))))

(defmethod add-node ((graph undirected-typed-graph) value &key capacity)
  "Add a node to the graph."
  (or (gethash value (nodes graph))
      (let ((id (incf (last-id graph))))
        (maphash (lambda (etype matrix)
                   (declare (ignore etype))
                   (incf-sarray-dimensions matrix))
                 (matrix graph))
        (when capacity
          (setf (gethash id (node-caps graph)) capacity))
        (setf (gethash id (degree-table graph)) 0
              (gethash value (nodes graph)) id
              (gethash id (ids graph)) value)
        id)))
        (graph-utils:add-node graph node)))))

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
  (unless (or (= node1 node2)
              (graph-utils:edge-exists? graph node1 node2 :edge-type edge-type))
    (let ((vis-edge (inn-format-edge node1 node2 :id (gensym "edge"))))
      (setf (gethash (list node1 node2) (vis-edges graph)) vis-edge
            (gethash (list node2 node1) (vis-edges graph)) vis-edge)
      (wi:vis-add-edge vis-edge)
      (graph-utils::add-edge graph node1 node2 :weight weight :edge-type edge-type))))

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
  (let ((vis-edge (or (gethash (list n1 n2) (vis-edges graph))
                      (gethash (list n2 n1) (vis-edges graph)))))
    (vis-remove-edge vis-edge)
    (remhash (list n1 n2) (vis-edges graph))
    (remhash (list n2 n1) (vis-edges graph))
    (graph-utils::delete-edge graph n1 n2 edge-type)))

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
