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

(defmethod question-answered? ((graph integrative-narrative-network)
                               (node t)
                               &key &allow-other-keys)
  (declare (ignore graph  node))
  nil)

(defmethod question-answered? ((graph integrative-narrative-network)
                               (question narrative-question)
                               &key answered-by answer &allow-other-keys)
  (cond ((and (eql :open-narrative-question (narrative-question-type question))
              (> (length 
                  (graph-utils::neighbors graph (narrative-question-id question)))
                 1))
         (inn-answer-question question 
                              :answered-by answered-by
                              :answer answer))
        ((and (eql :answered-narrative-question (narrative-question-type question))
           (< (length 
               (graph-utils::neighbors graph (narrative-question-id question)))
              2))
         (undo-inn-answer-question question))
        (t
         nil)))

(defmethod question-answered? ((graph integrative-narrative-network)
                               (node-id integer)
                               &key &allow-other-keys)
  (let ((node (lookup-node graph node-id)))
    (question-answered? graph node)))

;; ----------------------------------------------------------------------------------
;; 1. Edges
;; ----------------------------------------------------------------------------------

(defmethod inn-add-edge ((graph integrative-narrative-network)
                         (node1 integer)
                         (node2 integer)
                         &key (weight 1) edge-type label  &allow-other-keys)
  (unless (or (= node1 node2)
              (graph-utils:edge-exists? graph node1 node2 :edge-type edge-type))
    (let* ((edge-id-symbol (gensym "edge"))
           (edge-id (format nil "~(~a~)" (symbol-name edge-id-symbol)))
           (vis-edge (inn-format-edge node1 node2 :id edge-id :label label)))
      (setf (gethash (list node1 node2) (vis-edges graph)) vis-edge
            (gethash (list node2 node1) (vis-edges graph)) vis-edge)
      (wi:vis-add-edge vis-edge)
      (graph-utils::add-edge graph node1 node2 :weight weight :edge-type edge-type))))

(defmethod inn-add-edge ((graph integrative-narrative-network)
                         (node1 inn-node)
                         (node2 inn-node)
                         &key (weight 1) edge-type &allow-other-keys)
  (inn-add-edge graph (inn-node-id node1) (inn-node-id node2) 
                :weight weight :edge-type edge-type))

(defmethod inn-add-edges ((graph integrative-narrative-network)
                          (edges list)
                          &key (weight 1) edge-type &allow-other-keys)
  (dolist (edge edges graph)
    (inn-add-edge graph (first edge) (second edge)
                  :weight weight 
                  :edge-type edge-type)))

(defmethod inn-delete-edge ((graph integrative-narrative-network)
                            (n1 integer)
                            (n2 integer)
                            &optional edge-type)
  (let ((vis-edge (or (gethash (list n1 n2) (vis-edges graph))
                      (gethash (list n2 n1) (vis-edges graph)))))
    ;; Remove the edge from the web interface
    (vis-remove-edge vis-edge)
    ;; Remove edge information from the narrative-network
    (remhash (list n1 n2) (vis-edges graph))
    (remhash (list n2 n1) (vis-edges graph))
    ;; Remove the edge from the inn:
    (graph-utils::delete-edge graph n1 n2 edge-type)
    ;; Check whether its from and to nodes require updating:
    (question-answered? graph n1)
    (question-answered? graph n2)))

(defmethod inn-delete-edge ((graph integrative-narrative-network)
                            (node1 inn-node)
                            (node2 inn-node)
                            &optional edge-type)
  (inn-delete-edge graph (inn-node-id node1) (inn-node-id node2) edge-type))

(defmethod inn-delete-edges ((graph integrative-narrative-network)
                             (edges list)
                             &optional edge-type)
  (dolist (edge edges graph)
    (inn-delete-edge graph (first edge) (second edge) edge-type)))

;; ----------------------------------------------------------------------------------
;; 2. Nodes
;; ----------------------------------------------------------------------------------

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

(defmethod inn-add-nodes ((graph integrative-narrative-network)
                          (nodes list)
                          &key &allow-other-keys)
  (dolist (node nodes graph)
    (inn-add-node graph node)))
  
(defmethod inn-delete-node ((graph integrative-narrative-network)
                            (id integer)
                            &key &allow-other-keys)
  (wi:vis-remove-node (inn:inn-format-node id))
  (let ((neighbours (graph-utils::neighbors graph id))) ;; Get the id of the neighbours
    ;; Remove relevant edges as well:
    (dolist (neighbour neighbours)
      (let ((direction1 (list id neighbour))
            (direction2 (list neighbour id)))
        (remhash direction1 (vis-edges graph))
        (remhash direction2 (vis-edges graph))))
    ;; Delete the node
    (graph-utils::delete-node graph id)
    ;; Now check whether questions become open again:
    (dolist (neighbour neighbours)
      (question-answered? graph neighbour))))

(defmethod inn-delete-node ((graph integrative-narrative-network)
                            (node inn-node)
                             &key &allow-other-keys)
  (inn-delete-node graph (inn-node-id node)))

(defmethod inn-delete-nodes ((graph integrative-narrative-network)
                             (nodes list)
                             &key &allow-other-keys)
  (dolist (node nodes graph)
    (inn-delete-node graph node)))

