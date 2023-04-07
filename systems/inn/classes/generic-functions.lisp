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

;; ---------------------------------------------------------------------------------
;; See /systems/inn/network-management/update-network.lisp for the methods.
;; ---------------------------------------------------------------------------------

(export '(question-answered?))

(defgeneric question-answered? (graph question &key &allow-other-keys))

;; ----------------------------------------------------------------------------------
;; 1. Edges
;; ----------------------------------------------------------------------------------

(export '(inn-add-edge
          inn-add-edges
          inn-delete-edge
          inn-delete-edges))

(defgeneric inn-add-edge (graph node1 node2 &key weight edge-type &allow-other-keys))
(defgeneric inn-add-edges (graph edges &key weight edge-type &allow-other-keys))
(defgeneric inn-delete-edge (graph node1 node2 &optional edge-type))
(defgeneric inn-delete-edges (graph edges &optional edge-type))

;; ----------------------------------------------------------------------------------
;; 2. Nodes
;; ----------------------------------------------------------------------------------

(export '(inn-add-node
          inn-add-nodes
          inn-delete-node
          inn-delete-nodes))

(defgeneric inn-add-node (graph node &key &allow-other-keys))
(defgeneric inn-add-nodes (inn list-of-nodes &key &allow-other-keys))
(defgeneric inn-delete-node (graph node &key &allow-other-keys))
(defgeneric inn-delete-nodes (graph nodes &key &allow-other-keys))


;; ----------------------------------------------------------------------------------
;; 3. Events
;; ----------------------------------------------------------------------------------

(export '(inn-double-click
          inn-right-click))

;; Customize the event behaviors based on your network class.
(defgeneric inn-double-click (selection network))
(defgeneric inn-right-click (network))

;;
;; 4. Find nodes by label and add edges
;; ----------------------------------------------------------------------------------

(defun inn-find-node-by-label (network label)
  (loop for node being each hash-value of (graph-utils::ids network)
        when (string= label (inn-node-label node))
          return node))
; test
; (inn-node-id (inn-find-node-by-label (get-current-inn) "Who?"))

(defun inn-add-edges-to-node (from-node-label to-nodes-list)
  "adds edges between a given starting node and a list of nodes"
  (let* ((from-node-id (inn-node-id (inn-find-node-by-label (get-current-inn) from-node-label)))
         (network (get-current-inn))
         (node-ids (loop for node in to-nodes-list
                         collect (inn-add-node network node))))
    (loop for to-node-id in node-ids
          do (inn-add-edge network from-node-id to-node-id))))


(defun inn-add-edges-by-nodes-label (from-node-label to-node-label)
  "add an edge between the labels of two nodes"
    (let* ((network (get-current-inn))
           (from-id (inn-find-node-by-label network from-node-label))
           (to-id (inn-find-node-by-label network to-node-label)))
  (inn-add-edge network from-id to-id)))

;(add-edges-by-nodes-label "Material?" "Art Movement?")

(defun inn-answer-question-by-adding-edge (from-node-label to-node-label)
  "add an edge between the labels of two nodes and mark the narrative quesiton as answered"
    (let* ((network (get-current-inn))
           (from-id (inn-find-node-by-label network from-node-label))
           (to-id (inn-find-node-by-label network to-node-label)))
  (inn-add-edge network from-id to-id) 
  (inn-answer-question from-id :answer to-id)))

(defun inn-answer-question-by-adding-edge-and-nodes (from-node-label nodes-labels-list source-type)
  "add an edge between the labels of the node and the list of nodes and mark the narrative quesiton as answered"
    (let* ((network (get-current-inn))
           (from-id (inn-node-id(inn-find-node-by-label network from-node-label)))
            (target-ids (loop for detail in nodes-labels-list
                          collect (inn-add-node network (make-inn-node :label detail :type source-type)))))
      (loop for target-id in target-ids
            do (inn-add-edge network from-id target-id))
       (loop for target-id in target-ids
             do (inn-answer-question from-id :answer target-id))))


(defun inn-add-label-edge-from-nodes-label (from-node-label to-node-label edge-label)
  (let* ((network (get-current-inn))
         (from-id (inn-find-node-by-label network from-node-label))
         (to-id (inn-find-node-by-label network to-node-label)))
    (inn-add-edge network from-id to-id :label edge-label)))


(defun inn-add-label-edge-and-node (from-node-label to-node-label edge-label source-type)
  (let* ((network (get-current-inn))
         (from-id (inn-node-id(inn-find-node-by-label network from-node-label)))
         (to-id  (inn-add-node network (make-inn-node :label to-node-label :type source-type))))
    (inn-add-edge network from-id to-id :label edge-label)))

; (inn-add-label-edge-and-node "When?" "1530" "inception" :kb-wikidata)


(defun inn-add-narrative-question-and-answer (from-node-label narrative-question-label nodes-labels-list source-type)
  "To insert a new narrative question, link it to an existing node, and solve it by adding a list of nodes colored according to their source" 
(let ((art-network (get-current-inn)))
  (let ((question-node-id (inn-add-node art-network (make-answered-narrative-question :label narrative-question-label)))
        (target-ids (loop for detail in nodes-labels-list
                          collect (inn-add-node art-network (make-inn-node :label detail :type source-type))))
        (from-node-id (inn-node-id (inn-find-node-by-label art-network from-node-label))))
    (inn-add-edge art-network from-node-id question-node-id)
    (loop for target-id in target-ids
          do (inn-add-edge art-network question-node-id target-id)))))

;(inn-add-narrative-question-and-answer "Why?" "Commissioned by?" '("Unknown") :kb-wikidata)

