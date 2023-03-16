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

(export '(draw-inn-network-in-vis-js))

(defun collect-vis-edges (inn)
  (let (vis-edges)
    (maphash #'(lambda(key value)
                 (declare (ignore key))
                 (pushnew value vis-edges :test #'string=))
             (vis-edges inn))
    vis-edges))

(defun draw-inn-network-in-vis-js (inn 
                                   &key (destroy-network? t)
                                   (id "integrativeNarrativeNetwork")
                                   (options "interaction: { navigationButtons: true, keyboard: true }"))
  (if destroy-network?
    (vis-destroy-network))
  (let* ((nodes (graph-utils::nodes inn))
         (inn-nodes (graph-utils::ids inn))
         (formatted-nodes (if nodes
                            (loop for key being each hash-key of nodes
                                    using (hash-value value)
                                  collect (inn-format-node (gethash value inn-nodes)))))
         (edges (collect-vis-edges inn)))
    (add-element 
     `((div :id ,id) 
       ,(wi::make-vis-network :element-id id
                              :nodes formatted-nodes
                              :edges edges
                              :options options)))))
