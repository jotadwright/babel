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

(export '(integrative-narrative-network
          id bound-variables update-nodes update-edges
          make-inn))

(defclass integrative-narrative-network (network)
  ((id :documentation "The ID of the network."
       :type symbol
       :initform (gensym "network")
       :initarg :id
       :accessor id)
   (bound-variables :documentation "A list of bound variables."
                    :type hash-table
                    :initform (make-hash-table)
                    :initarg  :bound-variables
                    :accessor bound-variables)
   (update-nodes :documentation "Nodes to be updated."
                 :type hash-table
                 :initform (make-hash-table)
                 :initarg  :update-nodes
                 :accessor update-nodes)
   (update-edges :documentation "Edges to be updated."
                 :type hash-table
                 :initform (make-hash-table)
                 :initarg  :update-edges
                 :accessor update-edges))
  (:documentation "Base class for integrative narrative networks."))

(defmacro make-inn (&rest keys-and-values)
  `(make-instance 'integrative-narrative-network
                  ,@keys-and-values))
