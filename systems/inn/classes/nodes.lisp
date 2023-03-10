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

;; -------------------------------------------------------------------------
;; Class definition
;; -------------------------------------------------------------------------

(export '(inn-node inn-label inn-color inn-shape inn-type inn-attributes
                   open-question answered-question entity predicate))

(defclass inn-node (net-node)
  ((label :documentation "The label of the node. Need not be unique."
	  :initform nil
	  :initarg :label
	  :accessor inn-label)
   (color :documentation "The color of the node."
          :type string
          :initform "grey"
          :initarg :color
          :accessor inn-color)
   (shape :documentation "The shape of the node."
          :initform "square"
          :initarg :shape
          :accessor inn-shape)
   (type :documentation "The type of a node."
         :type symbol
         :initform t
         :initarg :type
         :accessor inn-type)
   (attributes :documentation "A list of attributes."
               :type list
               :initform nil
               :initarg :attributes
               :accessor inn-attributes))
  (:documentation "The basic node class for Integrative Narrative Networks."))

(defmethod initialize-instance :after ((class inn-node) &key)
  (case (inn-type class)
    (open-question
     (setf (slot-value class 'color) "red"
           (slot-value class 'shape) "diamond"))
    (answered-question
     (setf (slot-value class 'color) "green"
           (slot-value class 'shape) "diamond"))
    (entity
     (setf (slot-value class 'color) "cyan"
           (slot-value class 'shape) "circle"))
    (predicate
     (setf (slot-value class 'color) "purple"
           (slot-value class 'shape) "triangle"))
    (t
     nil)))

(export '(posed-by answered-by inn-answer))

(defclass narrative-question (inn-node)
  ((posed-by :documentation "The knowledge source or cognitive system that has posed the question."
             :initform nil
             :initarg :posed-by
             :accessor posed-by)
   (answered-by :documentation "The knowledge source or cognitive system(s) that have answered the question."
                :initform nil
                :initarg :answered-by
                :accessor answered-by)
   (answer :documentation "The node that represents the answer to the question."
           :initform nil
           :initarg :answer
           :accessor inn-answer))
  (:documentation "A narrative question is a particular node in integrative narrative questions."))

;; -------------------------------------------------------------------------
;; Helper Macros
;; -------------------------------------------------------------------------

(export '(inn-make-open-question inn-make-answered-question
                                 inn-make-entity inn-make-predicate))

;; Helper macros
;; ----------------
(defmacro inn-make-open-question (&rest keys-and-values)
  `(make-instance 'narrative-question
                  :type 'open-question
                  ,@keys-and-values))
;; (inn-make-open-question)

(defmacro inn-make-answered-question (&rest keys-and-values)
  `(make-instance 'narrative-question
                  :type 'answered-question
                  ,@keys-and-values))
;; (inn-make-answered-question)

(defmacro inn-make-entity (&rest keys-and-values)
  `(make-instance 'inn-node
                  :type 'entity
                  ,@keys-and-values))
;; (inn-make-entity :id 'my-entity)

(defmacro inn-make-predicate (&rest keys-and-values)
  `(make-instance 'inn-node
                  :type 'predicate
                  ,@keys-and-values))
;; (inn-make-predicate :label '(push ?ev ?x ?y))