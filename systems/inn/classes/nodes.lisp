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

;; See example of how to use a helper macro for writing custom inn-node code
;; at the end of this file.

;; -------------------------------------------------------------------------
;; 1. Visual Identity of an INN-node
;; -------------------------------------------------------------------------
(export '(get-node-color
          get-node-shape))

;; Color of nodes.
;; ---------------
(defgeneric get-node-color (type))

(defmethod get-node-color ((type (eql :narrative-question)))
  "red")

(defmethod get-node-color ((type (eql :open-narrative-question)))
  "red")

(defmethod get-node-color ((type (eql :answered-narrative-question)))
  "green")

(defmethod get-node-color ((type (eql :entity)))
  "cyan")

(defmethod get-node-color ((type (eql :predicate)))
  "purple")

(defmethod get-node-color ((type t))
  "gray")

;; Shape of nodes.
;; ---------------
(defgeneric get-node-shape (type))

(defmethod get-node-shape ((type (eql :narrative-question)))
  "diamond")

(defmethod get-node-shape ((type (eql :answered-narrative-question)))
  "diamond")

(defmethod get-node-shape ((type (eql :open-narrative-question)))
  "diamond")

(defmethod get-node-shape ((type (eql :entity)))
  "circle")

(defmethod get-node-shape ((type (eql :predicate)))
  "triangle")

(defmethod get-node-shape ((type (eql :inn-image)))
  "image")

(defmethod get-node-shape ((type t))
  "square")

;; -------------------------------------------------------------------------
;; 2. Struct definitions, associated methods, and Constructor Functions
;; -------------------------------------------------------------------------

;; (a) INN-node
;; -------------------------------------------------------------------------
;;
;; The defstruct inn-node is the base node "class" (actually a structure).
;; A helper function #'make-inn-node is used for ensuring all the "subclasses"
;; will call the correct methods for initializing a node's color and shape.

(export '(inn-node 
          make-inn-node
          inn-node-cluster-ids
          inn-node-p
          inn-node-label inn-node-color inn-node-shape inn-node-description
          inn-node-type inn-node-attributes inn-node-id
          get-inn-node-cluster-id get-inn-node-attribute-value))

;; Inn-nodes "inherit" from the node-struct from graph-utils.
;; They are therefore structs as well.
(defstruct (inn-node
            (:constructor %make-inn-node)
            (:include graph-utils::node (id (graph-utils::next-node-id))))
  "Type (:entity, :predicate, T or other values):"
  (label "")
  cluster-ids ; Put here information relevant for clustering nodes.
  color
  shape
  (description "No description available.")
  (type t)
  attributes) ; Put here information that should not impact the visualization.

(defmethod get-node-color ((node inn-node))
  (get-node-color (inn-node-type node)))

(defmethod get-node-shape ((type inn-node))
  (get-node-shape (type-of type)))

(defun make-inn-node (&rest parameters 
                            &key (node-class 'inn-node) 
                            &allow-other-keys)
  "Supporting 'constructor' function"
  (let* ((package (package-name (symbol-package node-class)))
         (constructor-fn (read-from-string 
                          (format nil "~a::%make-~a" package node-class))))
    (remf parameters :node-class)
    (let* ((node (apply constructor-fn parameters))
           (type (inn-node-type node)))
      (unless (member :color parameters)
        (setf (inn-node-color node) (get-node-color type)))
      (unless (member :shape parameters)
        (setf (inn-node-shape node) (get-node-shape type)))
      node)))

(defun make-entity-node (&rest parameters)
  (apply 'make-inn-node `(:type :entity
                          ,@parameters)))
;; (make-entity-node)

(defmacro make-predicate-node (&rest parameters)
  (apply 'make-inn-node `(:type :entity
                          ,@parameters)))
;; (make-predicate-node)

(defun get-inn-node-attribute-value (inn-node attribute)
  (rest (assoc attribute (inn-node-attributes inn-node))))
;; (get-inn-node-attribute-value (make-inn-node :attributes '((:attribute . value))) :attribute)

(defun get-inn-node-cluster-id (inn-node cluster-id)
  (rest (assoc cluster-id (inn-node-cluster-ids inn-node))))
;; (get-inn-node-cluster-id (make-inn-node :cluster-ids '((:cid . 1))) :cid)

;; (b) The DEF-INN-NODE macro
;; -------------------------------------------------------------------------
;;
;; Used for defining new node "classes" that subclass from another inn-node.
;; The macro will ensure that new kinds of nodes will be fully compatible 
;; with the inn code.

(export '(def-inn-node))

(defmacro def-inn-node (name parent &rest body)
  (let ((constructor-fn (read-from-string (format nil "%make-~a" name))))
    `(progn
       (defstruct (,name
                   (:constructor ,constructor-fn)
                   (:include ,@parent))
         ,@body)

       (defun ,(read-from-string (format nil "make-~a" name))
              (&rest parameters)
         (apply 'make-inn-node `(,@parameters
                                 :node-class ,',name))))))

;; (c) narrative-question
;; -------------------------------------------------------------------------
(export '(posed-by answered-by inn-answer
          narrative-question
          narrative-question-p
          narrative-question-id
          narrative-question-label
          narrative-question-description
          narrative-question-shape
          narrative-question-color
          narrative-question-posed-by
          narrative-question-answered-by
          narrative-question-answer
          narrative-question-bindings
          narrative-question-irl-programs
          inn-answer-question undo-inn-answer-question))

(def-inn-node narrative-question (inn-node 
                                  (type :open-narrative-question))
              "Type (:open-narrative-question or :answered-narrative-question):"
              posed-by ;; The knowledge source or cognitive system that 
                       ;; introduced a question
              answered-by ;; The knowledge source or system that answered a question
              answer ;; The ID of the node that represents the answer to the question.
              bindings ;;  Bindings for variables used in the narrative question 
              irl-programs) ;; Possible  IRL programs in which the question may appear

(export '(make-narrative-question
          make-open-narrative-question
          make-answered-narrative-question
          inn-answer-question
          undo-inn-answer-question))

(defun make-open-narrative-question (&rest parameters)
  (apply 'make-narrative-question parameters))
;; (make-open-narrative-question)

(defun make-answered-narrative-question (&rest parameters)
  (apply 'make-narrative-question `(:type :answered-narrative-question
                                    ,@parameters)))

(defun undo-inn-answer-question (narrative-question)
  (setf (narrative-question-type narrative-question) 
        :open-narrative-question
        (narrative-question-shape narrative-question) 
        (get-node-shape 
         :open-narrative-question)
        (narrative-question-color narrative-question) 
        (get-node-color :open-narrative-question)
        (narrative-question-answer narrative-question) nil
        (narrative-question-answered-by narrative-question) nil)
  (vis-update-node (inn-format-node narrative-question))
  narrative-question)

(defun inn-answer-question (narrative-question
                            &key answered-by answer)
  (setf (narrative-question-type narrative-question) 
        :answered-narrative-question
        (narrative-question-shape narrative-question) 
        (get-node-shape :answered-narrative-question)
        (narrative-question-color narrative-question) 
        (get-node-color :answered-narrative-question)
        (narrative-question-answer narrative-question) answer
        (narrative-question-answered-by narrative-question) answered-by)
  (vis-update-node (inn-format-node narrative-question))
  narrative-question)

;; (d) inn-image
;; -------------------------------------------------------------------------

(export '(inn-image
          inn-image-attributes inn-image-cluster-ids inn-image-color
          inn-image-shape inn-image-url inn-image-p inn-image-id
          inn-image-description inn-image-label inn-image-p
          inn-image-type inn-image-value inn-image-weight
          make-inn-image))

(def-inn-node inn-image (inn-node 
                         (type :inn-image))
              "Type (:INN-IMAGE):"
              (url "http://via.placeholder.com/640x360.png"))

(export '(inn-node-structures get-inn-node-constructor))

;; (d) Getting an overview of which inn-node structures exist
;; -------------------------------------------------------------------------
;;
;; These are helper functions mainly used for the web interface.

(defun inn-get-all-subclasses (list-of-classes)
  (if (null list-of-classes)
    nil
    (cons (first list-of-classes) 
          (inn-get-all-subclasses
           (append (rest list-of-classes)
                   (clos:class-direct-subclasses (first list-of-classes)))))))

(defun inn-node-structures ()
  "Return the list of inn-node structures."
  (let ((class (find-class 'inn-node)))
    (inn-get-all-subclasses (list class))))

(defun get-structure-descriptor (class)
  (let ((the-class (if (symbolp class) (find-class class) class)))
    (slot-value the-class 'clos::wrapper)))

(defun get-inn-node-constructor (class)
  (svref (get-structure-descriptor class) 13))
;; (get-inn-node-constructor 'inn-node)

(defun get-inn-node-slot-descriptors 
       (class 
        &optional (the-ignorable '(graph-utils::value 
                                   graph-utils::weight
                                   graph-utils::id
                                   type label description
                                   color shape attributes
                                   cluster-ids)))
  (let ((slot-descriptors (svref (get-structure-descriptor class) 11)))
    (loop for slot-descriptor in slot-descriptors
          for name = (slot-value slot-descriptor 'structure::name)
          unless (member name the-ignorable)
            collect 
              (list name 
                    (slot-value slot-descriptor 'structure::default)))))

;;; Deprecated: helper macro
;;;
;;; ;; -------------------------------------------------------------------------
;;; ;; Helper Macro for writing customized inn-node-code.
;;; ;; -------------------------------------------------------------------------
;;; ;;
;;; ;; The following macro writes the necessary code (defstructs, constructor functions,
;;; ;; and defmethods) for defining a custom inn-node with additional slots.
;;; ;; --------------------------------------------------------------------------
;;; (export '(define-inn-node))

;;; (defmacro define-inn-node (name &key (include :inn-node) (stream t) (package :inn) slots type color shape)
;;;   `(progn
;;;      (format ,stream "~%~%(in-package :~(~a~))~%~%" ,package)
;;;      (format ,stream "(defstruct (~(~a~) (:include ~a)" ',name ,include)
;;;      (format ,stream "~%                  (:constructor make-~(~a~)-constructor))" ',name)
;;;      (format ,stream "~%  \"Type (:~a):\"" ',(or type name))
;;;      (format ,stream "~%  ~{~(~a~)~^ ~})~%~%" ',slots)
;;;      (format ,stream "(defun make-~(~a~) (&rest parameters" ',name)
;;;      (format ,stream "~%                         &key &allow-other-keys)")
;;;      (format ,stream "~%  (destructuring-bind (&whole whole")
;;;      (format ,stream "~%                              &key (constructor 'make-~(~a~)-constructor)" ',name)
;;;      (format ,stream "~%                                   (type :~(~a~))" ',(or type name))
;;;      (format ,stream "~%                                   &allow-other-keys)")
;;;      (format ,stream "~%      parameters")
;;;      (format ,stream "~%    (dolist (indicator '(:constructor :type))")
;;;      (format ,stream "~%      (remf whole indicator))")
;;;      (format ,stream "~%    (apply 'make-inn-node `(:constructor ,constructor :type ,type ,@whole))))~%~%")
;;;      ,@(if color
;;;          `((format ,stream "(defmethod get-node-color ((type (eql :~(~a~))))" ',(or type name))
;;;            (format ,stream "~%  ~s)~%~%" ,color)))
;;;      ,@(if shape
;;;          `((format ,stream "(defmethod get-node-shape ((type (eql :~(~a~))))" ',(or type name))
;;;            (format ,stream "~%  ~s)~%~%" ,shape)))
;;;      (format ,stream "(defmethod inn-format-node ((node ~a))" ',name)
;;;      (format ,stream "~%  (call-next-method)) ;; please customize")))

#|
Example (check output buffer):
------------------------------
(define-inn-node inn-image :slots (url) :type image :color "orange" :shape "square")

Example writing to a file:
--------------------------
(with-open-file (stream (babel-pathname :name "test" :type "lisp") 
                        :direction :output
                        :if-exists :supersede)
  (define-inn-node inn-image 
                   :slots (url size)
                   :stream stream 
                   :color "blue" 
                   :shape "square"))
|#
