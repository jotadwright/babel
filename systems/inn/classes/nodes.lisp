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

(export '(inn-node 
          make-inn-node
          inn-node-cluster-ids
          inn-node-p
          inn-node-label inn-node-color inn-node-shape inn-node-description
          inn-node-type inn-node-attributes inn-node-id))

;; Inn-nodes "inherit" from the node-struct from graph-utils.
;; They are therefore structs as well.
(defstruct (inn-node
            (:constructor make-inn-node-constructor)
            (:include graph-utils::node))
  "Type (:entity, :predicate, T or other values):"
  (label "")
  cluster-ids ; can be used for clustering nodes.
  color
  shape
  (description "No description available.")
  (type t)
  attributes)

(defmethod get-node-color ((node inn-node))
  (get-node-color (inn-node-type node)))

(defmethod get-node-shape ((type inn-node))
  (get-node-shape (type-of type)))

;; Customized constructor function.
(defun make-inn-node (&rest parameters
                            &key &allow-other-keys)
  (destructuring-bind (&whole whole
                              &key (constructor 'make-inn-node-constructor)
                              (type t)
                              (id (graph-utils::next-node-id))
                              &allow-other-keys)
      parameters
    ;; Remove them from the parameters list, also remove color or shape
    ;; (indeed: you CANNOT manually override color and shape. You need to 
    ;;  use the dedicated defmethods for that!)
    (dolist (indicator '(:constructor :id :type :color :shape))
      (remf whole indicator))
    ;; Now apply the constructor. It is impossible to 
    (apply constructor `(:id ,id
                         :type ,type
                         :color ,(get-node-color type)
                         :shape ,(get-node-shape type)
                         ,@whole))))
;; (make-inn-node :type 'answered-narrative-question)

(export '(make-entity-node make-predicate-node))

(defmacro make-entity-node (&rest parameters)
  `(make-inn-node ,@parameters
                  :type :entity))

(defmacro make-predicate-node (&rest parameters)
  `(make-inn-node ,@parameters
                  :type :predicate))

;; (b) narrative-question
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

(defstruct (narrative-question 
            (:include inn-node)
            (:constructor make-narrative-question-constructor))
  "Type (:open-narrative-question or :answered-narrative-question):"
  posed-by ;; The knowledge source or cognitive system that introduced a question
  answered-by ;; The knowledge source or system that answered a question
  answer ;; The ID of the node that represents the answer to the question.
  bindings ;;  Bindings for variables used in the narrative question 
  irl-programs) ;; Possible  IRL programs in which the question may appear

(export '(make-narrative-question
          make-open-narrative-question
          make-answered-narrative-question
          inn-answer-question))

(defun make-narrative-question (&rest parameters
                                      &key &allow-other-keys)
  (destructuring-bind (&whole whole
                              &key (constructor 'make-narrative-question-constructor)
                              (type :open-narrative-question)
                              &allow-other-keys)
      parameters
    ;; Remove the constructor and type from the parameters
    (dolist (indicator '(:constructor :type))
      (remf whole indicator))
    ;; Now call the basic constructor function
    (apply 'make-inn-node 
           `(:constructor ,constructor
             :type ,(if (keywordp type) type :open-narrative-question)
             ,@whole))))
;; (make-narrative-question)

(defmacro make-open-narrative-question (&rest parameters)
  `(make-narrative-question ,@parameters
                            :type :open-narrative-question))

(defmacro make-answered-narrative-question (&rest parameters)
  `(make-narrative-question ,@parameters
                            :type :answered-narrative-question))

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

;; (c) inn-image
;; -------------------------------------------------------------------------

(export '(inn-image
          inn-image-attributes inn-image-cluster-ids inn-image-color
          inn-image-shape inn-image-url inn-image-p inn-image-id
          inn-image-description inn-image-label inn-image-p
          inn-image-type inn-image-value inn-image-weight
          make-inn-image))

(defstruct (inn-image (:include INN-NODE)
                  (:constructor make-inn-image-constructor))
  "Type (:INN-IMAGE):"
  (url "http://via.placeholder.com/640x360.png")) ; Customized to have placeholder. 

(defun make-inn-image (&rest parameters
                         &key &allow-other-keys)
  (destructuring-bind (&whole whole
                              &key (constructor 'make-inn-image-constructor)
                                   (type :inn-image)
                                   &allow-other-keys)
      parameters
    (dolist (indicator '(:constructor :type))
      (remf whole indicator))
    (apply 'make-inn-node `(:constructor ,constructor 
                            :type ,(if (keywordp type) type :inn-image)
                            ,@whole))))
;; (make-inn-image)

(export '(inn-node-structures get-inn-node-constructor))

(defun inn-node-structures ()
  "Return the list of inn-node structures."
  (let ((class (find-class 'inn-node)))
    (labels ((inn-get-all-subclasses (list-of-classes)
               (if (null list-of-classes)
                 nil
                 (cons (first list-of-classes)
                       (append (clos:class-direct-subclasses (first list-of-classes))
                               (inn-get-all-subclasses (rest list-of-classes)))))))
      (inn-get-all-subclasses (list class)))))

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

                          
;; -------------------------------------------------------------------------
;; Helper Macro for writing customized inn-node-code.
;; -------------------------------------------------------------------------
;;
;; The following macro writes the necessary code (defstructs, constructor functions,
;; and defmethods) for defining a custom inn-node with additional slots.
;; --------------------------------------------------------------------------
(export '(define-inn-node))

(defmacro define-inn-node (name &key (include :inn-node) (stream t) (package :inn) slots type color shape)
  `(progn
     (format ,stream "~%~%(in-package :~(~a~))~%~%" ,package)
     (format ,stream "(defstruct (~(~a~) (:include ~a)" ',name ,include)
     (format ,stream "~%                  (:constructor make-~(~a~)-constructor))" ',name)
     (format ,stream "~%  \"Type (:~a):\"" ',(or type name))
     (format ,stream "~%  ~{~(~a~)~^ ~})~%~%" ',slots)
     (format ,stream "(defun make-~(~a~) (&rest parameters" ',name)
     (format ,stream "~%                         &key &allow-other-keys)")
     (format ,stream "~%  (destructuring-bind (&whole whole")
     (format ,stream "~%                              &key (constructor 'make-~(~a~)-constructor)" ',name)
     (format ,stream "~%                                   (type :~(~a~))" ',(or type name))
     (format ,stream "~%                                   &allow-other-keys)")
     (format ,stream "~%      parameters")
     (format ,stream "~%    (dolist (indicator '(:constructor :type))")
     (format ,stream "~%      (remf whole indicator))")
     (format ,stream "~%    (apply 'make-inn-node `(:constructor ,constructor :type ,type ,@whole))))~%~%")
     ,@(if color
         `((format ,stream "(defmethod get-node-color ((type (eql :~(~a~))))" ',(or type name))
           (format ,stream "~%  ~s)~%~%" ,color)))
     ,@(if shape
         `((format ,stream "(defmethod get-node-shape ((type (eql :~(~a~))))" ',(or type name))
           (format ,stream "~%  ~s)~%~%" ,shape)))
     (format ,stream "(defmethod inn-format-node ((node ~a))" ',name)
     (format ,stream "~%  (call-next-method)) ;; please customize")))

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
