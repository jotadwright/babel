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
;; Visual Identity
;; -------------------------------------------------------------------------

(export '(get-node-color
          get-node-shape
          open-narrative-question answered-narrative-question predicate))

;; Color of nodes.
;; ---------------
(defgeneric get-node-color (type))

(defmethod get-node-color ((type (eql 'open-narrative-question)))
  "red")

(defmethod get-node-color ((type (eql 'answered-narrative-question)))
  "green")

(defmethod get-node-color ((type (eql 'entity)))
  "cyan")

(defmethod get-node-color ((type (eql 'predicate)))
  "purple")

(defmethod get-node-color ((type t))
  "gray")

;; Shape of nodes.
;; ---------------
(defgeneric get-node-shape (type))

(defmethod get-node-shape ((type (eql 'answered-narrative-question)))
  "diamond")

(defmethod get-node-shape ((type (eql 'open-narrative-question)))
  "diamond")

(defmethod get-node-shape ((type (eql 'entity)))
  "circle")

(defmethod get-node-shape ((type (eql 'predicate)))
  "triangle")

(defmethod get-node-shape ((type t))
  "square")

;; -------------------------------------------------------------------------
;; Struct definitions and Constructor Functions
;; -------------------------------------------------------------------------

(export '(inn-node 
          make-inn-node
          inn-node-label inn-node-color inn-node-shape 
          inn-node-type inn-node-attributes inn-node-id))

;; Inn-nodes "inherit" from the node-struct from graph-utils.
;; They are therefore structs as well.
(defstruct (inn-node
            (:constructor make-inn-node-constructor)
            (:include graph-utils::node))
  label
  color
  shape
  (type t)
  attributes)

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

(export '(posed-by answered-by inn-answer
                   narrative-question-posed-by
                   narrative-question-answered-by
                   narrative-question-answer))

(defstruct (narrative-question 
            (:include inn-node)
            (:constructor make-narrative-question-constructor))
  posed-by ;; The knowledge source or cognitive system that introduced a question
  answered-by ;; The knowledge source or system that answered a question
  answer) ;; The ID of the node that represents the answer to the question.

(export '(make-narrative-question
          make-open-narrative-question
          make-answered-narrative-question))

(defun make-narrative-question (&rest parameters
                                      &key &allow-other-keys)
  (destructuring-bind (&whole whole
                              &key (constructor 'make-narrative-question-constructor)
                              (type 'open-narrative-question)
                              &allow-other-keys)
      parameters
    ;; Remove the constructor and type from the parameters
    (dolist (indicator '(:constructor :type))
      (remf whole indicator))
    ;; Now call the basic constructor function
    (apply 'make-inn-node 
           `(:constructor ,constructor
             :type ,type
             ,@whole))))
;; (make-narrative-question)

;; -------------------------------------------------------------------------
;; Helper Macro for writing customized inn-node-code.
;; -------------------------------------------------------------------------
;;
;; The following macro writes the necessary code (defstructs, constructor functions,
;; and defmethods) for defining a custom inn-node with additional slots.
;; --------------------------------------------------------------------------
(export '(define-inn-node))

(defmacro define-inn-node (name &key (stream t) (package :inn) slots type color shape)
  `(progn
     (format ,stream "~%~%(in-package :~(~a~))~%~%" ,package)
     (format ,stream "(defstruct (~(~a~) (:include inn-node)" ',name)
     (format ,stream "~%                  (:constructor make-~(~a~)-constructor))" ',name)
     (format ,stream "~%  ~{~(~a~)~^ ~})~%~%" ',slots)
     (format ,stream "(defun make-~(~a~) (&rest parameters" ',name)
     (format ,stream "~%                         &key &allow-other-keys)")
     (format ,stream "~%  (destructuring-bind (&whole whole")
     (format ,stream "~%                              &key (constructor 'make-~(~a~)-constructor)" ',name)
     (format ,stream "~%                                   (type '~(~a~))" ',(or type name))
     (format ,stream "~%                                   &allow-other-keys)"
     (format ,stream "~%      parameters")
     (format ,stream "~%    (dolist (indicator '(:constructor :type))")
     (format ,stream "~%      (remf whole indicator))")
     (format ,stream "~%    (apply 'make-inn-node `(:constructor ,constructor :type ,type ,@whole))))~%~%")
     ,@(if color
         `((format ,stream "(defmethod get-node-color ((type (eql '~(~a~))))" ',(or type name))
           (format ,stream "~%  ~s)~%~%" ,color)))
     ,@(if shape
         `((format ,stream "(defmethod get-node-shape ((type (eql '~(~a~))))" ',(or type name))
           (format ,stream "~%  ~s)" ,shape)))))

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
