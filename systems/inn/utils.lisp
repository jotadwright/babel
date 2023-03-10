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

(defun cons-new (e lst)
  (if (member e lst)
    lst
    (cons e lst)))
;; (cons-new 'a  '(a b))
;; (cons-new 'a '(b))

(defun get-object-id (object)
  "Object-id function from older version."
  (cond ((null object) (make-var 'question))
        ((eql 'none object) (make-const 'none))
        ((or (symbolp object) (numberp object)) object)
        ((eql 'binding (type-of object)) (slot-value object 'variable))
        (t
         (persistent-id object))))

(defun get-slot-names (clos-object)
  "Retrieve the slot names associated with the class of a clos-object."
  (mapcar #'harlequin-common-lisp:slot-definition-name
          (harlequin-common-lisp:class-slots (class-of clos-object))))