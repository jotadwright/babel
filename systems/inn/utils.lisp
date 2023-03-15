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

(defun get-slot-names (clos-object)
  "Retrieve the slot names associated with the class of a clos-object."
  (mapcar #'harlequin-common-lisp:slot-definition-name
          (harlequin-common-lisp:class-slots (class-of clos-object))))

(export '(inn-object-id))

(defgeneric inn-object-id (object))

(defmethod inn-object-id ((object (eql nil)))
  (make-var 'question))
;; (inn-object-id nil)

(defmethod inn-object-id ((object (eql 'none)))
  (make-const 'none))
;; (inn-object-id 'none)

(defmethod inn-object-id (object)
  object)
;; (inn-object-id 'test)
;; (inn-object-id 14)

(defmethod inn-object-id ((object binding))
  (slot-value object 'variable))
;; (inn-object-id (make-instance 'binding :var (make-var)))

(defun store-network (inn &optional pathname)
  (let ((path (or pathname (merge-pathnames ".tmp/inn.fcg" (babel-pathname)))))
    (cl-store::store inn path)))

(defun restore-network (pathname)
  (cl-store::restore pathname))


