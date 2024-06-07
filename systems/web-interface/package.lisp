;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

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

(in-package :common-lisp)

#-:hunchentoot-available-on-this-platform
(error "Hunchentoot is not available on this platform. The web-interface does not work.")

(defpackage #:web-interface
  (:nicknames #:wi)
  (:use :common-lisp :cl-user :hunchentoot :ht-simple-ajax :utils)
  (:documentation "A package for the web interface"))

;; Temporarily overwrite this function from ht-simple-ajax until concatenate bug in LW has been resolved...

(in-package :ht-simple-ajax)

#+lispworks
(defun call-lisp-function (processor)
  "This is called from hunchentoot on each ajax request. It parses the 
   parameters from the http request, calls the lisp function and returns
   the response."
  (let* ((fn-name (string-trim "/" (subseq (script-name* *request*)
                                           (length (server-uri processor)))))
         (fn (gethash fn-name (lisp-fns processor)))
         (args (mapcar #'cdr (get-parameters* *request*))))
    (unless fn
      (error "Error in call-lisp-function: no such function: ~A" fn-name))
    
    (setf (reply-external-format*) (reply-external-format processor))
    (setf (content-type*) (content-type processor))
    (no-cache)
    (format nil "<?xml version=\"1.0\"?>
<response xmlns='http://www.w3.org/1999/xhtml'>~a</response>" (apply fn args))))