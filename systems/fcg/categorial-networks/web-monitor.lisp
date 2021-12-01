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
(in-package :fcg)

(export '(trace-categorial-networks-in-web-interface))

(define-monitor trace-categorial-networks-in-web-interface
                :documentation "This is mostly used for debugging purposes")

(define-event-handler (trace-categorial-networks-in-web-interface category-added)
  (add-element `((h3) ,(format nil "Adding category \"~a\" to the categorial network"
                               category)))
  (add-element (make-html categorial-network :weights? t))
  (add-element '((hr))))

(define-event-handler (trace-categorial-networks-in-web-interface category-removed)
  (add-element `((h3) ,(format nil "Removing category \"~a\" from the categorial network"
                               category)))
  (add-element (make-html categorial-network :weights? t))
  (add-element '((hr))))

(define-event-handler (trace-categorial-networks-in-web-interface link-added)
  (add-element `((h3) ,(format nil "Creating link between \"~a\" and \"~a\""
                               category-1 category-2)))
  (add-element (make-html categorial-network :weights? t))
  (add-element '((hr))))

(define-event-handler (trace-categorial-networks-in-web-interface link-removed)
  (add-element `((h3) ,(format nil "Removing link between \"~a\" and \"~a\""
                               category-1 category-2)))
  (add-element (make-html categorial-network :weights? t))
  (add-element '((hr))))