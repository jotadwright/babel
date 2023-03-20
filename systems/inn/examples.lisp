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

; (ql:quickload :inn)

(in-package :inn)

;; -------------------------------------------------------------------------
;; Example 1: The Basics
;; -------------------------------------------------------------------------

(defparameter *my-inn* (make-instance 'integrative-narrative-network))
(defparameter *my-nodes*
  (list (make-inn-node :label "INN package") ; node 0
        (make-narrative-question :label "Author?") ; Node 1
        (make-narrative-question :label "Language?") ; Node 2
        (make-entity-node :label "Remi van Trijp") ; Node 3
        (make-entity-node :label "Common Lisp") ; Node 4
        (make-entity-node :label "Imer nav Pjirt"))) ; Node 5

;; An integrative narrative network is a Lisp object, but if you have 
;; a visualization opened in the web interface, you can see how the network
;; is updated in real-time using vis.js. Simply make a vis-network and 
;; add it to the web interface (see /systems/web-interface/vis-js.lisp).

(draw-inn-network-in-vis-js *my-inn*)

;; We add our starting node:
(inn-add-node *my-inn* (nth 0 *my-nodes*))

;; This node raises two "questions":
(progn
  (inn-add-nodes *my-inn* (subseq *my-nodes* 1 3))
  (inn-add-edges *my-inn* '((0 1) ;; You can pass the nodes or their IDs (integers)
                            (0 2))))

;; We now answer one question:
(progn
  (inn-add-node *my-inn* (nth 3 *my-nodes*))
  (inn-answer-question (nth 1 *my-nodes*) :answer (nth 3 *my-nodes*))
  (inn-add-edge *my-inn* 1 3))

;; Oops we made some mistakes:
(progn
  (inn-add-node *my-inn* (nth 5 *my-nodes*))
  (inn-add-edge *my-inn* 0 4)
  (inn-add-edge *my-inn* 0 3))

;; Let us delete the wrong node (which automatically calls delete-edge as well) and edge:
(progn
  (inn-delete-node *my-inn* 4)
  (inn-delete-edge *my-inn* 0 3))

;; Oh no! We accidentally reset the web interface!
(wi::reset)

;; No worries, we can redraw the network:
(draw-inn-network-in-vis-js *my-inn*)

;; -------------------------------------------------------------------------
;; Example 2: Storing, Restoring, and Saving
;; -------------------------------------------------------------------------
;;
;; You can save the vis-network as an image by right-clicking on it and 
;; selecting "copy image". You can now paste it into your favorite software.
;; You can also select "save as". It will be saved as a PNG file.

(defparameter *stored-network* nil)

;; Let's call it a day:
(progn
  (wi::reset)
  (store-network *my-inn* (merge-pathnames "my-fabulous-network.lisp" (babel-pathname))))

;; Let's get starting again
(setf *stored-network* (restore-network (merge-pathnames "my-fabulous-network.lisp" (babel-pathname))))
(draw-vis-network *stored-network*)

