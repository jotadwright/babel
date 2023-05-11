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
;; Example 2: Manipulating the network and adding custom nodes.
;; -------------------------------------------------------------------------
;; (a) Resetting the interface and network
;; -------------------------------------------------------------------------
(progn
  (wi::reset)
  (setf *my-inn* (make-instance 'integrative-narrative-network))
  (inn-add-nodes *my-inn* (list (make-entity-node :label "INN-package")
                                (make-open-narrative-question
                                 :label "Author?")))
  (inn-add-edge *my-inn* 0 1)
  (draw-inn-network-in-vis-js *my-inn*))

;; (b) Creating custom node classes
;; -------------------------------------------------------------------------
;; The node class "image-node" displays nodes as square images. Imagine 
;; we now want to have a node that visualizes nodes as circular images.
;; 
;; The easiest way to do so is to create a new "type" of inn-image. All
;; we have to do is to customize the helper methods that take care of the
;; visual identity of a node as follows:
;;
(defmethod get-node-shape ((type (eql :circular-image)))
  (declare (ignore type))
  "circularImage")

;; Let us now test to add such a circular image as the answer to
;; our question:
(progn
  (inn-add-node *my-inn* (make-inn-image
                          :type :circular-image
                          :description "Remi van Trijp"
                          :url "https://csl.sony.fr/wp-content/uploads/elementor/thumbs/remi-van-trijp-pwhtmlglh8mdlsqkxo0lxunj1gad8hlylx3oj7yey0.png"))
  (inn-add-edge *my-inn* 1 2)
  (inn-answer-question (lookup-node *my-inn* 1)))

;; However, suppose I want to create my own node "class" because this in 
;; turn will automatically create convenient helper functions. We 
;; can use the following macro:
;;
;; (def-inn-node name (parent)
;;      "Write a description function that mentions which TYPES are
;;       associated with this kind of inn-node."
;;      body)
;;
;; If you want to have default values that are different from those
;; of the "parent", you can do so as follows (similar to defstruct syntax):
;; (def-inn-node name (parent (keyword default-value))
;;      "description"
;;      body)
;;
;; Example:
(def-inn-node circular-image (inn-image
                              (type :circular-image))
              "Type (:my-inn-node):") ; This description is used by the web interface

;; If necessary, we should define helper methods GET-NODE-SHAPE and 
;; INN-FORMAT-NODE to ensure that the node is displayed correctly in 
;; the web interface, but in this case we have already methods for INN-NODE
;; of type :circular-image that would do the trick.
;;
;; Let us test it:
(progn
  (inn-add-node *my-inn* (make-circular-image
                          :description "Remi van Trijp"
                          :url "https://csl.sony.fr/wp-content/uploads/elementor/thumbs/remi-van-trijp-pwhtmlglh8mdlsqkxo0lxunj1gad8hlylx3oj7yey0.png"))
  (inn-add-edge *my-inn* 1 3))
;;
;; Let us delete the latter node because we have a duplicate now:
(inn-delete-node *my-inn* 3)

;; Now we manually edit the network:
;; -------------------------------------------------------------------------
;; 1. * In the web interface, click "add node"
;;    * Select "narrative-question" in the first slot
;;    * Provide "Works in?" as label
;;    * Click "save"
;;    -> the new node should now appear
;;
;; 2. * Click the button "add edge"
;;    * Draw an edge from the new question to the image node
;;      (or vice versa)
;;
;; 3. * Add an inn-image node. 
;;    * Description: "Paris"
;;    * URL: https://upload.wikimedia.org/wikipedia/commons/4/4b/La_Tour_Eiffel_vue_de_la_Tour_Saint-Jacques%2C_Paris_ao%C3%BBt_2014_%282%29.jpg
;;
;; 4. * Draw an edge from the red open question to the Paris node
;;    * The open question (red) is now answered and becomes green
;;
;; 5. * Now select the picture node of "Remi van Trijp". 
;;    * While keeping your command-buttonpressed, select the nodes "works in?" 
;;      and the Paris node. 
;;    * Now click on "cluster selected nodes" to make a cluster on the fly; 
;;      => Note that it is possible to create clusters of clusters in this way.
;;      => Note that the cluster will inherit its appearance from the first node that
;;         you selected. You should therefore always start with the most representative
;;         node of a cluster.
;;
;; 6. Double click the cluster to open up the cluster again.
;;
;; 7. Select either the Paris node or the edge that connects it
;;    to the "works in" question. Now click the "delete" button  
;;    that appears. The question is open again and becomes red.

;; -------------------------------------------------------------------------
;; Example 4: Storing, Restoring, and Saving
;; -------------------------------------------------------------------------
;;
;; You can save the vis-network as an image by right-clicking on it and 
;; selecting "copy image". You can now paste it into your favorite software.
;; You can also select "save as". It will be saved as a PNG file.
;;

(defparameter *stored-network* nil)

;; Let's call it a day:
(progn
  (wi::reset)
  (store-network *my-inn* 
                 (merge-pathnames "my-fabulous-network.lisp" 
                                  (babel-pathname))))

;; Let's get starting again
(setf *stored-network* 
      (restore-network (merge-pathnames "my-fabulous-network.lisp" 
                                        (babel-pathname))))
(draw-inn-network-in-vis-js *stored-network*)

