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

;;;;; -----------------------------------------------------------------------------------
;;;;; Author(s) of this file: Remi van Trijp - remi.vantrijp@sony.com
;;;;; -----------------------------------------------------------------------------------
;;;;; Using vis.js in the web interface for creating interactive network visualizations.
;;;;; Please visit https://visjs.org/ for examples and documentation about all the options.
;;;;;
;;;;; To use the code in this file, simply load the :web-interface package and evaluate:
;;;;; (activate-vis-js)

(in-package :web-interface)

(export '(activate-vis-js 
          predicate-network->vis-js
          vis-network-interface ; General interface functions
          vis-add-node vis-update-node vis-remove-node ; convenience functions
          vis-add-edge vis-update-edge vis-remove-edge
          vis-destroy-network vis-redraw-network))

;;;;; -----------------------------------------------------------------------------------
;;;;; Activating vis.js in the web interface
;;;;; -----------------------------------------------------------------------------------

(defun activate-vis-js ()
  (define-js-library 'node-js "https://unpkg.com/vis-network/standalone/umd/vis-network.min.js"))

;;;;; -----------------------------------------------------------------------------------
;;;;; Making and dynamically controlling a network
;;;;; -----------------------------------------------------------------------------------

(defun make-vis-network (&key (element-id "visNetwork")
                              nodes
                              edges (options "")
                              (other ""))
  "Returns a network, to be added with add-element."
  `((script :type "text/javascript")
    ,(format nil
             "var nodes = new vis.DataSet([~a]);~%
              var edges = new vis.DataSet([~a]);~%~%
              var container = document.getElementById('~a');~%~%

              var data = {
                    nodes: nodes,
                    edges: edges
              };

              var options = { ~%~a
              };
             
             var network = new vis.Network(container, data, options);

             ~a"
             (if nodes (vis-format-many nodes) "")
             (if edges (vis-format-many edges) "")
             element-id
             options
             other)))


;;;;; -----------------------------------------------------------------------------------
;;;;; For dynamically controlling the network
;;;;; -----------------------------------------------------------------------------------

(defun vis-format-many (args)
  "Helper function for the functions below."
  (if (stringp args)
    args
    (format nil "~{~a~^,~%~}" args)))

;;;;; Adding, updating and removing nodes and edges 
;;;;; -----------------------------------------------------------------------------------
(defun vis-network-interface (data-type interface-fn args)
  (add-element `((script :type "text/javascript")
                 ,(format nil "network.body.data.~a.~a([~a])"
                          data-type interface-fn 
                          (vis-format-many args)))))

(defun vis-add-node (nodes)
  (vis-network-interface "nodes" "add" nodes))

(defun vis-update-node (nodes)
  (vis-network-interface "nodes" "update" nodes))

(defun vis-remove-node (nodes)
  (vis-network-interface "nodes" "remove" nodes))

(defun vis-add-edge (edges)
  (vis-network-interface "edges" "add" edges))

(defun vis-update-edge (edges)
  (vis-network-interface "edges" "update" edges))

(defun vis-remove-edge (edges)
  (vis-network-interface "edges" "remove" edges))

;;;;; Global network operations
;;;;; -----------------------------------------------------------------------------------
(defun vis-destroy-network ()
  (add-element `((script :type "text/javascript")
                 "network.destroy()")))

(defun vis-redraw-network ()
  (add-element `((script :type "text/javascript")
                 "network.redraw()")))

;;;;; -----------------------------------------------------------------------------------
;;;;; Predicate Networks in vis.js
;;;;; -----------------------------------------------------------------------------------

(defun format-vis-js-edge ( start-id end-id)	
  "Helper function for adding an edge to the javascript code."	
  (format nil "{id: '~a-~a', from: '~a', to: '~a' }" start-id end-id start-id end-id))

(defun format-vis-js-edge (start-id end-id)
  "Helper function for adding an edge to the javascript code."
  (format nil "{ from: '~a', to: '~a' }" start-id end-id))

;;;;; Disclaimer: this function works, but no effort has been put into making the 
;;;;;             visualization look nice. You can customize the look&feel of the
;;;;;             network through the keywords.
(defun predicate-network->vis-js (predicate-network 
                                  &key (element-id "visSemanticNetwork")
                                  (edges-options "color: 'black'")
                                  (predicate-node-options 
                                   "shape: 'box', color: {border: 'black', background: 'white', hover: 'blue'}")
                                  (variable-node-options 
                                   "shape: 'circle', color: {border: 'black', background: 'white', hover: 'blue'}"))
  "Translate a predicate network into a vis.js specification."
  (let (predicate-nodes variable-nodes edges)
    (dolist (predicate predicate-network)
      (let ((id (gensym))) ;; Unique ID for the predicate. Variables will just keep their name.
        (dolist (argument (rest predicate)) 
          (when (variable-p argument)
            (pushnew argument variable-nodes)
            (setf edges (cons (format-vis-js-edge id argument) edges))))
        (setf predicate-nodes (cons (format-vis-js-node id :label predicate) predicate-nodes))))
    (format nil "var nodes = new vis.DataSet([~%
                    ~{~a~^,~%~},~%
                    ~{~a~^,~%~}~%
                    ]);~%~%

                 var edges = new vis.DataSet([~%
                   ~{~a~^,~%~}~%
                   ]);~%~%

                 var container = document.getElementById('~a');~%~%

                 var data = {
                     nodes: nodes,
                     edges: edges
                 };

                 var options = {~%
                   edges: {~a},
                   groups: { 
                     predicateNodes: {~a},
                     variableNodes: {~a}
                   }

                 };

                 var network = new vis.Network(container, data, options);"
            predicate-nodes
            (loop for variable in variable-nodes
                  collect (format-vis-js-node variable :group "variableNodes"))
            edges
            element-id
            edges-options
            predicate-node-options
            variable-node-options)))

#|

;;;;; -----------------------------------------------------------------------------------
;;;;; Demonstrations
;;;;; -----------------------------------------------------------------------------------

(activate-vis-js)

;;;;; Manipulating Networks
;;;;; -----------------------------------------------------------------------------------

;; Add an initial network:

(add-element `((div :id "demoNetwork")
               ,(make-vis-network :element-id "demoNetwork"
                                  :nodes '("{id: 'node1', label: 'Node 1'}"
                                           "{id: 'node2', label: 'Node 2'}"
                                           "{id: 'node3', label: 'Node 3'}"
                                           "{id: 'node4', label: 'Node 4'}"
                                           "{id: 'node5', label: 'Node 5'}")
                                  :edges '("{from: 'node1', to: 'node3'}"
                                           "{from: 'node1', to: 'node2'}"
                                           "{from: 'node2', to: 'node4'}"
                                           "{from: 'node2', to: 'node5'}"))))
                                           
;; Nodes and edges with the general function:
;; ------------------------------------------
;; With the following function, you can already do all kinds of interface
;; functions with nodes and edges. Let us add a node:
(vis-network-interface "nodes" "add" "{id: 'node6'}")

;; Oops we forgot the label. Let us update the node:
(vis-network-interface "nodes" "update" "{id: 'node6', label: 'node 6'}")

;; We can add multiple nodes at once:
(vis-network-interface "nodes" "add" "{id: 'node7'}, {id: 'node8'}")
;; or:
(vis-network-interface "nodes" "add" '("{id: 'node9'}" "{id: 'node10'}"))

;; The graph is getting cluttered, let us remove some:
(vis-network-interface "nodes" "remove" "{id: 'node6'}")
(vis-network-interface "nodes" "remove" "{id: 'node7'}, {id: 'node8'}, {id: 'node9'}")

;; Let us now connect the remaining new node with an edge:
(vis-network-interface "edges" "add" "{from: 'node1', to: 'node10'}")

;; We can alternatively also use convenience functions:
;; ----------------------------------------------------
(vis-remove-node "{id: 'node10'}")
(vis-add-node "{id: 'node7', label: 'node 7'}")
(vis-add-edge "{from: 'node1', to: 'node7'}")

;; Finally, we destroy our network:
(vis-destroy-network)

;;;;; "Lost in Translation" Partial Character Relationship Chart
;;;;; -----------------------------------------------------------------------------------
(let ((murray (list "Bob" "https://fr.web.img2.acsta.net/newsv7/20/12/07/18/08/0772723.png"))
      (johansson (list "Charlotte" 
                       "https://focus.telerama.fr/967x550/100/2021/03/29/7b620e16-079e-4f47-a085-e09b0a897ff9.jpg"))
      (ribisi (list "John" 
                    "https://i0.wp.com/nobadmovie.com/wp-content/uploads/2021/07/giovanniribisi_lostintranslation_box.png"))
      (faris (list "Kelly"
                   "https://m.media-amazon.com/images/M/MV5BZWUzZTM3MzEtMmQwZi00ZWExLThhNzYtMGQwODFlMTk2NzNhXkEyXkFqcGdeQXVyNDA4MDkxNzE@._V1_.jpg")))
  (add-element 
   `((div :id "LostInTranslation")
     ,(make-vis-network :element-id "LostInTranslation"
                        :nodes (loop for character in (list murray johansson ribisi faris)
                                     collect (format nil "{id: '~a', label: '~a', shape: 'circularImage', image: '~a'}"
                                                     (first character) (first character) (second character)))
                        :edges '("{from: 'Charlotte', to: 'Bob', label: 'friends'}"
                                 "{from: 'Charlotte', to: 'John', label: 'married'}"
                                 "{from: 'John', to: 'Kelly', label: 'co-workers'}")))))
 
;;;;; Predicate Networks
;;;;; -----------------------------------------------------------------------------------

(defparameter *vis-js-predicate-network* nil "Try out different semantic networks.")

(setf *vis-js-predicate-network* '((get-context ?context)
                                   (bind shape ?shape [square])
                                   (filter-by-shape ?squares ?context ?shape)
                                   (unique-element ?topic ?squares)))

;; Now add it to the web interface:
(add-element `(((div :id "visSemanticNetwork"))
               ((script :type "text/javascript")
                ,(predicate-network->vis-js *vis-js-predicate-network*))))

(setf *vis-js-predicate-network* '((cause-frame ?ev)
                                   (causer ?ev ?x)
                                   (effect ?ev ?y)
                                   (heat ?x)
                                   (draught ?y)))

(add-element `((script :type "text/javascript")
               ,(predicate-network->vis-js *vis-js-predicate-network*)))
|#
