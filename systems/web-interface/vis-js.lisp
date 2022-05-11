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

(in-package :web-interface)

(export '(activate-vis-js predicate-network->vis-js))

;;;;; Using vis.js in the web interface for creating interactive network visualizations.
;;;;; Please visit https://visjs.org/ for examples and documentation about all the options.
;;;;;
;;;;; To use the code in this file, simply load the :web-interface package and evaluate:
;;;;; (activate-vis-js)

(defun activate-vis-js ()
  (define-js-library 'node-js "https://unpkg.com/vis-network/standalone/umd/vis-network.min.js"))

(defun format-vis-js-node (id &key label (group "predicateNodes"))
  "Helper function for adding a node to the javascript code."
  (format nil "{ id: '~a', label: '~a', group: '~a' }" id (or label id) group))

(defun format-vis-js-edge (start-id end-id)
  "Helper function for adding an edge to the javascript code."
  (format nil "{ from: '~a', to: '~a' }" start-id end-id))

(defun predicate-network->vis-js (predicate-network 
                                  &key (element-id "visSemanticNetwork")
                                  (edges-options "color: 'black'")
                                  (predicate-node-options 
                                   "shape: 'box', color: {border: 'black', background: 'white', hover: 'blue'}")
                                  (variable-node-options 
                                   "shape: 'circle', color: {border: 'black', background: 'white', hover: 'blue'}"))
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
;;;;; Demonstration
;;;;; -----------------------------------------------------------------------------------

(activate-vis-js)

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


#|
;; Some tryout for automatically converting s-dot to dot to node-js.
;; Did not work (yet), but potentially very useful.

(in-package :web-interface)

(defun wi-s-dot->dot (s-dot-expression)
  (with-open-stream (dot-stream (make-string-output-stream))
    (s-dot:s-dot->dot dot-stream s-dot-expression)
    (get-output-stream-string dot-stream)))

;; Does not seem to work because of the s-dot->dot conversion being incompatible
(defun import-s-dot-in-node-js (s-dot-expression &key (element-id "mynetwork") (extended-options ""))
  "Translate s-dot-expression to dot and import it in node.js"
  (let ((container (format nil "var container = document.getElementById('~a');" element-id))
        (dot-string (format nil "var DOTstring = '~a'" (wi-s-dot->dot s-dot-expression)))
        (parsed-data "var parsedData = vis.parseDOTNetwork(DOTstring);

                      var data = {
                        nodes: parsedData.nodes,
                        edges: parsedData.edges
                      }

                      var options = parsedData.options;")
        (network "var network = new vis.Network(container, data, options);"))
    (format nil "~%~a~%~a~%~a~%~%~a~%~%" container dot-string parsed-data extended-options network)))
|#
