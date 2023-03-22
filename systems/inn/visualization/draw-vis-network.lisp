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

(export '(draw-inn-network-in-vis-js))

(defun collect-vis-edges (inn)
  (let (vis-edges)
    (maphash #'(lambda(key value)
                 (declare (ignore key))
                 (pushnew value vis-edges :test #'string=))
             (vis-edges inn))
    vis-edges))

(defun draw-inn-network-in-vis-js 
       (inn 
        &key
        (interaction t)
        (id "integrativeNarrativeNetwork")
        (height "80%")
        (width "80%")
        (other-options "")
        (reset-web-interface t))
  (vis-destroy-network)
  (let* ((options 
          (format nil "~a~%~a~%~a~%~a~%~a"
                  (if interaction "interaction: { navigationButtons: true, 
                                                  keyboard: false, 
                                                  multiselect: true }," "")
                  "manipulation: {
                     enabled: false,
                     addEdge: function (edgeData, callback) {
                         ajax_addedge(edgeData.from, edgeData.to);
                         },
                   },"
                  (format nil "height: '~a'," height)
                  (format nil "width: '~a'," width)
                  other-options))
         (nodes (graph-utils::nodes inn))
         (inn-nodes (graph-utils::ids inn))
         (formatted-nodes (if nodes
                            (loop for value being each hash-value of nodes
                                  collect (inn-format-node (gethash value inn-nodes)))))
         (edges (collect-vis-edges inn)))
    (if reset-web-interface (wi::reset))
    (add-element
     `((div :id "interactiveINN")
       ((div :class "table")
        ((table)
         ((tr)
          ((td)
           ((button :class "inn-button" :role "button" 
                    :onclick "javascript:ajax_addinnnode();") 
            "Add Node"))
          ((td)
           ((button :class "inn-button" :role "button"
                    :onclick ,(format nil
                                      "javascript:network.addEdgeMode();"))
            "Add Edge"))
          ((td)
           ((button :class "inn-button" :role "button"
                    :onclick ,(format nil
                                      "javascript:clusterSelected();"))
            "Cluster Selected Nodes"))
          ((td :id "deleteSelectionButton")))
         ((tr :colspan "4")
          ((div :id "innpopup")))))
       ((div :id ,id) 
        ,(wi::make-vis-network
          :element-id id
          :nodes formatted-nodes
          :edges edges
          :options options
          :other (format nil
                  "network.on(\"selectNode\", function (params) {
                      var nodeId = params.nodes[0];
                      if (network.isCluster(nodeId) == true) {
                           network.openCluster(nodeId); }
                      else { javascript:ajax_nodeselected(nodeId); } });

                   network.on(\"selectEdge\", function (params) {
                      var nodeId = params.nodes[0];
                      var edgeId = params.edges[0];
                      var connectedNodes = network.getConnectedNodes(edgeId);
                      var fromId = connectedNodes[0];
                      var toId = connectedNodes[1];
                      javascript:ajax_edgeselected(edgeId, fromId, toId, nodeId); });

                   network.on(\"deselectNode\", function (params) {
                      javascript:ajax_removedeletebutton(); });

                   network.on(\"deselectEdge\", function (params) {
                      javascript:ajax_removedeletebutton(); });

                   function clusterByPid() {

                   }

                   function clusterSelected() {

                      var selectedNodes = network.getSelectedNodes();
                      var headNodeId = selectedNodes[0];
                      var headNode = nodes.get(headNodeId);
                      var clusterOptions = {
                             joinCondition: function(nodeOptions) {
                                var nodeId = nodeOptions.id;
                                return selectedNodes.includes(nodeId);
                             },
                             clusterNodeProperties: {
                                borderWidth: 3,
                                shape: headNode.shape,
                                label: headNode.label,
                                color: headNode.color,
                                image: headNode.image
                             },
                      };
                      network.clustering.cluster(clusterOptions);
                      network.selectNodes([]);
                      network.selectEdges([]);
                      ajax_removedeletebutton();
                   }

                  " id)))))))
;; (draw-inn-network-in-vis-js (make-instance 'integrative-narrative-network))
;; (draw-inn-network-in-vis-js (get-current-inn))
;; (wi::reset)

;;; (add-element `((script :type "text/javascript")
;;;                "var myNode = network.body.data.nodes.get('3');
;;;                 var myNodeId = myNode.id;
;;;                 var myOptions = { clusterNodeProperties: {
;;;                                    borderWidth: 3,
;;;                                    label: myNode.label,
;;;                                    color: myNode.color,
;;;                                    shape: myNode.shape,
;;;                                    image: myNode.image,
;;;                                    } }
;;;                 network.clustering.clusterByConnection(myNodeId, myOptions)"))
