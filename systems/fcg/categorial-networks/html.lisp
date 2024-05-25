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

(export '(categorial-network->image
          categorial-network-components->images))

(defun categorial-network->image (categorial-network
                              &key (render-program "dot")
                              weights? colored-edges-0-1
                              path file-name format
                              (open nil))
  "makes an image from a type hierarchy using s-dot
   (avoids generating a .dot file in the tmp folder)"
  (let* ((dir (or path wi::*graphviz-output-directory*))
         (format (or format "png"))
         (file-name (or file-name (downcase (gensym "CATEGORIAL-NETWORK-"))))
         (path (merge-pathnames dir (make-pathname :name file-name
                                                   :type format))))
    (s-dot->image
     (categorial-network->s-dot
      categorial-network
      :weights? weights?
      :colored-edges-0-1 colored-edges-0-1)
     :format format :open open :path path
     :render-program render-program)))

(defun categorial-network-components->images (categorial-network
                                          &key (render-program "dot")
                                          weights? colored-edges-0-1
                                          path file-name format
                                          (open nil)
                                          (minimum-component-size 0))
  "Makes a separate image for each connected component of the type hierarchy.
   Use the keyword ':minimum-component-size' if you want to bundle together
   components smaller than the specified size in a single image."
  (let* ((graph (graph categorial-network))
         (list-of-components (graph-utils::find-components graph)))
    (setf list-of-components
          (loop with original-components = nil
                with bundled-component = nil
                for component in list-of-components
                if (length> component minimum-component-size)
                do (push component original-components)
                else do (setf bundled-component
                              (append bundled-component component))
                finally (return (if bundled-component
                                  (append original-components (list bundled-component))
                                  original-components))))
    (if (length= list-of-components 1)
      (categorial-network->image categorial-network :render-program render-program
                             :weights? weights? :colored-edges-0-1 colored-edges-0-1
                             :path path :file-name file-name :format format :open open)
      (let* ((dir (or path wi::*graphviz-output-directory*))
             (format (or format "png"))
             (file-name (or file-name (downcase (gensym "CATEGORIAL-NETWORK-"))))
             (path (merge-pathnames dir (make-pathname :name file-name
                                                       :type format))))
        (loop for component in list-of-components
              for i from 1
              for subgraph = (graph-utils::make-graph :directed? (graph-utils::directed? graph))
              for subgraph-path
              = (make-pathname :directory (pathname-directory path)
                               :name (format nil "~a-~a" (pathname-name path) i)
                               :type (pathname-type path))
              do (loop for id in component
                       for name = (gethash id (graph-utils::ids graph))
                       do (graph-utils:add-node subgraph name))
              do (loop for id in component
                       for name = (gethash id (graph-utils::ids graph))
                       do (loop for neighbor-id in (if (graph-utils::directed? graph)
                                                     (graph-utils::outbound-neighbors graph name)
                                                     (graph-utils::neighbors graph name))
                                for neighbor-name = (gethash neighbor-id (graph-utils::ids graph))
                                for weight = (graph-utils::edge-weight graph name neighbor-name)
                                do (graph-utils:add-edge subgraph name neighbor-name :weight weight)))
              do (s-dot->image
                  (graph-utils::categorial-network->s-dot
                   subgraph :weights? weights?
                   :colored-edges-0-1 colored-edges-0-1)
                  :format format :open open :path subgraph-path
                  :render-program render-program))))))
            

(defmethod make-html ((categorial-network categorial-network)
                      &key (weights? nil)
                      (colored-edges-0-1 t)
                      (render-program "dot")
                      (expand-initially t)
                      &allow-other-keys)
  "generates html code for a type hierarchy using s-dot
   (avoids generating a .dot and a .svg file in tmp folder)"
  (let* ((element-id (make-id 'categorial-network))
         (title (format nil "CATEGORIAL NETWORK (~a categor~@p, ~a link~p)"
                                (nr-of-categories categorial-network)
                                (nr-of-categories categorial-network)
                                (nr-of-links categorial-network)
                                (nr-of-links categorial-network)))
         (outer-div
          (lambda (expanded? children)
            (make-div-with-menu 
             :div-attributes (if expanded?
                               '(:class "fcg-light-construction-inventory fcg-light-construction-inventory-expanded")
                               '(:class "fcg-light-construction-inventory"))
             :div-children children)))
         (collapsed (lambda ()
                      (funcall outer-div nil
                      `(((div :class "title") 
                        ((a ,@(make-expand/collapse-link-parameters 
                               element-id t "show network")) 
                       ,title))))))
         (expanded (lambda ()
                     (funcall outer-div t
                     `(((div :class "title") 
                        ((a ,@(make-expand/collapse-link-parameters 
                               element-id nil "hide network")) 
                         ,title))
                       ((div)
                        ,(s-dot->svg
                          (categorial-network->s-dot
                           categorial-network
                           :weights? weights?
                           :colored-edges-0-1 colored-edges-0-1)
                          :render-program render-program)))))))
                            
    `((div)
      ,(make-expandable/collapsable-element 
        element-id nil 
        collapsed 
        expanded
        :expand-initially expand-initially))))

(defmethod categorial-network->s-dot ((categorial-network categorial-network)
                                  &key weights? colors sizes colored-edges-0-1)
  "Make a dot representation of the graph using s-dot.
   weights? : show the weights on the edges or not.
   colors : a hash table that stores a color for each node
   sizes : a hash table that stores a size for each node
   colored-edges-0-1 : use shades of gray and edge style to indicate
   if the weight of the edge is close to 0 or 1"
   (let* ((graph (graph categorial-network))
          (graph-properties '((s-dot::fontcolor "#000000")
                              (s-dot::fontsize "10.0")
                              (s-dot::fontname "Helvetica")
                              (s-dot::rankdir "LR")))
         (all-node-names (graph-utils::list-nodes graph))
         (all-node-ids
          (loop for node-name in all-node-names
                for id = (gethash node-name (graph-utils::nodes graph))
                collect id))
         (all-edges (graph-utils::list-edges graph :nodes-as-ids t))
         (s-dot-nodes
          (loop for node-name in all-node-names
                for node-id in all-node-ids
                collect (graph-utils::categorial-network-node->s-dot node-name node-id
                                                    :colors colors
                                                    :sizes sizes)))
         (s-dot-edges
          ;; we are in an undirected graph
          (loop for (from-id to-id) in all-edges
                for edge-weight = (graph-utils::edge-weight graph from-id to-id)
                when (> from-id to-id)
                collect (graph-utils::categorial-network-edge->s-dot from-id to-id
                                                    :weight (if weights? edge-weight nil)
                                                    :directedp (graph-utils::directed? graph)
                                                    :colored-edges-0-1 colored-edges-0-1))))
     `(s-dot::graph ,graph-properties
                    ,@s-dot-nodes
                    ,@s-dot-edges)))

      
