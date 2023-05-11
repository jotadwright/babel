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

(in-package :web-interface)

(setf *no-reset-button* t)

;; =========================================================================
;; AJAX and JAVASCRIPT
;; =========================================================================

;; -------------------------------------------------------------------------
;; 1. Add Node
;; -------------------------------------------------------------------------

(defun-ajax clearaddnode () (*ajax-processor*)
  (replace-element-content "innpopup" "")
  nil)

(defun make-keyword-from-input (input)
  (cond ((string= "nodetype" input) :type)
        ((string= "nodelabel" input) :label)
        ((string= "nodedescription" input) :description)
        ((string= "clusterids" input) :cluster-ids)
        (t
         (read-from-string (format nil ":~a" input)))))
;; (make-keyword-from-input "nodetype")
;; (make-keyword-from-input "URL")

(defun turn-split-inputs-into-keys-and-values (inputs)
  (cond ((null inputs) 
         nil)
        ((string= "" (first inputs))
         (turn-split-inputs-into-keys-and-values (subseq inputs 2)))
        (t
         (let* ((key (make-keyword-from-input (first inputs)))
                (val (cond ((string= "" (second inputs)) nil)
                           ((eq key :type) (read-from-string (second inputs)))
                           (t
                            (second inputs)))))
           (append (list key val)
                   (turn-split-inputs-into-keys-and-values (subseq inputs 2)))))))

(defun-ajax makeinnnode (class-number inputs selected-node-id) (*ajax-processor*)
  (let* ((from-node-id (parse-integer selected-node-id :junk-allowed t))
         (class-name (class-name (nth (parse-integer class-number) (inn:inn-node-structures))))
         (split-inputs (split-sequence::split-sequence #\, inputs))
         (keys-and-values (turn-split-inputs-into-keys-and-values split-inputs))
         (package (package-name (symbol-package class-name)))
         (constructor-fn (read-from-string 
                          (format nil "~a::make-~a" package class-name)))
         (inn (inn:get-current-inn)))
    (destructuring-bind (&whole whole
                                &key (type nil)
                                (description "No description available.")
                                (label "label")
                                (cluster-ids nil)
                                &allow-other-keys)
        keys-and-values
      (dolist (indicator '(:type :description :label :cluster-ids))
        (remf whole indicator))
      (let ((target-node-id 
             (inn::inn-add-node 
              inn
              (apply constructor-fn `(,@(if type
                                          (let ((the-type (if (stringp type)
                                                            (read-from-string type)
                                                            type)))
                                            (if (keywordp the-type)
                                              `(:type ,the-type))))
                                      :description ,description
                                      :label ,label
                                      :cluster-ids ,(if (stringp cluster-ids)
                                                      (if (string= cluster-ids "")
                                                        nil
                                                        (read-from-string cluster-ids))
                                                      nil)
                                      ,@whole)))))
        (if from-node-id
          (inn::inn-add-edge inn from-node-id target-node-id))
        (clearaddnode)))))

(defun-ajax expandaddnode (value) (*ajax-processor*)
  (let* ((class-name (class-name 
                      (nth (parse-integer value) (inn:inn-node-structures))))
         (slot-descriptors (inn::get-inn-node-slot-descriptors class-name))
         (type-instruction (or (documentation class-name 'structure) "Type:")))
    (replace-element-content 
     "typeinstruction"
     `((th) ,type-instruction))
    (replace-element-content 
     "expandablerow"
     (if (null slot-descriptors)
       nil
       `((div)
         ,@(loop for slot-descriptor in slot-descriptors
                 for th = (format nil "~a" (first slot-descriptor))
                 for default = (if (second slot-descriptor) 
                                 (format nil "~a" (second slot-descriptor))
                                 "")
                 collect `((tr)
                           ((th) ,th)
                           ((td :colspan "2")
                            ((input :id ,th
                                    :type "text"
                                    :name "array[]"
                                    :value ,default))))))))
    nil))

(defun-ajax addinnnode () (*ajax-processor*)
  (let* ((inn-node-structures (let ((i -1))
                                (loop for struct in (inn:inn-node-structures)
                                      collect (list (incf i) struct))))
         (type-instruction (or (documentation 
                                (class-name (second (first inn-node-structures))) 'structure)
                               "Type:")))
    (replace-element-content 
     "innpopup"
     `((table :style "text-align:left;border-style:solid;")
       ((tr :style "background-color:powderblue;")
        ((th :colspan "3") "Add Node"))
       ((tr)
        ((th) "Node class:")
        ((td :colspan "2")
         ((select :id "selectnodeclass" :onchange "javascript:selectednodeclass();")
          ,@(loop for node-spec in inn-node-structures
                  for value = (format nil "~a" (first node-spec))
                  for content = (format nil "~a" (class-name (second node-spec)))
                  collect `((option :value ,value) ,content)))))
       ((tr)
        ((div :id "typeinstruction")
         ((th) ,type-instruction))
        ((td :colspan "2")
         ((input :type "text" :id "nodetype" :value "type"))))
       ((tr)
        ((th) "label:")
        ((td :colspan "2")
         ((input :type "text" :id "nodelabel" :value "label"))))
       ((tr)
        ((th) "description:")
        ((td :colspan "2")
         ((input :type "text" :id "nodedescription" 
                 :value "No description available."))))
       ((tr)
        ((th) "cluster-ids:")
        ((td :colspan "2")
         ((input :type "text" :id "clusterids" :value ""))))
       ((tr :id "expandablerow"))
       ((tr)
        ((td :colspan "3")
         ((table)
          ((tr) 
           ((td) ((button :onclick "javascript:makeinnnode();") 
                  "save"))
           ((td) "or")
           ((td) ((button :onclick "javascript:ajax_clearaddnode();")
                  "cancel"))))))))
    nil))

(define-js 'selectednodeclass " {function selectednodeclass () {
   var value = document.getElementById('selectnodeclass').value;
  
   javascript:ajax_expandaddnode(value); } }")

(define-js 'makeinnnode "{ function makeinnnode () {
   var selectedNodes = network.getSelectedNodes();
   var selectedNodeId = selectedNodes[0];
   var nodeclass = document.getElementById('selectnodeclass').value;
   var rest = [];
   var inputs = document.getElementsByTagName('input');

   for(var key in inputs) {
       var id = inputs[key].id;
       var value = inputs[key].value;
       var idvalue = [ id, value ];
       rest.push(idvalue); };       

   javascript:ajax_makeinnnode(nodeclass, rest, selectedNodeId);
   } }")

;; -------------------------------------------------------------------------
;; 2. Add Edge
;; -------------------------------------------------------------------------

(defun-ajax addedge (from to) (*ajax-processor*)
  (let ((from-id (parse-integer from))
        (to-id (parse-integer to))
        (inn (inn:get-current-inn)))
    (inn::inn-add-edge inn from-id to-id)
    ;; See if there were open questions that are now answered:
    (inn:question-answered? inn from-id :answered-by to-id)
    (inn:question-answered? inn to-id :answered-by from-id)
    nil))

;; -------------------------------------------------------------------------
;; 3. Events
;; -------------------------------------------------------------------------

(defun-ajax doubleclick (selection) (*ajax-processor*)
  (inn::inn-double-click selection (inn:get-current-inn))
  nil)

(defun-ajax rightclick () (*ajax-processor*)
  (inn:inn-right-click (inn:get-current-inn))
  nil)

;; -------------------------------------------------------------------------
;; 4. Delete Selection
;; -------------------------------------------------------------------------

(defun-ajax deleteselection (element-id from-id to-id) (*ajax-processor*)
  (format t "Delete ~a" element-id)
  (let ((node-id (parse-integer element-id :junk-allowed t))
        (formatted-element (format nil "{id: '~a'}" element-id))
        (inn (inn:get-current-inn)))
    ;; Delete in the browser
    (if node-id ;; We have selected a node.
      (progn
        (vis-remove-node formatted-element)
        (inn:inn-delete-node inn node-id))
      ;; Or only an edge:
      (progn
        (vis-remove-edge formatted-element)
        (inn::inn-delete-edge inn (parse-integer from-id) (parse-integer to-id))))
    ;; Remove delete button
    (removedeletebutton)))

(defun-ajax removedeletebutton () (*ajax-processor*)
  (replace-element-content "deleteSelectionButton" "")
  nil)

(defun-ajax nodeselected (node-id) (*ajax-processor*)
  ;(format t "Selected node: ~a" node-id)
  (replace-element-content "deleteSelectionButton"
                           `((button :class "inn-button" :role "button"
                                     :onclick ,(format nil
                                                       "javascript:ajax_deleteselection('~a', 'nil', 'nil');"
                                                       node-id))
                             "Delete Selected Node"))
  nil)

(defun-ajax edgeselected (edge-id from-id to-id node-id) (*ajax-processor*)
  (let ((node (parse-integer node-id :junk-allowed t)))
    (format t "Edge connected with from ~a to ~a" from-id to-id)
    (replace-element-content "deleteSelectionButton"
                             `((button :class "inn-button" :role "button"
                                       :onclick ,(format nil
                                                         "javascript:ajax_deleteselection('~a', '~a', '~a');"
                                                       (if node node-id edge-id)
                                                       from-id to-id))
                               ,(if node "Delete Selected Node" "Delete Selected Edge")))
    nil))

;; -------------------------------------------------------------------------
;; CSS
;; -------------------------------------------------------------------------

(define-css 'inn-button "
.inn-button {
  background-image: linear-gradient(-180deg, #37AEE2 0%, #1E96C8 100%); 
  border-radius: .5rem;
  box-sizing: border-box;
  color: #FFFFFF;
  display: flex;
  font-size: 16px;
  justify-content: center;
  padding: 1rem 1.75rem;
  text-decoration: none;
  width: 100%;
  border: 0;
  cursor: pointer;
  user-select: none;
  -webkit-user-select: none;
  touch-action: manipulation;
}


.inn-button:active {
  background-image: linear-gradient(-180deg, #1D95C9 0%, #17759C 100%);
}

@media (min-width: 768px) {
  .button-43 {
    padding: 1rem 2rem;
  }
}")

;;; .inn-button:hover {
;;;   background-image: linear-gradient(-180deg, #1D95C9 0%, #17759C 100%);
;;; }
