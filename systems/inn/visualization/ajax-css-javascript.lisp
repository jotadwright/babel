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

(defun-ajax makeinnnode (class-number inputs) (*ajax-processor*)
  (let* ((class-name (class-name (nth (parse-integer class-number) (inn:inn-node-structures))))
         (split-inputs (split-sequence::split-sequence #\, inputs))
         (keys-and-values (turn-split-inputs-into-keys-and-values split-inputs))
         (package (package-name (symbol-package class-name)))
         (constructor-fn (read-from-string (format nil "~a::make-~a" package class-name)))
         (inn (inn:get-current-inn)))
    (destructuring-bind (&whole whole
                                &key (type t)
                                (description "No description available.")
                                (label "label")
                                &allow-other-keys)
        keys-and-values
      (dolist (indicator '(:type :description :label))
        (remf whole indicator))
      (inn::inn-add-node inn
                         (apply constructor-fn `(:type ,(if (stringp type)
                                                          (read-from-string type)
                                                          type)
                                                 :description ,description
                                                 :label ,label
                                                 ,@whole)))
      (clearaddnode))))

(defun-ajax expandaddnode (value) (*ajax-processor*)
  (let* ((class-name (class-name (nth (parse-integer value) (inn:inn-node-structures))))
         (slot-descriptors (inn::get-inn-node-slot-descriptors class-name))
         (type-instruction (or (documentation class-name 'structure) "Type:")))
    (replace-element-content "typeinstruction"
                             `((th) ,type-instruction))
    (replace-element-content "expandablerow"
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
         (type-instruction (or (documentation (class-name (second (first inn-node-structures))) 'structure)
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
         ((input :type "text" :id "nodedescription" :value "No description available."))))
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
   var nodeclass = document.getElementById('selectnodeclass').value;
   var rest = [];
   var inputs = document.getElementsByTagName('input');

   for(var key in inputs) {
       var id = inputs[key].id;
       var value = inputs[key].value;
       var idvalue = [ id, value ];
       rest.push(idvalue); };       

   javascript:ajax_makeinnnode(nodeclass, rest);
   } }")

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

.button-43:hover {
  background-image: linear-gradient(-180deg, #1D95C9 0%, #17759C 100%);
}

@media (min-width: 768px) {
  .button-43 {
    padding: 1rem 2rem;
  }
}")
