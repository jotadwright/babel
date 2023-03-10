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

(export '(identify-network-updates))

;; ------------------------------------------------------------------------------------------
;; 1. Identify network updates
;; ------------------------------------------------------------------------------------------
;; Generic function for identifying which updates are needed to the 
;; network, such as which nodes to add.
;; Each method returns either NIL or a list of objects that 
;; another method will handle (e.g. variables, entities, bindings, ...).

(defgeneric identify-network-updates (nodes network &key &allow-other-keys))

;; ------------------------------------------------------------------------------------------
;; (a) semantic networks: method called after an FCG parsing task.
;; ------------------------------------------------------------------------------------------


;; ------------------------------------------------------------------------------------------
;; (b) entities: IRL-bindings that consist of a variable and an entity.
;; ------------------------------------------------------------------------------------------



#|

;; ------------------------------------------------------------------------------------------
;; (a) semantic networks: method called after an FCG parsing task.
;; ------------------------------------------------------------------------------------------

(defstruct semantic-network primitives bindings)

(defmethod identify-network-updates ((semantic-network semantic-network)
                                     (network integrative-network)
                                     &key &allow-other-keys)
  ;; This method is always called after parsing with FCG. 
  (if (null (semantic-network-primitives semantic-network)) ;; We handled all primitives
    nil
    ;; We handle the first primitive and recursively deal with the rest.
    (let* ((primitive (pop (semantic-network-primitives semantic-network)))
           (predicate (first primitive))
           (arguments (rest primitive))
           (predicate-id (make-const predicate)))
      (unless (eql 'bind (first primitive)) ;; Don't handle bind statements yet, that is done in interpretation
        ;; Add the predicate as a new node with new edges
        (setf (gethash predicate-id (integrative-network-update-nodes network)) (make-integrative-network-node
                                                                                 :label (mkstr predicate)
                                                                                 :type 'predicate
                                                                                 :connected-nodes arguments
                                                                                 :under-attention t)
              (gethash predicate-id (integrative-network-update-edges network)) arguments)
        ;; Handle the arguments
        (dolist (argument arguments)
          (identify-network-updates argument network 
                                    :under-attention? t
                                    :source predicate-id)))
      ;; Handle the other primitives:
      (identify-network-updates semantic-network network))))

;; ------------------------------------------------------------------------------------------
;; (b) accessible entities: the discourse model contains a list of accessible entities
;;                          consisting of an IRL-binding that binds a variable and an entity.
;; ------------------------------------------------------------------------------------------

(defmethod identify-network-updates ((node binding)
                                     (network integrative-network)
                                     &key exhaustive? &allow-other-keys)
  ;; An IRL-binding relates a variable to an entity.
  (let* ((node-id (get-object-id node)) ;; The variable name
         (value (slot-value node 'value)) ;; An entity
         (value-id (if value (get-object-id value))))
    ;; If the binding is already known, we do not have to add it again.
    (if (and (gethash node-id (integrative-network-nodes network))
             (eql 'answered-question (integrative-network-node-type 
                                      (gethash node-id (integrative-network-nodes network)))))
      nil
      ;; Otherwise we add a new node and its edges to the update list
      (if value
        (progn
          (setf (gethash node-id (integrative-network-update-nodes network)) (make-integrative-network-node
                                                                              :label node-id
                                                                              :type 'answered-question
                                                                              :connected-nodes (list value-id)
                                                                              :under-attention t)
                (gethash node-id (integrative-network-update-edges network)) (list value-id))
          ;; Now handle the entity.
          (identify-network-updates value network :under-attention? t :exhaustive? exhaustive?))    
        (setf (gethash node-id (integrative-network-update-nodes network)) (make-integrative-network-node
                                                                            :label node-id
                                                                            :type 'open-question
                                                                            :under-attention t))))
    ;; Return NIL
    nil))

(defmethod identify-network-updates ((entity entity)
                                     (network integrative-network)
                                     &key exhaustive? (under-attention? nil) &allow-other-keys)
  ;; Most complex object to handle because there may be new edges, and 
  ;; values can be lists of entities.
  (let* ((node-id (get-object-id entity))
         (node (make-integrative-network-node :label node-id :type 'entity :under-attention under-attention?)))
    ;; If the node does not exist yet, add it to our update-list:
    (unless (gethash node-id (integrative-network-nodes network))
      (setf (gethash node-id (integrative-network-update-nodes network)) node)
      ;; When we want to search exhaustively, handle the edges as well:
      (if exhaustive?
        ;; Only handle edges when we search exhaustively:
        (let* ((slot-names (remove-if #'(lambda(x)
                                          (member x '(id persistent-id)))
                                      (get-slot-names entity)))
               (slot-values (loop for slot-name in slot-names
                                  for value = (slot-value entity slot-name)
                                  append (cond ((or (null value)
                                                    (eql 'none value)) (list (get-object-id value)))
                                               ((listp value)  value)
                                               (t
                                                (list value)))))
               (edges (mapcar #'get-object-id slot-values)))
          ;; Add edges so they will be checked if update is required.
          (setf (integrative-network-node-connected-nodes node) edges
                (gethash node-id (integrative-network-update-edges network)) edges)
          ;; Handle the other nodes:
          (identify-network-updates slot-values network :exhaustive? exhaustive? :under-attention? nil))
        ;;  Else return NIL:
        nil))))

;; ------------------------------------------------------------------------------------------
;; (c) List: Intended to be used as handling a list of bindings (= accessible entities), 
;;           but can be used for any list.
;; ------------------------------------------------------------------------------------------
(defmethod identify-network-updates ((nodes list)
                                     (network integrative-network)
                                     &key exhaustive? &allow-other-keys)
  (dolist (node nodes)
    (identify-network-updates node network :exhaustive? exhaustive?)))

;; ------------------------------------------------------------------------------------------
;; (d) T and other atomic values: For any value of an entity's slot (except variables)
;; ------------------------------------------------------------------------------------------

(defmethod identify-network-updates ((object t)
                                     (network integrative-network)
                                     &key source (under-attention? nil) &allow-other-keys)
  (let* ((node-id (get-object-id object))
         (existing-node (or (gethash node-id (integrative-network-update-nodes network)) 
                            (gethash node-id (integrative-network-nodes network))))
         (existing-edges (if existing-node (integrative-network-node-connected-nodes existing-node)))
         (node (make-integrative-network-node
                :type (cond ((and existing-node (variable-p node-id)) 'answered-question)
                            ((variable-p node-id) 'open-question)
                            (t
                             'value))
                :connected-nodes (if source
                                   (union (list source) existing-edges)
                                   existing-edges)
                :under-attention under-attention?)))
    ;; Add (or replace) the node to the list of updates:
    (setf (gethash node-id (integrative-network-update-nodes network)) node)
    ;; We have reached a terminal node so return NIL.
    nil))

;; -----------------------------------------------------------------------
;; 2. Writing to gephi-compatible files
;; -----------------------------------------------------------------------

(defun write-data-to-gephi (counter fcg-or-irl utterance network)
  (let ((pathnames (loop for network-entity in '("nodes" "edges")
                         collect (babel-pathname :directory '(".tmp")
                                                 :name (format nil "~a-~a-~a-~a" network-entity 
                                                               (if (>= counter 10)
                                                                 counter
                                                                 (format nil "0~a" counter))
                                                               fcg-or-irl
                                                               (cl-ppcre:regex-replace-all " " utterance "_"))
                                                 :type  "csv"))))
    ;; Write the nodes
    (with-open-file (stream (first pathnames) :direction :output
                            :if-exists :supersede)
      (format stream "Id,Type,Shape,Color~%")
      (loop for key being the hash-keys in (integrative-network-nodes network)
            for type = (integrative-network-node-type (gethash key (integrative-network-nodes network)))
            do (format stream "~a,~a,~a,~a~&" key type (get-node-shape type) (get-node-color type))))
    ;; Write the edges
    (with-open-file (stream (second pathnames) :direction :output
                            :if-exists :supersede)
      (format stream "Source,Target~%")
      (loop for key being the hash-keys in (integrative-network-edges network)
            do (loop for target in (gethash key (integrative-network-edges network))
                     collect (format stream "~a,~a~%" key target))))))

;; ------------------------------------------------------------------------------------------
;; Tryout 1 (MUHAI meeting Barcelona 6-7 October 2022)
;; ------------------------------------------------------------------------------------------
;; We set the recipe:
;; ------------------
(defparameter *my-great-recipe* nil "Set your recipe here. List of strings.")
(defparameter *my-pdm*  nil "Initialize the conversation memory here.")
(defparameter *my-network* nil "Will contain the network.")

(defun initialize-integrative-network-test ()
  (reset-id-counters)
  (my-initialize-values)
  (setf *my-great-recipe* '(;;;; Ingredients
                                "226 grams butter , room temperature"
                                "116 grams sugar"   
                                "4 grams vanilla extract"
                                "4 grams almond extract"
                                "340 grams flour"
                                "112 grams almond flour"
                                "29 grams powdered sugar"
                                
                                ;;;; Instructions
                                "beat the butter and the sugar together until light and fluffy"
                                "add the vanilla and almond extracts and mix"
                                "add the flour and the almond flour"
                                "mix thoroughly"
                                "take generous tablespoons of the dough and roll it into a small ball , about an inch in diameter , and then shape it into a crescent shape"
                                "place onto a parchment paper lined baking sheet"
                                "bake at 175 C for 15 - 20 minutes" ; or until a light golden brown
                                "dust with powdered sugar"
                                "end"
                                )
        *my-pdm* (initialise-personal-dynamic-memory
                  *fcg-constructions*
                  `((get-kitchen ,(make-var 'kitchen-state))))
        *my-network* (make-integrative-network)))

(defun irl-binding-to-bind-statement (irl-binding)
  "Transforms an IRL binding into a (bind class ?var object) statement."
  `(bind ,(type-of (value irl-binding)) ,(var irl-binding) ,(value irl-binding) ,(available-at irl-binding)))

(defun visualize-linguistic-comprehension (meaning network)
  ;; We use the final solution node but we retrace comprehension node by node.
    (identify-network-updates (make-semantic-network :primitives meaning)
                              network)
    (multiple-value-bind (to-add to-update)
        (handle-node-updates network)
      (vis-add-node (wi::vis-format-many to-add))
      (vis-update-node (wi::vis-format-many to-update)))
    (vis-add-edge (wi::vis-format-many (collect-new-edges network))))

(defun visualize-semantic-interpretation (existing-bindings extended-meaning resulting-bindings-lists network)
  (let* ((open-variables (get-unconnected-vars extended-meaning))
         (resulting-bindings-with-open-variables (mapcar #'(lambda (bindings-list)
                                                             (loop for v in open-variables
                                                                   when (and (or (find v existing-bindings :key #'var)
                                                                                 (find v (all-variables extended-meaning)))
                                                                             (available-at (find v bindings-list :key #'var)))
                                                                   collect (find v bindings-list :key #'var)))
                                                         resulting-bindings-lists)))
    (loop for bindings-list in resulting-bindings-with-open-variables
          do (push (make-instance 'world-state
                                  :accessible-entities bindings-list
                                  :personal-dynamic-memory *my-pdm*)
                   (world-states *my-pdm*)))
    (identify-network-updates (first resulting-bindings-lists) network :exhaustive? nil)
    (identify-network-updates (accessible-entities (first (world-states *my-pdm*)))
                              network
                              :exhaustive? nil)
    (multiple-value-bind (to-add to-update)
        (handle-node-updates *my-network*)
      (vis-add-node (wi::vis-format-many to-add))
      (vis-update-node (wi::vis-format-many to-update)))
    (vis-add-edge (wi::vis-format-many (collect-new-edges *my-network*)))))


(defun my-append-meaning-and-irl-bindings (irl-program irl-bindings)
  "Macroexpands an irl program and appends bind statements from accessible-entities in PDM."
  (append (irl-bindings-to-bind-statements irl-bindings)
          (instantiate-non-variables-in-irl-program irl-program)))

;; We run the network:
;; -------------------
(defun run-the-network ()
  (initialize-integrative-network-test)
  (let ((initial-world-state (first (world-states *my-pdm*)))
        (counter 0))
    (identify-network-updates (accessible-entities initial-world-state) *my-network*
                              :exhaustive? nil)
    ;; We create the initial network:
    (add-element `((div :id "narrativeNetwork")
                   ,(wi::make-vis-network :element-id "narrativeNetwork"
                                          :nodes (handle-node-updates *my-network*)
                                          :edges (collect-new-edges *my-network*))))
    (write-data-to-gephi counter "initial" "" *my-network*)
    (incf counter)
    ;; Now we go through the recipe
    (dolist (utterance *my-great-recipe*)
      ;; Parse the utterance with FCG
      (let* ((parsed-meaning (understand utterance (grammar *my-pdm*) (first (world-states *my-pdm*))))
             (existing-bindings (accessible-entities (first (world-states *my-pdm*))))
             (extended-meaning (my-append-meaning-and-irl-bindings parsed-meaning existing-bindings)))
        (multiple-value-bind (irl-solutions irl-solution-nodes)
            (evaluate-irl-program extended-meaning nil)
          (unless (string= utterance "end")
            (visualize-linguistic-comprehension (irl-program (irl::pip (first irl-solution-nodes)))
                                                *my-network*)
            (write-data-to-gephi counter "fcg" utterance *my-network*))
          (visualize-semantic-interpretation 
           existing-bindings extended-meaning irl-solutions *my-network*)
          (write-data-to-gephi counter "irl" utterance *my-network*)
          (incf counter))))))
; (run-the-network)


|#