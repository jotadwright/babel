;;;; agent.lisp

(in-package :clg)

;; -----------------
;; + Agent Classes +
;; -----------------

(defclass clevr-learning-agent (agent object-w-tasks)
  ((topic :initarg :topic :initform nil
          :accessor topic :type (or null entity number)
          :documentation "The answer for the current question")
   (role :initarg :role :initform 'no-role :type symbol :accessor role
         :documentation "The role of the agent (tutor or learner)")
   (grammar :initarg :grammar :accessor grammar :initform nil
            :type (or null fcg-construction-set)
            :documentation "The agent's grammar")
   (ontology :initarg :ontology :accessor ontology :initform nil
             :type (or null blackboard)
             :documentation "The agent's ontology")
   (available-primitives :initarg :available-primitives
                         :accessor available-primitives
                         :initform nil :type (or null primitive-inventory)
                         :documentation "The primitives available for the agent"))
  (:documentation "Base class for both agents"))

(defclass clevr-learning-tutor (clevr-learning-agent)
  ((question-success-table
    :initarg :success-table :initform nil :type list
    :accessor question-success-table
    :documentation "Agent keeps track of which questions
                               were seen, and how often it was
                               successful or not")
   (current-question-index :initform -1 :accessor current-question-index
                           :type number :documentation "Index of current question"))
   (:documentation "The tutor agent"))

(defclass clevr-learning-learner (clevr-learning-agent)
  ((task-result :initarg :task-result
                :accessor task-result
                :initform nil
                :documentation "Pointer to the result of the task in this interaction")
   (memory :initarg :memory :accessor memory :initform (make-hash-table :test #'eq)
           :documentation "The agent's memory (used by composer strategy)")
   (composer-chunks :initarg :composer-chunks :accessor composer-chunks
                    :initform nil :type list
                    :documentation "The chunks the agent can use for composing"))
  (:documentation "The learner agent"))

(defmethod tutorp ((agent clevr-learning-agent))
  (eql (role agent) 'tutor))

(defmethod learnerp ((agent clevr-learning-agent))
  (eql (role agent) 'learner))

(defun make-clevr-learning-tutor (experiment)
  (make-instance 'clevr-learning-tutor
                 :role 'tutor :experiment experiment
                 :grammar (default-clevr-grammar)
                 :ontology (copy-object *clevr-ontology*)
                 :success-table (loop for i below (length (question-data experiment))
                                      collect (cons i nil))
                 :available-primitives (copy-object *clevr-primitives*)))

(defun make-clevr-learning-learner (experiment)
  (let ((learner
         (make-instance 'clevr-learning-learner
                        :role 'learner :experiment experiment
                        :grammar (empty-cxn-set (get-configuration experiment :hide-type-hierarchy)
                                                (get-configuration experiment :learner-cxn-supplier))
                        :ontology (copy-object *clevr-ontology*))))
    ;; restore concepts
    (set-up-concepts learner)
    ;; set the agent as the owner of the grammar
    (set-data (blackboard (grammar learner)) :owner learner)
    ;; initialise the primitives
    (set-primitives-for-current-challenge-level learner (get-configuration experiment :primitives))
    ;; update the composer chunks
    (update-composer-chunks-w-primitive-inventory learner)
    learner))

(defmethod clear-question-success-table ((agent clevr-learning-tutor))
  (setf (question-success-table agent)
        (loop for i below (length (question-data (experiment agent)))
              collect (cons i nil))))

(defmethod clear-memory ((agent clevr-learning-learner))
  (setf (memory agent)
        (make-hash-table :test #'eq)))

(defmethod copy-object ((learner clevr-learning-learner))
  (make-instance 'clevr-learning-learner))

;;;

(defun set-up-concepts (agent)
  (let* ((grammar (grammar agent))
         (ontology (ontology agent))
         (concepts (cl-store::restore (babel-pathname
                                       :directory `("experiments" 
                                                    "concept-emergence2" 
                                                    "storage"
                                                    "cle4-grammar")
                                       :name (format nil "inventory")
                                       :type "store"))))
    (loop with new-hash-table = (make-hash-table :test 'eq)
          for form being the hash-keys of concepts
            using (hash-value concept)
          
         ; do (setf (gethash (intern (upcase form) 'clevr-world) new-hash-table) concept)
          do (cond ;; spatials
                   ((member form '("behind" "front" "left" "right") :test #'equalp)
                    (add-cxn-and-ontology agent 'spatial-concept concept form 'spatial 'spatial-category new-hash-table))
                   ;; colors
                   ((member form '("blue" "brown" "cyan" "gray" "green" "purple" "red" "yellow") :test #'equalp)
                    (add-cxn-and-ontology agent 'color-concept concept form 'color 'color-category new-hash-table))
                   ;; materials
                   ((member form '("metal" "rubber") :test #'equalp)
                    (add-cxn-and-ontology agent 'material-concept concept form 'material 'material-category new-hash-table))
                   ;; size
                   ((member form '("small" "large") :test #'equalp)
                    (add-cxn-and-ontology agent 'size-concept concept form 'size 'size-category new-hash-table))
                   ;; shapes
                   ((member form '("cube" "cylinder" "sphere") :test #'equalp)
                    (add-cxn-and-ontology agent 'shape-concept concept form 'shape 'shape-category new-hash-table :add-plural t)))
          finally (push-data ontology 'all-concepts new-hash-table))))

(defun add-cxn-and-ontology (agent attribute-class concept form sem-class category-type new-hash-table &key (add-plural nil))
  (let ((grammar (grammar agent))
        (ontology (ontology agent))
        (clg-concept (make-instance attribute-class
                                    :id (intern (upcase form) :clevr-world)
                                    :meaning concept)))
    (setf (gethash (intern (upcase form) 'clevr-world) new-hash-table) clg-concept)
    ;; add it to the ontology of the agent
    (push-data ontology attribute-class clg-concept)
    ;; add the morph
    ;(add-morph-cxn-for-concept grammar clg-concept form)
    ;; add the lex
    (loop for synonym in (get-synonyms form)
          do (add-lex-cxn-for-concept agent grammar clg-concept synonym sem-class category-type :add-plural add-plural))
    (add-lex-cxn-for-concept agent grammar clg-concept form sem-class category-type :add-plural add-plural)))


(defun get-synonyms (form)
  ;; This function will read synonyms.json and read all keys
  ;; (e.g. thing, sphere, left of, ...). For each key, get the
  ;; list of synonyms and generate morphological constructions that
  ;; will map to the lexical construction of the key, created in
  ;; previous function.
  
  ;; IMPORTANT! The file synonyms.json contains entries for "above"
  ;; and "below", but these are not specified in the types in metadata.json
  ;; (so they have no lexical cxns). For the moment, these are left out.
  ;; One option would be to consider these as synonyms for "behind" and
  ;; "in front of", but they are not specified in the dataset as such.

  ;; Also, metadata.json contains 'cylinder' as a shape, but there are
  ;; no synonyms specified in synonyms.json. So, we add the morph cxns
  ;; for cylinder manually.
  (let* ((synonyms-file (babel-pathname :directory '("grammars" "clevr-grammar" "data")
                                        :name "synonyms" :type "json"))
         (synonyms (decode-json-from-source synonyms-file)))
    (rest (assqv (intern (upcase form) :keyword) synonyms))))


(defmethod add-lex-cxn-for-concept (agent cxn-inventory concept word sem-class category-type &key (add-plural nil)) ; (type (eql :*shape)))
  (let* ((lex-id (upcase (id concept)))
         (cxn-name (internal-symb (upcase (string-append (hyphenize word) "-lex-cxn"))))
         (cxn-pl-name (internal-symb (upcase (string-append (hyphenize word) "s-lex-cxn"))))
         (unit-name (make-var (upcase (string-append (hyphenize lex-id) "-unit"))))
         (out-var (make-var sem-class))
         (initial-cxn-score (get-configuration agent :initial-cxn-score)))

    ;; singular
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (syn-cat (phrase-type lexical)
                                   (fcg::lex-class ,(intern (symbol-name (make-const word)) :fcg)))
                          (args (,out-var))
                          )
                         <-
                         (,unit-name
                          (HASH meaning ((bind ,category-type ,out-var ,(internal-symb (hyphenize lex-id)))))
                          --
                          (HASH form ((string ,unit-name ,word)))
                          ))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-lex)
                        :attributes (:score ,initial-cxn-score
                                     :cxn-type lexical
                                     :repair concept-learning ;; TODO
                                     :string ,word
                                     :meaning ,(internal-symb (hyphenize lex-id))
                                     ;:lex-id ,(internal-symb (hyphenize lex-id)) 
                                     ;:clevr-datatype ,(symbol-name type)
                                     )))

    ;; plural
    #|(when add-plural   
      (eval `(def-fcg-cxn ,cxn-pl-name
                          ((,unit-name
                            (syn-cat (phrase-type lexical)
                                     (fcg::lex-class ,(intern (symbol-name (make-const word)) :fcg)))
                            (args (,out-var))
                            )
                           <-
                           (,unit-name
                            (HASH meaning ((bind shape-category ,out-var ,(internal-symb (hyphenize lex-id)))))
                            --
                            (HASH form ((string ,unit-name ,(concatenate 'string word "s"))))
                            ))
                          :cxn-inventory ,cxn-inventory
                          :cxn-set (hashed hashed-lex)
                          :attributes (:score ,initial-cxn-score
                                       :cxn-type lexical
                                       :repair concept-learning ;; TODO
                                       :string ,word
                                       :meaning ,(internal-symb (hyphenize lex-id))
                                     ;:lex-id ,(internal-symb (hyphenize lex-id)) 
                                     ;:clevr-datatype ,(symbol-name type)
                                       ))))|#))

