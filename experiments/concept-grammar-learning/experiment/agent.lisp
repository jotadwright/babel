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
                         :documentation "The primitives available for the agent")
   (gold-standard-program :initarg :gold-standard-program
                          :accessor gold-standard-program
                          :initform nil
                          :type list))
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
  (let ((tutor
         (make-instance 'clevr-learning-tutor
                        :role 'tutor :experiment experiment
                        :grammar (default-clevr-grammar)
                        :ontology (make-blackboard)
                        :success-table (loop for i below (length (question-data experiment))
                                             collect (cons i nil))
                        :available-primitives (copy-object *clevr-primitives*))))
    (push-data (ontology tutor) 'booleans (make-instance 'boolean-category :id 'yes :bool t))
    (push-data (ontology tutor) 'booleans (make-instance 'boolean-category :id 'no :bool nil))

    (loop for concept in (list "blue" "brown" "cyan" "gray" "green" "purple" "red" "yellow" "metal" "rubber" "small" "large" "cube" "cylinder" "sphere")
          do (push-data (ontology tutor) 'concepts (make-instance 'clg-concept :id (intern (upcase concept)))))
    tutor))



(defun make-clevr-learning-learner (experiment)
  (let ((learner (make-instance 'clevr-learning-learner
                                :role 'learner :experiment experiment
                                :grammar (empty-cxn-set (get-configuration experiment :hide-type-hierarchy)
                                                        (get-configuration experiment :learner-cxn-supplier)
                                                        (get-configuration experiment :diagnostics)
                                                        (get-configuration experiment :repairs))
                                :ontology (make-blackboard))))

    ;; to access configurations
    (set-data (ontology learner) 'owner learner)

    (push-data (ontology learner) 'booleans (make-instance 'boolean-category :id 'yes :bool t))
    (push-data (ontology learner) 'booleans (make-instance 'boolean-category :id 'no :bool nil))

    

    ;; setup pretrained-concepts or not
    (if (get-configuration experiment :pretrained-concepts)
      ;; setup
      ;;    1. concepts pretrained (using tutor-learner concept learning) or
      ;;    2. randomly setup n concepts
      (set-up-concepts learner)
      ;; load-concepts 
      (set-data (ontology learner) 'concepts (make-hash-table :test #'eq)))

    ;; setup concepts
    (set-data (ontology learner) 'max-concept-update-iterations (get-configuration experiment :max-concept-update-iterations))
    (set-data (ontology learner) 'filter-similarity-threshold (get-configuration experiment :filter-similarity-threshold))
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

;;;;;;;;;;;;;;;;;;;;;;
;; loading concepts

(defun load-pretrained-concepts (agent)
  (cl-store::restore (babel-pathname
                      :directory `("experiments" 
                                   "concept-emergence2" 
                                   "storage"
                                   "cle4-grammar")
                      :name (format nil "inventory-~a" (get-configuration agent :data-source))
                      :type "store")))


(defun find-concept-given-category (ontology category)
  (gethash category (gethash 'category-to-concept (get-data ontology 'category-concept-map))))

(defun find-category-given-concept (ontology concept)
  (gethash concept (gethash 'concept-to-category (get-data ontology 'category-concept-map))))

(defun add-category-and-concept-to-ontology (ontology concept category)
  (setf (gethash category (gethash 'category-to-concept (get-data ontology 'category-concept-map))) concept)
  (setf (gethash concept (gethash 'concept-to-category (get-data ontology 'category-concept-map))) category)
  )

(defmethod set-up-concepts (agent)
  (let* ((grammar (grammar agent))
         (ontology (ontology agent))
         (concepts (load-pretrained-concepts agent)))

    (let ((cat-to-concept-table (make-hash-table :test 'eq))
          (concept-to-cat-table (make-hash-table :test 'eq))
          (table (make-hash-table :test 'eq)))
      (setf (gethash 'concept-to-category table) cat-to-concept-table)
      (setf (gethash 'category-to-concept table) concept-to-cat-table)    
      (set-data ontology 'category-concept-map table))
    
    (loop with table = (make-hash-table :test 'eq)
          for form being the hash-keys of concepts
            using (hash-value concept) and idx from 0
          do (cond ;; spatials
                   ;((member form '("behind" "front" "left" "right") :test #'equalp)
                   ; (add-cxn-and-ontology agent 'spatial-concept concept form 'spatial 'spatial-category table))
                   ;; colors
                   ((member form '("blue" "brown" "cyan" "gray" "green" "purple" "red" "yellow") :test #'equalp)
                    (add-cxn-and-ontology agent 'clg-concept concept form 'concept 'clg-concept table))
                   ;; materials
                   ((member form '("metal" "rubber") :test #'equalp)
                    (add-cxn-and-ontology agent 'clg-concept concept form 'concept 'clg-concept table))
                   ;; size
                   ((member form '("small" "large") :test #'equalp)
                    (add-cxn-and-ontology agent 'clg-concept concept form 'concept 'clg-concept table))
                   ;; shapes
                   ((member form '("cube" "cylinder" "sphere") :test #'equalp)
                    (add-cxn-and-ontology agent 'clg-concept concept form 'concept 'clg-concept table :add-plural t)))
          finally (set-data ontology 'concepts table))))

(defun add-cxn-and-ontology (agent attribute-class concept form sem-class category-type table &key (add-plural nil))
  (let ((grammar (grammar agent))
        (ontology (ontology agent))
        (clg-concept (make-instance attribute-class
                                    :id (intern (upcase form))
                                    ;; copy concept as they otherwise would be shared by different synonyms
                                    :meaning (copy-object concept)))) 
    ;; add the concept to the table
    (setf (gethash (intern (upcase form)) table) clg-concept)

    ;; create an attribute table if it doesnt exist yet
    #|(when (not (find-data ontology attribute-class))
      (set-data ontology attribute-class (make-hash-table :test #'eq)))|#
    ;; add the concept to its attribute class table
    ;(setf (gethash (id clg-concept) (get-data ontology attribute-class)) clg-concept)
    ;; create lexical constructions for the concepts
    #|(loop for synonym in (get-synonyms form)
          for clg-concept = (make-instance attribute-class
                                            :id (intern (upcase synonym))
                                            :meaning (copy-object concept))
          do (setf (gethash (intern (upcase synonym) ) table) clg-concept)
          do (setf (id (meaning clg-concept)) (intern (upcase synonym)))
          ;do (setf (gethash (id clg-concept) (get-data ontology attribute-class)) clg-concept)
          do (add-lex-cxn-for-concept agent grammar clg-concept synonym sem-class category-type attribute-class table ontology :add-plural add-plural))|#
    (add-lex-cxn-for-concept agent grammar clg-concept form sem-class category-type attribute-class table ontology :add-plural add-plural)))


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
  (let* ((synonyms-file (babel-pathname :directory '("experiments" "concept-grammar-learning" "clevr-grammar" "data")
                                        :name "synonyms" :type "json"))
         (synonyms (decode-json-from-source synonyms-file)))
    (rest (assqv (intern (upcase form) :keyword) synonyms))))


(defmethod add-lex-cxn-for-concept (agent cxn-inventory concept word sem-class category-type attribute-class table ontology &key (add-plural nil))
  (let* ((lex-id (upcase (id concept)))
         (cxn-name (internal-symb (upcase (string-append (hyphenize word) "-lex-cxn"))))
         (cxn-pl-name (internal-symb (upcase (string-append (hyphenize word) "s-lex-cxn"))))
         (unit-name (make-var (upcase (string-append (hyphenize lex-id) "-unit"))))
         (out-var (make-var sem-class))
         (initial-cxn-score (get-configuration agent :initial-cxn-score))
         (lex-class-name-1 (intern (symbol-name (make-const word)) :fcg))
         (lex-class-name-2 (intern (symbol-name (make-const (concatenate 'string word "s"))) :fcg))
         (category-id (internal-symb (hyphenize lex-id))))

    ;; singular
    (add-category-and-concept-to-ontology ontology (id concept) lex-class-name-1)
    ;(push-data ontology 'list-of-things-to-make-it-work concept)
    
    ;(push-data ontology 'categories (make-instance 'category :id (id concept)))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (syn-cat (phrase-type lexical)
                                   (fcg::lex-class ,lex-class-name-1))
                          (args (,out-var))
                          )
                         <-
                         (,unit-name
                          (HASH meaning ((bind ,category-type ,out-var ,category-id)))
                          --
                          (HASH form ((string ,unit-name ,word)))
                          ))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-lex)
                        :attributes (:score ,initial-cxn-score
                                     :cxn-type lexical
                                     :repair concept-learning ;; TODO
                                     :string ,word
                                     :construction-category ,lex-class-name-1
                                     :meaning ,category-id)))
    ;; plural
    (when add-plural
      (let ((clg-concept (make-instance attribute-class
                                        :id (intern (upcase (concatenate 'string word "s")))
                                        :meaning (copy-object (meaning concept)))))
        (setf (id (meaning clg-concept)) (intern (upcase (concatenate 'string word "s"))))
        (setf (gethash (intern (upcase (concatenate 'string word "s"))) table) clg-concept)
        ;(setf (gethash (id clg-concept) (get-data ontology attribute-class)) clg-concept)
  
        ;(push-data (ontology agent) 'cat-mapper (cons lex-class-name-2 (id clg-concept)))
        (add-category-and-concept-to-ontology ontology (id clg-concept) lex-class-name-2)
        ;(push-data ontology 'list-of-things-to-make-it-work clg-concept)
        
        (eval `(def-fcg-cxn ,cxn-pl-name
                            ((,unit-name
                              (syn-cat (phrase-type lexical)
                                       (fcg::lex-class ,lex-class-name-2))
                              (args (,out-var))
                              )
                             <-
                             (,unit-name
                              (HASH meaning ((bind ,category-type ,out-var ,(intern (upcase (concatenate 'string word "s"))))))
                              --
                              (HASH form ((string ,unit-name ,(concatenate 'string word "s"))))
                              ))
                            :cxn-inventory ,cxn-inventory
                            :cxn-set (hashed hashed-lex)
                            :attributes (:score ,initial-cxn-score
                                         :cxn-type lexical
                                         :repair concept-learning ;; TODO
                                         :string ,(concatenate 'string word "s")
                                         :construction-category ,lex-class-name-2
                                         :meaning ,(intern (upcase (concatenate 'string word "s"))))))))))

;; utility function
(defun get-feature-names (experiment)
  (let* ((world (world experiment))
         (scene-name (first (utils::hash-keys (scenes world))))
         (loaded-scene (find-scene-by-name scene-name world))
         ;; take a random object to get the feature names
         (object (first (objects loaded-scene)))
         (feature-names (utils::hash-keys (features object))))
    feature-names))



;; unused

#|(defmethod set-up-concepts (agent (mode (eql :random-initialised-concepts)))
  (loop with ontology = (ontology agent)
        with feature-names = (get-feature-names (experiment agent))
        with table = (make-hash-table :test 'equalp)
        for idx from 1 to 15 ;; TODO: make configurable
        for form = (format nil "concept-~a" idx)
        for concept = (concept-representations::create-concept-representation feature-names :weighted-multivariate-distribution)
        do (cond #|((< idx 8) ;; todo
                  (add-cxn-and-ontology agent 'color-concept concept form 'color 'color-category table))
                 ((< idx 10) ;; todo
                  (add-cxn-and-ontology agent 'material-concept concept form 'material 'material-category table))
                 ((< idx 12) ;; todo 
                  (add-cxn-and-ontology agent 'size-concept concept form 'size 'size-category table))|#
                 ;; shapes
                 ((< idx 15) ;; todo
                  (add-cxn-and-ontology agent 'shape-concept concept form 'shape 'shape-category table :add-plural t)))
        finally (progn
                  (set-data ontology 'all-concepts table)
                  (overwrite-ontology-category-ids ontology))))|#

#|(defun overwrite-ontology-category-ids (ontology)

  (loop for getter-categories in (list  'cw::shapes)
        for getter-concepts in (list 'shape-concept)
        for slot-name in (list  'cw::shape)
        for categories = (get-data ontology getter-categories)
        for concepts = (get-data ontology getter-concepts)
        do (loop for category in categories
                 for concept in concepts
                 do (setf (id category) (id concept))
                 do (setf (slot-value category slot-name) (id concept)))))|#