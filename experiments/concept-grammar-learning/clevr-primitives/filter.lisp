(in-package :clg)

;; case: second case: given source set, compute category and target-set
#|(loop for category2 in (loop for top-cat in (list 'cw::materials 'cw::sizes 'cw::shapes 'cw::colors)
                                append (get-data ontology top-cat))
         for computed-set = (filter-by-concept source-set category2 ontology)
         do (bind (category 1.0 category2)
                  (target-set 1.0 computed-set)))|#

;; --------------------
;; + filter primitive +
;; --------------------

(defprimitive filter ((target-set clevr-object-set)
                      (source-set clevr-object-set)
                      (category attribute))
  ;; first case: if given source-set and category, compute target-set
  ((source-set category => target-set)
   (let ((computed-set (filter-by-concept source-set category ontology)))
     (bind (target-set 1.0 computed-set))))
  ;; second case: given source set, compute category and target-set
  ;; invention case: make all possible combinations of objects in the source-set,
  ;; for each of those combinations find a concept that filters those successfully (using threshold) + max-iterations
  ;; if concept is found, make target-set with as score a combination of the iterations that were needed + similarity scores
  ((source-set => category target-set)
   
   ;; todo: target-set of length 0 case must still be solved
   (loop for combination-length from 1 to (length (objects source-set))
         ;do (format t "~% next iteration: ~a out of ~a" combination-length (length (objects source-set)))
         do (loop with all-combinations = (combinations-of-length (objects source-set) combination-length)
                  for target-objects in all-combinations
                  for candidate-target-set = (make-instance 'clevr-object-set
                                                            :objects target-objects
                                                            :similarities nil)
                    
                  for candidate-concept-and-iteration = (multiple-value-list (create-initial-concept-hypothesis ontology source-set candidate-target-set))
                  for candidate-concept = (first candidate-concept-and-iteration)
                  for iteration = (second candidate-concept-and-iteration)
                  for similarity = (sum (second (find candidate-concept (third candidate-concept-and-iteration) :key #'first)))
                  when candidate-concept
                    ;; todo: for target-set -> use similarity score as binding-score + as little as possible updates
                    do (progn
                         (let ((cat (make-instance 'attribute :id (id candidate-concept))))
                          ; (add-element `((h) ,(format nil "iterations needed for concept: ~a" iteration)))
                          ; (concept-representations::add-concept-to-interface (meaning candidate-concept) :weight-threshold 0.5)
                           (push-data ontology 'categories cat)
                           (push-data ontology 'candidate-concepts candidate-concept)
                           (bind (category 1.0 cat)
                                 (target-set similarity candidate-target-set)))))))
  ;; third case: consistency check
  ((target-set source-set category => )
   (filter-by-concept-updates ontology target-set source-set category)
   )
  
  :primitive-inventory *clevr-primitives*)

;; Utility functions

;; -------------------------------------
;; + case 1: filter (source, category) +
;; -------------------------------------
(defmethod filter-by-concept ((source-set clevr-object-set)
                              (category category)
                              (ontology blackboard))
  "Filter the set by the given category."
  (let ((candidate-concept (find (id category) (find-data ontology 'candidate-concepts) :test #'eq :key #'id)))
    (if candidate-concept
      ;; case 1: associated concept is candidate, so filter based on that
      (filter-by-concept-with-threshold source-set candidate-concept ontology)
      (filter-by-concept-with-threshold source-set
                                        (gethash (id category) (find-data ontology 'concepts))
                                        ontology)
      ;(filter-by-concept-no-threshold source-set category ontology)
      )))


(defmethod filter-by-concept-no-threshold ((source-set clevr-object-set)
                                           category
                                           (ontology blackboard))
  (let ((filtered-objects (filter-objects-by-concept-competition ontology category (objects source-set))))
        (if filtered-objects
          (make-instance 'clevr-object-set
                         :objects (first filtered-objects)
                         :similarities (cons (list category (second filtered-objects)) (similarities source-set)))
          (make-instance 'clevr-object-set
                         :id (make-id 'empty-set)))))


(defmethod filter-objects-by-concept-competition (ontology category entities)
  "Filters a set of given entities given a category.

   Which entities have higher similarity with a concept associated to category than to the other concepts

   Example: all entities for which the similarity with category 'small' is higher than 'large'."
  (loop with concepts = (get-competing-concepts ontology category)
        for entity in entities
        for (best-concept . similarity) = (find-best-concept concepts entity)
        when (and best-concept (eq (id category) (id best-concept)))
          collect entity into entities
        collect (cons (id entity) similarity) into similarities
        finally (return (list entities similarities))))



(defun get-competing-concepts (ontology category)
  "Given a category, find the associated concept and its competing concepts.

   For example, given <small>, the function returns (<concept-small> <concept-large>)."
  (cond ;; colors
        #|((member (id category) (get-data ontology 'clg::color-concept) :test #'eq :key #'id)
         (get-data ontology 'clg::color-concept))
        ;; materials
        ((member (id category) (get-data ontology 'clg::material-concept) :test #'eq :key #'id)
         (get-data ontology 'clg::material-concept))
        ;; size
        ((member (id category) (get-data ontology 'clg::size-concept) :test #'eq :key #'id)
         (get-data ontology 'clg::size-concept))|#
        ;; shapes

        ((gethash (id category) (get-data ontology 'concepts))
         (hash-values (get-data ontology 'concepts)))))

(defun find-best-concept (concepts entity)
  (loop with best-concept = nil
        with best-similarity = nil
        for concept in concepts
        for similarity = (concept-representations::concept-entity-similarity (meaning concept) entity)
        when (or (null best-concept)
                 (> similarity best-similarity))
          do (setf best-concept concept
                   best-similarity similarity)
        finally (return (cons best-concept best-similarity))))

(defun get-associated-concept (ontology category)
  (gethash category (get-data ontology 'all-concepts)))


;; ---------------------------------------
;; + case 2: filter (source) - invention +
;; ---------------------------------------
(defun create-initial-concept-hypothesis (ontology source-set target-set)
  (loop with clg-concept = (make-instance 'clg-concept :meaning (concept-representations::create-concept-representation (objects target-set) :weighted-multivariate-distribution))
        ;; todo return iterations required as heuristic for target-set binding score
        with max-iterations = (get-data ontology 'clg::max-concept-update-iterations)
        with current-iteration = 0
        with excluded-objects = (set-difference (objects source-set) (objects target-set))
        with solution = nil
        while (< current-iteration max-iterations)
        for not-used = (loop for object in (objects target-set)
                             ;; todo: update concept i.f.o target-set en not per object
                             ;; possible solution: use saliency
                             do (concept-representations::update-concept (meaning clg-concept)
                                                                         object
                                                                         excluded-objects))
        ;for category = (make-instance 'attribute :id (id concept))
        for computed-set = (filter-by-concept-with-threshold source-set clg-concept ontology)
        
        #|when (eq (mod current-iteration 10) 0)
            do (format t "~% [~a] ~a" current-iteration (id concept))|#
        ;; todo: check correctness
        when (and (equal (length (objects computed-set)) (length (objects target-set)))
                  (length= (set-difference (objects computed-set) (objects target-set)) 0))
          do (progn
               (setf solution clg-concept)
               (loop-finish))
        do (incf current-iteration)
        finally (return (values solution current-iteration (similarities computed-set)))))
      
(defmethod filter-by-concept-with-threshold ((source-set clevr-object-set)
                                             concept
                                             (ontology blackboard)
                                             &key (threshold 0.5))
  "Filter the set by the given category."
  (let* ((threshold (if (find-data ontology 'filter-similarity-threshold) (find-data ontology 'filter-similarity-threshold) threshold))
         (filtered-objects (filter-objects-by-concept ontology concept (objects source-set) threshold)))
    (if filtered-objects
      (make-instance 'clevr-object-set
                     :objects (first filtered-objects)
                     :similarities (cons (list concept (second filtered-objects)) (similarities source-set)))
      (make-instance 'clevr-object-set
                     :id (make-id 'empty-set)))))


(defmethod filter-objects-by-concept (ontology concept entities threshold)
  ""
  (loop for entity in entities
        for similarity = (concept-representations::concept-entity-similarity (meaning concept) entity)
        when (> similarity threshold)
          collect entity into entities
         and  collect similarity into similarities
        finally (return (list entities similarities))))
          





















;; --------------
;; + UNUSED!!!! +
;; --------------

(defun filter-by-concept-updates (ontology target-set source-set category)
  ;; (add-element `((h2) ,(format nil "-----start filter-----")))
  ;; (concept-representations::add-concept-to-interface (meaning (get-associated-concept ontology (id category))) :weight-threshold 0.5)

  (format t "~%Going to update a concept to make it filter a particular target set from a source")
  (format t "~% Initial concept: ~a" (id category))
  (format t "~% Initial source-set: ~a" (objects source-set))
  (format t "~% Initial target-set: ~a" (objects target-set))

  (let* ((concept (get-associated-concept ontology (id category)))
         (target-objects (objects target-set))
         (other-objects (set-difference (objects source-set) (objects target-set))))
    (format t "~% Initial similarities")
    (debug-concept-updates concept target-objects other-objects))
    
  
  (let* ((original-concepts (copy-object (get-data ontology 'original-concepts)))
         ;;(original-ontology (copy-object  (get-data ontology 'original-ontology)))
         ;; loop and update-concept, until this leads to the correct solution or max-iterations is reached.
         (consistent-p (loop with concept = (get-associated-concept ontology (id category))
                             with target-objects = (objects target-set) ;; to remove
                             with other-objects = (set-difference (objects source-set) (objects target-set))
                             with max-iterations = 1000
                             with current-iteration = 0
                             while (< current-iteration max-iterations)
                             for not-used = (loop for object in (objects target-set)
                                                  ;; misschien update concept doen obv target set en niet per object, dus saliency 
                                                  do (concept-representations::update-concept (meaning concept)
                                                                                              object
                                                                                              other-objects))
                             for computed-set = (objects (filter-by-concept source-set category ontology)) ;; to remove objects
                             do (incf current-iteration)

                             ;; debug mode
                             do (progn
                                  (format t "~% === Iteration: ~a ===" current-iteration)
                                  (format t "~%  - Resulting target-set: ~a" computed-set)
                                  (format t "~%  - New similarities")
                                  (debug-concept-updates concept target-objects other-objects)
                                  )

                             ;; todo equal klopt niet
                             when (and (equal (length computed-set) (length target-objects))
                                       (equal computed-set target-objects))
                               return t)))

     ;(add-element `((h2) ,(format nil "-----end filter----")))
     ;(concept-representations::add-concept-to-interface (meaning (get-associated-concept ontology (id category))) :weight-threshold 0.5)
    
    (if consistent-p
      (print "yes")
      (progn
        ;; only set this category back instead of all concepts when multiple filters
        (set-data ontology 'all-concepts (copy-object original-concepts))
         ;(setf ontology  original-ontology)
        ))
    consistent-p))


(defun debug-concept-updates (concept target-objects other-objects)
  (loop for obj in target-objects
        do (format t "~%    - ~a (u): ~,3f"
                   (id obj)
                   (concept-representations::concept-entity-similarity (meaning concept)
                                                                       obj)))
  (format t "~%    ---")
  (loop for obj in other-objects
        do (format t "~%    - ~a (d): ~,3f"
                   (id obj)
                   (concept-representations::concept-entity-similarity (meaning concept)
                                                                       obj))))


