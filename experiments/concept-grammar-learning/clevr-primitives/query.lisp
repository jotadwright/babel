(in-package :clg)

;; -------------------
;; + QUERY primitive +
;; -------------------


(defprimitive query ((concept clg-concept)
                     (source-object clevr-object)
                     (category category))
  ;; first case; given attribute and source-object, compute the target category
  ((source-object category => concept)
   (let* ((highest-concept (query-object-based-on-category source-object category ontology)))
     (bind (concept 1.0 (car highest-concept)))))

  ;; second case; given source-object, compute pairs of attribute and target-category
  ((source-object => category concept)
     (loop for candidate-category in (get-categories ontology)
           for (candidate-concept . sim) = (query-object-based-on-category source-object candidate-category ontology)
           when candidate-concept
             do (bind (category 1.0 candidate-category)
                      (concept sim candidate-concept))))
  
  :primitive-inventory *clevr-primitives*)

;; Utilities
(defmethod query-object-based-on-category ((object clevr-object)
                                           (category category)
                                           ontology)
  (let* ((candidate-concepts (concepts category))
         (highest-concept-and-sim (find-best-concept candidate-concepts object))
         (best-concept (car highest-concept-and-sim)) 
         (similarity (cdr highest-concept-and-sim)))
    (cons (find-entity-by-id ontology (id best-concept)) similarity)))


(defun get-categories (ontology)
  ;; assumption that we know how many categories there are + that we have learned them in a previous phase of the game.
  (if (get-configuration-from-ontology ontology :nr-of-categories)
    (let* ((agent (find-data ontology 'owner))
           (categories (get-data ontology 'categories)))
    #|(let* ((agent (find-data ontology 'owner))
           (categorial-network (categorial-network (grammar agent)))
           (similarities-hash-table (calculate-all-similarities-of-concepts agent)))
      
      similarities-hash-table)|#

      categories)))


    
    
 
(defun calculate-all-similarities-of-concepts (agent)
  (let ((ontology (ontology agent))
        (similarities-ht (make-hash-table))
        (cats-and-binds (loop for cxn in (constructions-list (grammar agent))
                              for meaning = (find-meaning cxn)
                              for category = (construction-category cxn)
                              when (eq (cxn-type cxn) 'clg::lexical)
                                collect (cons category meaning))))
    (loop for cat-and-bind in cats-and-binds
          for cat = (first cat-and-bind)
          for bind = (cdr cat-and-bind)
          do (let* ((similarities-of-category (find-competing-candidate-categories agent cat))
                    (similarities-of-category-return-binds
                     (mapcar #'(lambda (x) (cons (cdr (find (first x) cats-and-binds :key #'first)) (cdr x))) similarities-of-category)))
                 (setf (gethash bind similarities-ht) similarities-of-category-return-binds)))
    ;(set-data (ontology agent) 'category-similarities similarities-ht)
    
    #|(loop for concept in (hash-keys similarities-ht)
          for concepts-of-category = (loop for (concept-id . similarity) in (gethash concept similarities-ht)
                                           for c = (gethash concept-id (get-data ontology 'concepts))
                                           when (> similarity (get-configuration-from-ontology ontology :category-strategy-threshold))
                                           collect (cons c 0))
          collect (make-instance 'category :concepts concepts-of-category))|#
    similarities-ht))


  
#|(defun get-candidate-concepts (ontology attribute-category)
  (cond ((eq (id attribute-category) 'clevr-world::color)
         (hash-values (get-data ontology 'clg::color-concept)))
        ((eq (id attribute-category) 'clevr-world::material)
         (hash-values (get-data ontology 'clg::material-concept)))
        ((eq (id attribute-category) 'clevr-world::size)
         (hash-values (get-data ontology 'clg::size-concept)))
        ((eq (id attribute-category) 'clevr-world::shape)
         (hash-values (get-data ontology 'clg::shape-concept)))))|#