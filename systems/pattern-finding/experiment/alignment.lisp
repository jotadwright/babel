;;;; alignment.lisp

(in-package :pf)

(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))


(defun delete-cxn-and-grammatical-categories (cxn cxn-inventory)
  "Delete the grammatical categories that occur in cxn"
  (let* ((categories-to-remove
          (cons (extract-contributing-category cxn)
                (extract-conditional-categories cxn)))
         (links-to-remove
          (loop with network = (categorial-network cxn-inventory)
                for gram-cat in categories-to-remove
                for neighbours = (neighbouring-categories gram-cat network)
                append (loop for n in neighbours
                             collect (cons gram-cat n))))
         (alter-ego-cxn (alter-ego-cxn cxn cxn-inventory)))
    (loop for (from . to) in links-to-remove
          do (remove-link from to (categorial-network cxn-inventory)
                          :recompute-transitive-closure nil))
    (remove-categories categories-to-remove (categorial-network cxn-inventory)
                       :recompute-transitive-closure nil)
    (delete-cxn (name cxn) cxn-inventory :key #'name)
    (delete-cxn (name alter-ego-cxn) cxn-inventory :key #'name)
    t))
                            


(defun inc-cxn-score (agent cxn &key (delta 0.1))
  "increase the score of the cxn"
  (let* ((alter-ego-cxn (alter-ego-cxn cxn (grammar agent)))
         (current-score (attr-val cxn :score))
         (new-score (+ current-score delta)))
    (setf (attr-val cxn :score) new-score)
    (setf (attr-val alter-ego-cxn :score) new-score)
    cxn))


(defun dec-cxn-score (agent cxn &key (delta 0.1) (lower-bound 0.0))
  "decrease the score of the cxn."
  (let* ((alter-ego-cxn (alter-ego-cxn cxn (grammar agent)))
         (current-score (attr-val cxn :score))
         (new-score (- current-score delta)))
  (setf (attr-val cxn :score) new-score)
  (setf (attr-val alter-ego-cxn :score) new-score)
  (when (and (get-configuration (experiment agent) :remove-cxn-on-lower-bound)
             (<= (attr-val cxn :score) lower-bound))
    (delete-cxn-and-grammatical-categories cxn (grammar agent)))))



(defmethod run-alignment ((agent pattern-finding-agent) solution-cipn competing-cipns (strategy (eql :most-recent-generalisation)))
  "Run alignment by rewarding the applied cxns and punishing the
   cxns that were used for anti-unification, since more general cxns
   have been learned instead."                                      
  (notify alignment-started)

  ;; align categorial links
  (loop with categorial-network = (categorial-network (construction-inventory solution-cipn))
        for (cat-1 . cat-2) in (extract-used-categorial-links solution-cipn)
        ;; default delta is 0.1
        do (incf-link-weight cat-1 cat-2 categorial-network))

  ;; reward used cxns, except if they are just learned
  (let ((applied-cxns (original-applied-constructions solution-cipn)))
    (loop with cxn-delta = (get-configuration agent :cxn-incf-score)
          with interaction-nr = (interaction-number (current-interaction (experiment agent)))
          with rewarded-cxns = nil
          for cxn in applied-cxns
          for just-learned-p = (string= (format nil "@~a" interaction-nr)
                                        (attr-val cxn :learned-at))
          unless just-learned-p
          do (inc-cxn-score agent cxn :delta cxn-delta)
             (push cxn rewarded-cxns)
          finally (notify cxns-rewarded rewarded-cxns)))

  ;; punish anti-unified cxns
  (let ((cxns-to-punish (find-data (blackboard (construction-inventory solution-cipn)) :anti-unified-cxns)))
    (dolist (cxn cxns-to-punish)
      (dec-cxn-score agent cxn :delta (get-configuration (experiment agent) :cxn-decf-score)))
    (notify cxns-punished cxns-to-punish)))
    




(defmethod run-alignment ((agent pattern-finding-agent) solution-cipn competing-cipns (strategy (eql :lateral-inhibition)))
  (notify alignment-started)
  
  ;; align categorial links
  (loop with categorial-network = (categorial-network (construction-inventory solution-cipn))
        for (cat-1 . cat-2) in (extract-used-categorial-links solution-cipn)
        ;; default delta is 0.1
        do (incf-link-weight cat-1 cat-2 categorial-network))

  
  (let ((applied-cxns (original-applied-constructions solution-cipn)))
    ;; reward used cxns, except if they are just learned
    (loop with cxn-delta = (get-configuration agent :cxn-incf-score)
          with interaction-nr = (interaction-number (current-interaction (experiment agent)))
          with rewarded-cxns = nil
          for cxn in applied-cxns
          for just-learned-p = (string= (format nil "@~a" interaction-nr)
                                        (attr-val cxn :learned-at))
          unless just-learned-p
          do (inc-cxn-score agent cxn :delta cxn-delta)
             (push cxn rewarded-cxns)
          finally (notify cxns-rewarded rewarded-cxns))
    ;; punish competitors, except if they are just learned
    (let* ((cxns-of-competing-solutions
            (loop for cipn in competing-cipns
                  for competitor-cxns = (set-difference (applied-constructions cipn)
                                                        (applied-constructions solution-cipn) :key #'name)
                  append (mapcar #'original-cxn competitor-cxns)))
           (other-applied-cxns
            ; collect cxns of leaf nodes that are not solution nodes
            (loop for cipn in (all-leaf-nodes (cip solution-cipn))
                  unless (or (eq cipn solution-cipn)
                             (find cipn competing-cipns :test #'eq)
                             (find 'fcg::initial (statuses cipn)))
                    append (original-applied-constructions cipn)))
           (cxns-to-punish
            (set-difference
             (remove-duplicates
              (loop with interaction-nr = (interaction-number (current-interaction (experiment agent)))
                    for cxn in (append cxns-of-competing-solutions other-applied-cxns)
                    for just-learned-p = (string= (format nil "@~a" interaction-nr)
                                                  (attr-val cxn :learned-at))
                    unless just-learned-p collect cxn))
             applied-cxns)))
      (dolist (cxn cxns-to-punish)
        (dec-cxn-score agent cxn :delta (get-configuration (experiment agent) :cxn-decf-score)))
      (notify cxns-punished cxns-to-punish))))
      
