(in-package :fcg) 

;;;;;;;;;;;;;;;
;;           ;;
;; Alignment ;;
;;           ;;
;;;;;;;;;;;;;;;


(defgeneric align (solution-node cip solution-node-after-learning applied-fixes mode))

(defmethod align ((solution-node t)
                  (cip construction-inventory-processor)
                  (solution-node-after-learning t)
                  (applied-fixes list)
                  (mode (eql :no-alignment)))
  "No alignment")

(defmethod align ((solution-node t)
                  (cip construction-inventory-processor)
                  (solution-node-after-learning t)
                  (applied-fixes list)
                  (mode (eql :punish-non-gold-solutions)))
  "Reward successfully used cxns and categorial links, punish those that led to a non-gold solution."
  (declare (ignore solution-node solution-node-after-learning))
  (notify entrenchment-started)
  (multiple-value-bind (gold-solution-nodes non-gold-solution-nodes)
      (loop for node in (succeeded-nodes cip)
            if (gold-standard-solution-p (car-resulting-cfs (cipn-car node))
                                         (get-data (blackboard (construction-inventory node)) :speech-act)
                                         (direction cip)
                                         (configuration (construction-inventory node)))
              collect node into gold-solutions
            else
              collect node into non-gold-solutions
            finally (return (values gold-solutions non-gold-solutions)))
    (let* ((cxn-inventory (original-cxn-set (construction-inventory cip)))
           (category-mapping (find-data cip :category-mapping))
           (all-fix-cxns (loop for fix in applied-fixes append (when (slot-exists-p fix 'fix-constructions)
                                                                 (fix-constructions fix))))
           (constructions-to-reward (remove-duplicates (loop for node in gold-solution-nodes
                                                             for applied-cxns = (mapcar #'original-cxn (applied-constructions node))
                                                             append (loop for applied-cxn in applied-cxns
                                                                          for equivalent-cxn = (attr-val applied-cxn :equivalent-cxn)
                                                                          if (and equivalent-cxn
                                                                                  ;; if the applied cxn resulted from the same anti-unification repair, it
                                                                                  ;; is new and should not be rewarded.
                                                                                  (not (eq (au-repair-processor (anti-unification-state
                                                                                                                                (attr-val applied-cxn :fix)))
                                                                                           (au-repair-processor (anti-unification-state
                                                                                                                 (attr-val equivalent-cxn :fix))))))
                                                                            collect equivalent-cxn
                                                                          else
                                                                            unless (find (name applied-cxn) all-fix-cxns :key #'name)
                                                                              collect applied-cxn))))
           (constructions-to-punish (remove-duplicates (loop for node in non-gold-solution-nodes
                                                             for applied-cxns = (mapcar #'original-cxn (applied-constructions node))
                                                             append (loop for applied-cxn in applied-cxns
                                                                          unless (find (name applied-cxn) constructions-to-reward :key #'name)
                                                                            collect applied-cxn))))
           (links-to-reward (remove-duplicates (loop for node in gold-solution-nodes
                                                     append (loop for (cat-1 cat-2 link-type) in (used-categorial-links node)
                                                                  for mapped-link = (list (or (cdr (assoc cat-1 category-mapping))
                                                                                              cat-1)
                                                                                          (or (cdr (assoc cat-2 category-mapping))
                                                                                              cat-2)
                                                                                          link-type)
                                                                  when (and (find (first mapped-link) constructions-to-reward
                                                                                  :test #'member
                                                                                  :key #'(lambda (cxn)
                                                                                           (cons-if (attr-val cxn :cxn-cat)
                                                                                                    (attr-val cxn :slot-cats))))
                                                                            (find (second mapped-link) constructions-to-reward
                                                                                  :test #'member
                                                                                  :key #'(lambda (cxn)
                                                                                           (cons-if (attr-val cxn :cxn-cat)
                                                                                                    (attr-val cxn :slot-cats)))))
                                                                    collect mapped-link))
                                               :test #'equal))
           (links-to-punish (remove-duplicates (loop for node in non-gold-solution-nodes
                                                     append (loop for link in (used-categorial-links node)
                                                                  unless (find link links-to-reward :test #'equal)
                                                                  collect link))
                                               :test #'equal))
                                                                        
           (deleted-cxns nil)
           (deleted-categories nil)
           (deleted-links nil)
           (rewarded-links nil)
           (punished-links nil))
      ;; Reward
      (loop with reward-rate = (get-configuration cxn-inventory :li-reward)
            for cxn in constructions-to-reward
            for new-score = (+ reward-rate
                               (* (- 1 reward-rate)
                                  (attr-val cxn :entrenchment-score)))
            do (setf (attr-val cxn :entrenchment-score) new-score))
      (loop with reward-rate = (get-configuration cxn-inventory :li-reward)
            for (cat-1 cat-2 link-type) in links-to-reward
            for new-score = (+ reward-rate
                               (* (- 1 reward-rate)
                                  (link-weight cat-1 cat-2 cxn-inventory :link-type link-type)))
            do (push (list cat-1 cat-2 new-score) rewarded-links)
               (set-link-weight cat-1 cat-2 cxn-inventory new-score :link-type link-type))
      (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
            for cxn in constructions-to-punish
            for new-score = (* (- 1 inhibition-rate)
                               (attr-val cxn :entrenchment-score))
            do (setf (attr-val cxn :entrenchment-score) new-score)
            when (< (attr-val cxn :entrenchment-score) 0.01)
              do (delete-cxn cxn cxn-inventory)
                 (remove-categories (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) cxn-inventory)
                 (push cxn deleted-cxns)
                 (setf deleted-categories (append (cons-if (attr-val cxn :cxn-cat) (attr-val cxn :slot-cats)) deleted-categories)))
      (loop with inhibition-rate = (get-configuration cxn-inventory :li-punishement)
            for (cat-1 cat-2 link-type) in links-to-punish
            for categories-exist-p = (and (category-exists-p cat-1 cxn-inventory)
                                          (category-exists-p cat-2 cxn-inventory))
            for new-score = (when categories-exist-p
                              (* (- 1 inhibition-rate)
                               (link-weight cat-1 cat-2 cxn-inventory :link-type link-type)))
            do (when categories-exist-p (set-link-weight cat-1 cat-2 cxn-inventory new-score :link-type link-type)
                 (push (list cat-1 cat-2 new-score) punished-links))
            when (or (not categories-exist-p) (< (link-weight cat-1 cat-2 cxn-inventory :link-type link-type) 0.01))
              do (when categories-exist-p (remove-link cat-1 cat-2 cxn-inventory :link-type link-type :recompute-transitive-closure nil))
                 (push (cons cat-1 cat-2) deleted-links))
      
      (notify entrenchment-finished constructions-to-reward rewarded-links constructions-to-punish punished-links
              deleted-cxns deleted-categories deleted-links))))