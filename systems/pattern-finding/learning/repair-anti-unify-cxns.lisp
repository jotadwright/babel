(in-package :pf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair anti-unify cxns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass anti-unify-cxns (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))


(defmethod repair ((repair anti-unify-cxns)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  (let ((cxns-and-categorial-links
         (do-repair
          (get-data problem :utterance)
          (get-data problem :meaning)
          (make-blackboard)
          (construction-inventory node)
          node
          'anti-unify-cxns)))
    (when cxns-and-categorial-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-categorial-links))))


(defmethod do-repair (observation-form observation-meaning (args blackboard) (cxn-inventory construction-inventory) node (repair-type (eql 'anti-unify-cxns)))
  (when (constructions cxn-inventory)
    (let ((new-cxns-and-links (find-cxns-and-anti-unify observation-form observation-meaning args (original-cxn-set cxn-inventory))))
      (when new-cxns-and-links
        (destructuring-bind (cxns-to-apply cxns-to-consolidate cats-to-add cat-links-to-add) new-cxns-and-links
          (apply-fix :form-constraints observation-form
                     :cxns-to-apply cxns-to-apply
                     :cxns-to-consolidate cxns-to-consolidate
                     :categories-to-add cats-to-add
                     :categorial-links cat-links-to-add
                     :top-level-category (extract-contributing-category (last-elt cxns-to-apply))
                     :node node
                     :repair-name repair-type))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find cxns and anti-unify ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event learn-from-anti-unification (anti-unification-results list))

(defmethod find-cxns-and-anti-unify (observation-form observation-meaning (args blackboard) (cxn-inventory fcg-construction-set))
  "Given form and meaning of an observation and a cxn inventory,
   find the cxn that leads to the smallest generalisation
   and learn new cxn(s) from this generalisation."
  (let* (;; 1) select cxns by hasing the observation
         ;;    only form is provided since we are learning in comprehension
         (hash-compatible-cxns
          (constructions-for-anti-unification-hashed observation-form nil cxn-inventory))
         
         ;; 2) filter hash-compatible cxns for routine cxns with a positive score
         (filtered-hash-compatible-cxns
          (remove-if-not #'non-zero-cxn-p
                         (remove-if-not #'routine-cxn-p
                                        hash-compatible-cxns)))
           
         ;; 3) find the least general generalisation through anti-unification
         (least-general-generalisations
          (loop with max-au-cost = (get-configuration cxn-inventory :max-au-cost)
                for cxn in filtered-hash-compatible-cxns
                ;; returns all valid form anti unification results
                for form-anti-unification-results
                  = (anti-unify-form observation-form cxn
                                     :max-au-cost max-au-cost)
                ;; returns all valid meaning anti unification results
                for meaning-anti-unification-results
                  = (anti-unify-meaning observation-meaning cxn
                                        :max-au-cost max-au-cost)
                ;; make all combinations and filter for valid combinations
                for all-anti-unification-combinations
                  = (remove-if-not #'valid-au-combination-p
                                   (combinations meaning-anti-unification-results
                                                 form-anti-unification-results))
                when all-anti-unification-combinations
                ;; store all valid combinations with the cxn used for anti unification
                append (loop for combo in all-anti-unification-combinations
                             collect (cons cxn combo))
                into anti-unification-results
                ;; return the best anti unification combination (costs and cxn score)
                finally (return (sort-anti-unification-combinations anti-unification-results)))))

    ;; 4) learn cxns(s) from the anti-unification results
    (when least-general-generalisations
      (dolist (generalisation least-general-generalisations)
        (let* ((anti-unified-cxn (first generalisation))
               (new-cxns-and-links
                (make-cxns-from-generalisation generalisation args cxn-inventory)))
                ;(cond ((holophrase-cxn-p anti-unified-cxn)
                ;       (make-cxns-from-holophrase-generalisation generalisation args cxn-inventory))
                ;      ((holistic-cxn-p anti-unified-cxn)
                ;       (make-cxns-from-holistic-generalisation generalisation args cxn-inventory))
                ;      ((item-based-cxn-p anti-unified-cxn)
                ;       (make-cxns-from-item-based-generalisation generalisation args cxn-inventory)))))
          (when new-cxns-and-links
            (notify learn-from-anti-unification generalisation)
            (return new-cxns-and-links)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from generalisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change args into args-and-cats to pass on grammatical categories
;; on the source side in the recursion!

(defun make-cxns-from-generalisation (anti-unification-results args cxn-inventory)
  (destructuring-bind (anti-unified-cxn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* (;; form-args and meaning-args
           (form-args (compute-form-args form-anti-unification anti-unified-cxn args))
           (meaning-args (compute-meaning-args meaning-anti-unification anti-unified-cxn args))
           ;; holistic cxn from generalisation
           (generalisation-cxns-and-links
            (let ((recursion-args
                   (make-blackboard
                    :data-fields (list (cons :top-lvl-form-args (find-data form-args :generalisation-top-lvl-args))
                                       (cons :top-lvl-meaning-args (find-data meaning-args :generalisation-top-lvl-args))))))
              (handle-potential-holistic-cxn (generalisation form-anti-unification)
                                             (generalisation meaning-anti-unification)
                                             recursion-args cxn-inventory))
            ;(make-holistic-cxn (generalisation form-anti-unification)
            ;                   (generalisation meaning-anti-unification)
            ;                   (find-data form-args :generalisation-top-lvl-args)
            ;                   (find-data meaning-args :generalisation-top-lvl-args)
            ;                   cxn-inventory)
            )
           ;; item-based cxn from source delta
           (source-delta-cxns-and-links
            (let ((recursion-args
                   (make-blackboard
                    :data-fields (list (cons :top-lvl-form-args (find-data form-args :source-top-lvl-args))
                                       (cons :top-lvl-meaning-args (find-data meaning-args :source-top-lvl-args))
                                       (cons :slot-form-args (find-data form-args :source-slot-args))
                                       (cons :slot-meaning-args (find-data meaning-args :source-slot-args))
                                       (cons :slot-filler-cats (cons (afr-top-lvl-category generalisation-cxns-and-links)
                                                                     (find-data args :slot-filler-cats)))
                                       (cons :original-slot-cats (find-data args :original-slot-cats))))))
              (handle-potential-holistic-cxn (source-delta form-anti-unification)
                                             (source-delta meaning-anti-unification)
                                             recursion-args cxn-inventory))
            ;(make-item-based-cxn (source-delta form-anti-unification)
            ;                     (source-delta meaning-anti-unification)
            ;                     (find-data form-args :source-top-lvl-args)
            ;                     (find-data meaning-args :source-top-lvl-args)
            ;                     (find-data form-args :source-slot-args)
            ;                     (find-data meaning-args :source-slot-args)
            ;                     cxn-inventory)
            )
           (source-delta-slot-categories
            (remove (afr-top-lvl-category source-delta-cxns-and-links)
                    (afr-categories-to-add source-delta-cxns-and-links)))
           ;; item-based cxn from pattern delta
           (original-slot-cats
            (neighbouring-categories
             (extract-top-category-cxn anti-unified-cxn)
             (categorial-network cxn-inventory)))
           (pattern-delta-cxns-and-links
            (when (and (pattern-delta form-anti-unification)
                       (pattern-delta meaning-anti-unification))
              (let ((recursion-args
                     (make-blackboard
                      :data-fields (list (cons :top-lvl-form-args (find-data form-args :pattern-top-lvl-args))
                                         (cons :top-lvl-meaning-args (find-data meaning-args :pattern-top-lvl-args))
                                         (cons :slot-form-args (find-data form-args :pattern-slot-args))
                                         (cons :slot-meaning-args (find-data meaning-args :pattern-slot-args))
                                         (cons :slot-filler-cats (cons (afr-top-lvl-category generalisation-cxns-and-links)
                                                                       (find-data args :slot-filler-cats)))
                                         (cons :original-slot-cats (or (find-data args :original-slot-cats)
                                                                       original-slot-cats))))))
                (handle-potential-holistic-cxn (pattern-delta form-anti-unification)
                                               (pattern-delta meaning-anti-unification)
                                               recursion-args cxn-inventory))
              ;(make-item-based-cxn (pattern-delta form-anti-unification)
              ;                     (pattern-delta meaning-anti-unification)
              ;                     (find-data form-args :pattern-top-lvl-args)
              ;                     (find-data meaning-args :pattern-top-lvl-args)
              ;                     (find-data form-args :pattern-slot-args)
              ;                     (find-data meaning-args :pattern-slot-args)
              ;                     cxn-inventory)
              ))
           (pattern-delta-slot-categories
            (when pattern-delta-cxns-and-links
              (remove (afr-top-lvl-category pattern-delta-cxns-and-links)
                      (afr-categories-to-add pattern-delta-cxns-and-links))))
           ;; build results
           (cxns-to-apply
            (append
             (afr-cxns-to-apply source-delta-cxns-and-links)
             (afr-cxns-to-apply generalisation-cxns-and-links)))
           (cxns-to-consolidate
            (append
             (afr-cxns-to-consolidate source-delta-cxns-and-links)
             (afr-cxns-to-consolidate generalisation-cxns-and-links)
             (when pattern-delta-cxns-and-links
               (append (afr-cxns-to-apply pattern-delta-cxns-and-links)
                       (afr-cxns-to-consolidate pattern-delta-cxns-and-links)))))
           (categories-to-add
            (append
             (afr-categories-to-add source-delta-cxns-and-links)
             (afr-categories-to-add generalisation-cxns-and-links)
             (when pattern-delta-cxns-and-links
               (afr-categories-to-add pattern-delta-cxns-and-links))))
           (categorial-links
            (append
             (afr-categorial-links source-delta-cxns-and-links)
             (afr-categorial-links generalisation-cxns-and-links)
             ; + top lvl category of generalisation cxn <-> (first) slot category of source delta cxn
             ; => handled at the end of recursion!
             ;(list (cons (afr-top-lvl-category generalisation-cxns-and-links) (first source-delta-slot-categories)))
             ; + top lvl category of source delta cxn <-> slots that are filled by top lvl category of original source
             ; => handled at the end of recursion!
             ;(loop for original-slot in (find-data args :original-slot-cats)
             ;      collect (cons (afr-top-lvl-category source-delta-cxns-and-links) original-slot))
             ; + fillers of slots of original source <-> (rest of) slots of source delta cxn
             ; => handled at the end of recursion
             ;(loop for filler in (find-data args :slot-filler-cats)
             ;      for slot in (rest source-delta-slot-categories)
             ;      collect (cons filler slot))
             (when pattern-delta-cxns-and-links
               (append (afr-categorial-links pattern-delta-cxns-and-links)
                       ; + top lvl category of generalisation cxn <-> (first) slot category of pattern delta cxn
                       ; => handled at the end of recursion
                       ;(list (cons (afr-top-lvl-category generalisation-cxns-and-links) (first pattern-delta-slot-categories)))
                       ; + top lvl category of pattern delta cxn <-> slots that are filled by top lvl category of pattern
                       (link-filler-to-previous-slots
                        anti-unified-cxn
                        (afr-top-lvl-category pattern-delta-cxns-and-links)
                        cxn-inventory)
                       ; + fillers of slots of pattern <-> (rest of) slots of pattern delta cxn
                       (link-slots-to-previous-fillers
                        anti-unified-cxn
                        (rest pattern-delta-slot-categories)
                        cxn-inventory))))))
      (list cxns-to-apply
            cxns-to-consolidate
            categories-to-add
            categorial-links))))

(defun link-filler-to-previous-slots (anti-unified-cxn filler-category cxn-inventory)
  (let* ((anti-unified-cxn-top-category (extract-top-category-cxn anti-unified-cxn))
         (filling-slot-categories (neighbouring-categories anti-unified-cxn-top-category (categorial-network cxn-inventory))))
    (loop for slot in filling-slot-categories
          collect (cons slot filler-category))))

(defun link-slots-to-previous-fillers (anti-unified-cxn slot-categories cxn-inventory)
  (let* ((anti-unified-cxn-slot-categories (extract-slot-categories-item-based-cxn anti-unified-cxn))
         (filler-categories (loop for slot-category in anti-unified-cxn-slot-categories
                                  append (neighbouring-categories slot-category (categorial-network cxn-inventory)))))
    (loop for filler in filler-categories
          for slot in slot-categories
          collect (cons filler slot))))


#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from holophrase generalisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cxns-from-holophrase-generalisation (anti-unification-results args cxn-inventory)
  "When anti-unifying with a holophrase cxn, make
   1) an item-based cxn from the generalisation,
   2) a holistic cxn from the source delta, and
   3) a holistic cxn from the pattern delta (can be empty!).
   The cxns from both delta's fill the slot of the cxn from the generalisation."
  (destructuring-bind (anti-unified-cxn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* (;; all form-args and meaning-args
           (form-args (compute-form-args form-anti-unification anti-unified-cxn args))
           (meaning-args (compute-meaning-args meaning-anti-unification anti-unified-cxn args))
           ;; item-based cxn from generalisation
           (generalisation-cxns-and-categories
            (make-generalisation-cxn (generalisation form-anti-unification)
                                     (generalisation meaning-anti-unification)
                                     (find-data form-args :generalisation-top-lvl-args)
                                     (find-data meaning-args :generalisation-top-lvl-args)
                                     (find-data form-args :generalisation-slot-args)
                                     (find-data meaning-args :generalisation-slot-args)
                                     cxn-inventory))
           (generalisation-slot-categories
            (remove (afr-top-lvl-category generalisation-cxns-and-categories)
                    (afr-categories-to-add generalisation-cxns-and-categories)))
           ;; holistic cxn from source delta
           (source-delta-cxns-and-categories
            (let ((recursion-args
                   (make-blackboard
                    :data-fields
                    (list (cons :top-lvl-form-args (find-data form-args :source-top-lvl-args))
                          (cons :top-lvl-meaning-args (find-data meaning-args :source-top-lvl-args))))))
              (handle-potential-holistic-cxn (source-delta form-anti-unification)
                                             (source-delta meaning-anti-unification)
                                             recursion-args cxn-inventory)))
           ;; holistic cxn from pattern delta
           (pattern-delta-cxns-and-categories
            (when (and (pattern-delta form-anti-unification)
                       (pattern-delta meaning-anti-unification))
              (let ((recursion-args
                     (make-blackboard
                      :data-fields
                      (list (cons :top-lvl-form-args (find-data form-args :pattern-top-lvl-args))
                            (cons :top-lvl-meaning-args (find-data meaning-args :pattern-top-lvl-args))))))
                (handle-potential-holistic-cxn (pattern-delta form-anti-unification)
                                               (pattern-delta meaning-anti-unification)
                                               recursion-args cxn-inventory))))
           ;; build results
           (cxns-to-apply
            ;; holistic cxn source delta apply-first + item-based cxn generalisation apply-last
            (append (afr-cxns-to-apply source-delta-cxns-and-categories)
                    (afr-cxns-to-apply generalisation-cxns-and-categories)))
           (cxns-to-consolidate
            ;; holistic cxn source delta apply-last + item-based cxn generalisation apply-first
            ;; + holistic cxn pattern delta apply-first and apply-last
            (append (afr-cxns-to-consolidate source-delta-cxns-and-categories)
                    (afr-cxns-to-consolidate generalisation-cxns-and-categories)
                    (when pattern-delta-cxns-and-categories
                      (append (afr-cxns-to-apply pattern-delta-cxns-and-categories)
                              (afr-cxns-to-consolidate pattern-delta-cxns-and-categories)))))
           (categories-to-add
            ;; top lvl category source delta cxn + top lvl category generalisation cxn
            ;; + slot category generalisation cxn + top-lvl category pattern delta cxn
            (append (afr-categories-to-add source-delta-cxns-and-categories)
                    (afr-categories-to-add generalisation-cxns-and-categories)
                    (when pattern-delta-cxns-and-categories
                      (afr-categories-to-add pattern-delta-cxns-and-categories))))
           (links-to-add
            ;; link between slot of generalisation cxn and source delta cxn
            ;; + link between slot of generalisation cxn and pattern delta cxn
            (append (loop for slot-cat in generalisation-slot-categories
                          collect (cons (afr-top-lvl-category source-delta-cxns-and-categories) slot-cat))
                    (afr-categorial-links source-delta-cxns-and-categories)
                    (afr-categorial-links generalisation-cxns-and-categories)
                    (when pattern-delta-cxns-and-categories
                      (append (loop for slot-cat in generalisation-slot-categories
                                    collect (cons (afr-top-lvl-category pattern-delta-cxns-and-categories) slot-cat))
                              (afr-categorial-links pattern-delta-cxns-and-categories))))))
      (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from holistic generalisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cxns-from-holistic-generalisation (anti-unification-results args cxn-inventory)
  "When anti-unifying with a holistic cxn, make
   1) a holistic cxn from the generalisation,
   2) an item-based cxn from the source delta, and
   3) an item-based cxn from the pattern delta (can be empty!)
   The cxn from the generalisation fills the slot of both the source delta cxn
   and the pattern delta cxn.
   Also add links such that the pattern delta cxn fills the same slots as
   the original holistic cxn used for anti-unification! This ensures that
   the previous observations are now also covered with these new cxns."
  (destructuring-bind (anti-unified-cxn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* (;; all form-args and meaning-args
           (form-args (compute-form-args form-anti-unification anti-unified-cxn args))
           (meaning-args (compute-meaning-args meaning-anti-unification anti-unified-cxn args))
           ;; holistic cxn from generalisation
           (generalisation-cxns-and-categories
            (let ((recursion-args
                   (make-blackboard
                    :data-fields
                    (list (cons :top-lvl-form-args (find-data form-args :generalisation-slot-args))
                          (cons :top-lvl-meaning-args (find-data meaning-args :generalisation-slot-args))))))
              (handle-potential-holistic-cxn (generalisation form-anti-unification)
                                             (generalisation meaning-anti-unification)
                                             recursion-args cxn-inventory)))
           ;; item-based cxn from source delta
           (source-delta-cxns-and-categories
            (make-generalisation-cxn (source-delta form-anti-unification)
                                     (source-delta meaning-anti-unification)
                                     (find-data form-args :source-slot-args)
                                     (find-data meaning-args :source-slot-args)
                                     (find-data form-args :source-top-lvl-args)
                                     (find-data meaning-args :source-top-lvl-args)
                                     cxn-inventory))
           (source-delta-slot-categories
            (remove (afr-top-lvl-category source-delta-cxns-and-categories)
                    (afr-categories-to-add source-delta-cxns-and-categories)))
           ;; item-based cxn from pattern delta
           (pattern-delta-cxns-and-categories
            (when (and (pattern-delta form-anti-unification)
                       (pattern-delta meaning-anti-unification))
              (make-generalisation-cxn (pattern-delta form-anti-unification)
                                       (pattern-delta meaning-anti-unification)
                                       (find-data form-args :pattern-slot-args)
                                       (find-data meaning-args :pattern-slot-args)
                                       (find-data form-args :pattern-top-lvl-args)
                                       (find-data meaning-args :pattern-top-lvl-args)
                                       cxn-inventory)))
           (pattern-delta-slot-categories
            (when pattern-delta-cxns-and-categories
              (remove (afr-top-lvl-category pattern-delta-cxns-and-categories)
                      (afr-categories-to-add pattern-delta-cxns-and-categories))))
           ;; build results
           (cxns-to-apply
            ;; holistic generalisation cxn apply-first + item-based source delta cxn apply-last
            (append (afr-cxns-to-apply generalisation-cxns-and-categories)
                    (afr-cxns-to-apply source-delta-cxns-and-categories)))
           (cxns-to-consolidate
            ;; holistic generalisation cxn apply-last + item-based source delta cxn apply-first
            ;; + item-based pattern delta cxn apply-last and apply-first
            (append (afr-cxns-to-consolidate generalisation-cxns-and-categories)
                    (afr-cxns-to-consolidate source-delta-cxns-and-categories)
                    (when pattern-delta-cxns-and-categories
                      (append (afr-cxns-to-apply pattern-delta-cxns-and-categories)
                              (afr-cxns-to-consolidate pattern-delta-cxns-and-categories)))))
           (categories-to-add
            ;; top lvl category generalisation cxn + top lvl category source delta cxn
            ;; + slot category source delta cxn + top lvl category pattern delta cxn
            ;; + slot category pattern delta cxn
            (append (afr-categories-to-add generalisation-cxns-and-categories)
                    (afr-categories-to-add source-delta-cxns-and-categories)
                    (when pattern-delta-cxns-and-categories
                      (afr-categories-to-add pattern-delta-cxns-and-categories))))
           ;; link generalisation cxn to slot of source delta cxn
           ;; + link generalisation cxn to slot of pattern delta cxn
           ;; + link pattern delta cxn to slots that were filled by anti-unified cxn
           (links-to-add
            (append (loop for slot-cat in source-delta-slot-categories
                          collect (cons (afr-top-lvl-category generalisation-cxns-and-categories) slot-cat))
                    (afr-categorial-links generalisation-cxns-and-categories)
                    (afr-categorial-links source-delta-cxns-and-categories)
                    (when pattern-delta-cxns-and-categories
                      (append (loop for slot-cat in pattern-delta-slot-categories
                                    collect (cons (afr-top-lvl-category generalisation-cxns-and-categories) slot-cat))
                              (afr-categorial-links pattern-delta-cxns-and-categories)
                              (link-filler-to-previous-slots
                               anti-unified-cxn
                               (afr-top-lvl-category pattern-delta-cxns-and-categories)
                               cxn-inventory))))))
      (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add))))

(defun link-filler-to-previous-slots (anti-unified-cxn filler-category cxn-inventory)
  (let* ((anti-unified-cxn-category (extract-top-category-holistic-cxn anti-unified-cxn))
         (filling-slot-categories (neighbouring-categories anti-unified-cxn-category (categorial-network cxn-inventory))))
    (loop for slot in filling-slot-categories
          collect (cons slot filler-category))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from item-based generalisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cxns-from-item-based-generalisation (anti-unification-results args cxn-inventory)
  "When anti-unifying with an item-based cxn, make
   1) an item-based cxn from the generalisation,
   2) a holistic cxn from the source delta, and
   3) an item-based cxn from the pattern delta (can be empty!)
   The cxns from both delta's fill the slot of the cxn from the generalisation.
   Also add links such that the slots of the pattern delta cxn take the same
   fillers as the slots of the original item-based cxn used for anti-unification!
   This ensures that the previous observations are now also covered with these
   new cxns."
  (destructuring-bind (anti-unified-cxn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* (;; all form-args and meaning-args
           (form-args (compute-form-args form-anti-unification anti-unified-cxn args))
           (meaning-args (compute-meaning-args meaning-anti-unification anti-unified-cxn args))
           ;; item-based cxn from generalisation
           (generalisation-cxns-and-categories
            (make-generalisation-cxn (generalisation form-anti-unification)
                                     (generalisation meaning-anti-unification)
                                     (find-data form-args :generalisation-top-lvl-args)
                                     (find-data meaning-args :generalisation-top-lvl-args)
                                     (find-data form-args :generalisation-slot-args)
                                     (find-data meaning-args :generalisation-slot-args)
                                     cxn-inventory))
           (generalisation-slot-categories
            (remove (afr-top-lvl-category generalisation-cxns-and-categories)
                    (afr-categories-to-add generalisation-cxns-and-categories)))
           ;; holistic cxn from source delta
           (source-delta-cxns-and-categories
            (let ((recursion-args
                   (make-blackboard
                    :data-fields
                    (list (cons :top-lvl-form-args (find-data form-args :source-top-lvl-args))
                          (cons :top-lvl-meaning-args (find-data meaning-args :source-top-lvl-args))))))
              (handle-potential-holistic-cxn (source-delta form-anti-unification)
                                             (source-delta meaning-anti-unification)
                                             recursion-args cxn-inventory)))
           ;; item-based cxn from pattern delta
           (pattern-delta-form-arg-groups
            (loop for unit in (extract-slot-units anti-unified-cxn)
                  collect (cons (extract-category-unit unit)
                                (first (fcg-unit-feature-value unit 'form-args)))))
           (pattern-delta-meaning-arg-groups
            (loop for unit in (extract-slot-units anti-unified-cxn)
                  collect (cons (extract-category-unit unit)
                                (first (fcg-unit-feature-value unit 'meaning-args)))))
           (pattern-delta-cxns-and-categories
            (when (and (pattern-delta form-anti-unification)
                       (pattern-delta meaning-anti-unification))
              (make-generalisation-cxn-with-n-units (pattern-delta form-anti-unification)
                                                    (pattern-delta meaning-anti-unification)
                                                    (find-data form-args :pattern-top-lvl-args)
                                                    (find-data meaning-args :pattern-top-lvl-args)
                                                    (find-data form-args :pattern-slot-args)
                                                    (find-data meaning-args :pattern-slot-args)
                                                    pattern-delta-form-arg-groups
                                                    pattern-delta-meaning-arg-groups
                                                    cxn-inventory)))
           ;; build results
           (cxns-to-apply
            ;; holistic cxn source delta apply-first + item-based cxn generalisation apply-last
            (append (afr-cxns-to-apply source-delta-cxns-and-categories)
                    (afr-cxns-to-apply generalisation-cxns-and-categories)))
           (cxns-to-consolidate
            ;; holistic cxn source delta apply-last + item-based cxn generalisation apply-first
            ;; + item-based cxn pattern delta apply-last and apply-first
            (append (afr-cxns-to-consolidate source-delta-cxns-and-categories)
                    (afr-cxns-to-consolidate generalisation-cxns-and-categories)
                    (when pattern-delta-cxns-and-categories
                      (append (afr-cxns-to-apply pattern-delta-cxns-and-categories)
                              (afr-cxns-to-consolidate pattern-delta-cxns-and-categories)))))
           (categories-to-add
            ;; top lvl category source delta cxn + top lvl category generalisation cxn
            ;; + slot category generalisation cxn
            ;; + top lvl category pattern delta cxn + slot categories pattern delta cxn
            (append (afr-categories-to-add source-delta-cxns-and-categories)
                    (afr-categories-to-add generalisation-cxns-and-categories)
                    (when pattern-delta-cxns-and-categories
                      (afr-categories-to-add pattern-delta-cxns-and-categories))))
           (links-to-add
            ;; link between slot of generalisation cxn and source delta cxn
            ;; + link between slot of generalisation cxn and pattern delta cxn
            ;; + link between slots of pattern delta cxn and fillers of anti-unified item-based cxn!
            (append (loop for slot-cat in generalisation-slot-categories
                          collect (cons (afr-top-lvl-category source-delta-cxns-and-categories) slot-cat))
                    (afr-categorial-links source-delta-cxns-and-categories)
                    (afr-categorial-links generalisation-cxns-and-categories)
                    (when pattern-delta-cxns-and-categories
                      (append (loop for slot-cat in generalisation-slot-categories
                                    collect (cons (afr-top-lvl-category pattern-delta-cxns-and-categories) slot-cat))
                              (afr-categorial-links pattern-delta-cxns-and-categories)
                              (link-slots-to-previous-fillers
                               pattern-delta-meaning-arg-groups
                               (first (afr-cxns-to-apply pattern-delta-cxns-and-categories))
                               cxn-inventory))))))
      (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add))))

(defun link-slots-to-previous-fillers (arg-groups new-cxn cxn-inventory)
  "Link the slots of the new item-based cxn to the fillers of the slots of
   the item-based cxn that was used for anti-unification. To know which slot
   should take which fillers, use the grammatical category that is present in
   the top-arg/slot-arg predicates."
  (loop for arg-group in arg-groups
        for category-anti-unified-cxn = (first arg-group)
        for args = (rest arg-group)
        for fillers-anti-unified-cxn-slots
          = (neighbouring-categories category-anti-unified-cxn (categorial-network cxn-inventory))
        for unit = (loop for unit in (conditional-part new-cxn)
                         when (or (equal args (first (fcg-unit-feature-value unit 'meaning-args)))
                                  (equal args (first (fcg-unit-feature-value unit 'form-args))))
                           return unit)
        for unit-category = (extract-category-unit unit)
        append (loop for filler in fillers-anti-unified-cxn-slots
                     collect (cons filler unit-category))))
|#
