;;;; alignment.lisp

(in-package :pattern-finding-old)

(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))

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
      
