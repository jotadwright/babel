(in-package :duckie-language-learning)

;; ---------------
;; + Score utils +
;; ---------------

;; events
(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))
(define-event lexicon-changed)

(defun lateral-inhibition (agent cxns success)
  "Performs lateral inhibition.
   If the interaction was successful:
      reward the applied cxns and punish competitors.
   Otherwise, punish the applied cxns."
  (notify alignment-started)
  (if success
    (progn
      (loop with cxn-incf = (get-configuration agent :cxn-incf-score)
            for cxn in cxns do (inc-cxn-score cxn :delta cxn-incf)
            finally (notify cxns-rewarded cxns))
      (loop with delta = (get-configuration agent :cxn-inhibit-score) ;; separate inhibit delta!
            with remove-on-lower-bound = (get-configuration agent :remove-cxn-on-lower-bound)
            for competitor in (get-meaning-competitors agent cxns (utterance agent))
            do (dec-cxn-score agent competitor :delta delta
                              :remove-on-lower-bound
                              remove-on-lower-bound)
            collect competitor into competitors
            finally (notify cxns-punished competitors)))
    (when cxns
      (punish-cxns agent cxns))))

(defun punish-cxns (agent cxns)
  "Punish the list of provided cxns"
  (loop with delta = (get-configuration agent :cxn-decf-score) ;; separate failure delta!
        with remove-on-lower-bound = (get-configuration agent :remove-cxn-on-lower-bound)
        for cxn in cxns
        do (dec-cxn-score agent cxn :delta delta
                          :remove-on-lower-bound
                          remove-on-lower-bound)
        finally (notify cxns-punished cxns)))

(defun inc-cxn-score (cxn &key (delta 0.1) (upper-bound 1.0))
  "increase the score of the cxn"
  (incf (attr-val cxn :score) delta)
  (when (> (attr-val cxn :score) upper-bound)
    (setf (attr-val cxn :score) upper-bound))
  cxn)

(defun dec-cxn-score (agent cxn
                            &key (delta 0.1)
                            (lower-bound 0.0)
                            (remove-on-lower-bound t))
  "decrease the score of the cxn.
   remove it when it reaches 0"
  (decf (attr-val cxn :score) delta)
  (when (<= (attr-val cxn :score) lower-bound)
    (if remove-on-lower-bound
      (progn (notify lexicon-changed)
        (with-disabled-monitor-notifications
         (delete-cxn-and-th-node cxn agent)))
      (setf (attr-val cxn :score) lower-bound)))
  (grammar agent))

(defun delete-cxn-and-th-node (cxn agent)
  "Delete the cxn from the cxn inventory
   and remove ALL associated categories
   from the categorial network."
  (let ((lex-classes (loop for unit in (contributing-part cxn)
                           for lex-class = (lex-class-item-based unit)
                           when lex-class collect lex-class))
        (type-hierarchy (categorial-network (grammar agent))))
    (when lex-classes
      (remove-categories lex-classes type-hierarchy))
    (delete-cxn cxn (grammar agent))    
    (notify lexicon-changed)))
