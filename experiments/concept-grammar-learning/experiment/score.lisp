(in-package :clg)

;; -----------------------
;; + Updating CXN scores +
;; -----------------------

;; events
(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))

;; -----------
;; + Speaker +
;; -----------
(defun speaker-lateral-inhibition (agent cxns success)
  ;; Do the actual alignment. If success, reward
  ;; the applied cxns. If no success, punish the applied cxns.
  ;; Competitors are not considered in formulation, because
  ;; there is lots of synonymy in CLEVR. A meaning can be
  ;; expressed in many different, equally correct ways. We
  ;; do not want to converge on one meaning here.
  (notify alignment-started)
  (if success
    (loop with cxn-incf = (get-configuration agent :cxn-incf-score)
          for cxn in cxns do (inc-cxn-score cxn :delta cxn-incf)
          finally (notify cxns-rewarded cxns))
    (when cxns (punish-cxns agent cxns))))

;; ----------
;; + Hearer + 
;; ----------
(defun hearer-lateral-inhibition (agent cipn cxns success)
  ;; Do the actual alignment. If success, reward
  ;; the applied cxns and punish competitors. If
  ;; no success, punish the applied cxns.
  (notify alignment-started)
  (if success
    (progn
      ;; update concepts
      (when (get-configuration (experiment agent) :update-concepts-p)
        (loop with irl-program = (find-data (goal-test-data cipn) :irl-program)
              with bindings = (find-data (goal-test-data cipn) :bindings)
              for predicate in irl-program
              for predicate-name = (first predicate)
              for type = (second predicate)
              for variable = (third predicate)
              for category = (fourth predicate)

              ;; if its a bind and its a not an attribute-category
              ;;     it should be either a color-category, shape-category, size-category, material-category
              when (and (eq predicate-name 'bind) (not (eq type 'cw::attribute-category)))
                do (multiple-value-bind (target source) (find-associated-filter irl-program bindings variable)
                     (let ((other-objects (set-difference source target))
                           (concept (gethash category (get-data (ontology agent) 'concepts))))
                       (loop for topic in target
                             do (concept-representations::update-concept (meaning concept) topic other-objects))))))
      ;; reward cxns you used
      (loop with cxn-incf = (get-configuration agent :cxn-incf-score)
            for cxn in cxns do (inc-cxn-score cxn :delta cxn-incf)
            finally (notify cxns-rewarded cxns))
      ;; punish competitors
      (loop with delta = (get-configuration agent :cxn-inhibit-score) ;; separate inhibit delta!
            with remove-on-lower-bound = (get-configuration agent :remove-cxn-on-lower-bound)
            for competitor in (get-meaning-competitors agent cxns (utterance agent))
            if (eq (attr-val competitor :cxn-type) 'lexical)
              do (setf delta (get-delta-based-on-concept-similarity competitor cxns (get-data (ontology agent) 'concepts)
                                                                    (get-configuration (experiment agent) :lexical-cxn-inhibition-value)))
            do (dec-cxn-score agent competitor :delta delta
                              :remove-on-lower-bound
                              remove-on-lower-bound)
            collect competitor into competitors
            finally (notify cxns-punished competitors)))
    (when cxns
      (punish-cxns agent cxns))))


(defun get-delta-based-on-concept-similarity (competitor applied-constructions concepts inhibition-value)
  (let* ((concept-2 (gethash (attr-val competitor :meaning) concepts))
         (applied-cxn-with-same-form (first (loop for cxn in applied-constructions
                                                  when (string= (attr-val cxn :string) (attr-val competitor :string))
                                                    collect cxn)))
                                             
         (concept-1 (gethash (attr-val applied-cxn-with-same-form :meaning) concepts))
         (similarity (concept-representations::concept-similarity (meaning concept-1) (meaning concept-2))))
      (* inhibition-value similarity)))

;; Utility functions
(defun punish-cxns (agent cxns)
  "Punish the list of provided cxns"
  (loop with delta = (get-configuration agent :cxn-decf-score) ;; separate failure delta!
        with remove-on-lower-bound = (get-configuration agent :remove-cxn-on-lower-bound)
        for cxn in cxns
        do (dec-cxn-score agent cxn :delta delta
                          :remove-on-lower-bound
                          remove-on-lower-bound)
        finally (notify cxns-punished cxns)))

(define-event lexicon-changed)

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
  ;; if you are using :pretrained-concepts -> only remove if its not a lex
  ;; if you are not using :pretrained-concepts -> go ahead
  (when (or (not (get-configuration agent :pretrained-concepts))
            (and (get-configuration agent :pretrained-concepts)
                 (not (get-lex-classes cxn agent))))
    (decf (attr-val cxn :score) delta)
    (when (<= (attr-val cxn :score) lower-bound)
      (if remove-on-lower-bound
        (progn (notify lexicon-changed)
          (with-disabled-monitor-notifications
            (delete-cxn-and-th-node cxn agent)))
        (setf (attr-val cxn :score) lower-bound)))
    )
  (grammar agent))

(defun delete-cxn-and-th-node (cxn agent)
  "Delete the cxn from the cxn inventory
   and remove ALL associated categories
   from the categorial network."
  ;; if you are using :pretrained-concepts -> only remove if its not a lex
  ;; if you are not using :pretrained-concepts -> go ahead
  (when (or (not (get-configuration agent :pretrained-concepts))
            (and (get-configuration agent :pretrained-concepts)
                 (not (get-lex-classes cxn agent))))
    (remove-categories lex-classes type-hierarchy)
    (delete-cxn cxn (grammar agent))
    (notify lexicon-changed)))


;; utilities

(defun get-lex-classes (cxn agent)
  (let ((lex-classes (loop for unit in (contributing-part cxn)
                           for lex-class = (lex-class-item-based unit)
                           when lex-class collect lex-class))
        (type-hierarchy (categorial-network (grammar agent))))
    (values lex-classes type-hierarchy)))

(defun find-associated-filter (irl-program bindings variable)
  (loop with result = nil
        for predicate in irl-program
        when (and (eq (first predicate) 'filter)
                  (eq (fourth predicate) variable))
          do (setf result (list
                           (value (find (second predicate) bindings
                                        :key #'var))
                           (value (find (third predicate) bindings
                                        :key #'var))))
        finally (return (values (objects (first result))
                                (objects (second result))))))
