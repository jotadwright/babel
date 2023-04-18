(in-package :duckie-language-learning)

;; --------------------------
;; + Repair: ADD-HOLOPHRASE +
;; --------------------------

(defclass add-holophrase (repair)
  ((trigger :initform 'fcg::new-node)))

;; This repair is applied when the utterance is completely unknown
;; or when all repairs using the partial utterance have failed
;; or when interpretation has failed.

(defmethod repair ((repair add-holophrase)
                   (problem unknown-utterance-problem)
                   (node cip-node) &key
                   &allow-other-keys)
  "Repair by making a new holophrase, when the utterance
   is completely unknown."
  (make-instance 'fcg::cxn-fix
                 :repair repair
                 :problem problem
                 :restart-data (create-holophrase-cxn
                                node (find-data problem :intention))))

(defun create-holophrase-cxn (node intention)
  "Create a new holophrase construction from the reconstructed intention"
  (let* ((cxn-inventory
          (original-cxn-set
           (construction-inventory node)))
         (utterance (cipn-utterance node))
         (holophrase-cxn-name
          (make-const
           (gl::make-cxn-name
            (remove-spurious-spaces
             (remove-punctuation utterance))
            cxn-inventory)))
         (form-constraints
          (gl::form-constraints-with-variables
           utterance (get-configuration cxn-inventory :de-render-mode)))
         (meaning-constraints intention)
         (initial-cxn-score 0.5)
         (existing-holophrase-cxn
          (find-cxn-by-type-form-and-meaning 'holophrase
                                             form-constraints
                                             meaning-constraints
                                             cxn-inventory))
         (hash-string
          (form-predicates->hash-string form-constraints))
         (hash-meaning
          (meaning-predicates->hash-meaning meaning-constraints))
         (holophrase-cxn
          (or existing-holophrase-cxn
              (second
               (multiple-value-list
                (eval
                 `(def-fcg-cxn ,holophrase-cxn-name
                               ((?holophrase-unit
                                 (syn-cat (gl::phrase-type gl::holophrase)))
                                <-
                                (?holophrase-unit
                                 (HASH meaning ,meaning-constraints)
                                 --
                                 (HASH form ,form-constraints)))
                               :cxn-inventory ,(copy-object cxn-inventory)
                               :attributes (:score ,initial-cxn-score
                                            :cxn-type gl::holophrase
                                            :repair add-holo
                                            :string ,hash-string
                                            :meaning ,hash-meaning)
                               :cxn-set holophrase)))))))
    ;(add-composer-chunk agent meaning-constraints)
    ;(set-data interaction :applied-repair 'add-holophrase)
    holophrase-cxn))

(defmethod handle-fix ((fix fcg::cxn-fix) (repair add-holophrase)
                       (problem problem) (node cip-node)
                       &key &allow-other-keys)
  "Handle fix for the add-holophrase repair. Apply the holophrase
   to the initial node and add it to the cxn inventory."
  (push fix (fixes problem))
  
  (let* ((holophrase-cxn (restart-data fix))
         (form-constraints (gl::form-constraints-with-variables
                            (cipn-utterance node)
                            (get-configuration (original-cxn-set (construction-inventory node)) :de-render-mode))))
    (let ((new-nodes (with-disabled-monitor-notifications (apply-sequentially (gl::initial-node node)
                                                                              (list holophrase-cxn) node))))
      (set-data (car-resulting-cfs (cipn-car (first new-nodes)))
                :fix-cxns (list holophrase-cxn))
      (setf (cxn-supplier (first new-nodes)) (cxn-supplier node))
      ;; write some message on the blackboard of the initial node
      ;; for more efficient diagnostics
      (set-data (gl::initial-node node) :some-repair-applied t)
      (loop for node in new-nodes
            do (push (type-of repair) (statuses node))
               (push 'fcg::added-by-repair (statuses node)))
      (notify fix-applied 'nothing->holistic form-constraints (list holophrase-cxn))
      ; fix-applied (repair-name symbol) (form list) (learned-cxns list))
      (cip-enqueue (first new-nodes) (cip node)
                   (get-configuration node :queue-mode)))))
