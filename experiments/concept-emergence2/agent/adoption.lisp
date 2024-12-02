(in-package :cle)

;; ----------------------------
;; + Adoption of construction +
;; ----------------------------

;;;; events
(define-event event-adopt-start (cxn cxn))

(defmethod adopt ((agent cle-agent) meaning form)
  "Adopt a new cxn into the lexicon of the agent."
  (let* ((cxn (car (find-best-concept agent :inventories '(:fast :unassigned :trash)))))

    
    (if cxn
      ;; CASE 1: only if something is discriminative you associate existing cxn with the new form
      (let* ((new-cxn (copy-object cxn))
             (old-form (form new-cxn)))

        ;; set the new form
        (setf (form new-cxn) form)

        ;; if it is in the unassigned inventory, remove it from there
        (when (not (stringp (form cxn)))      
          (remhash old-form (get-inventory (lexicon agent) :unassigned)))
    
        ;; push the new construction
        (update-lexicon-inventory (lexicon agent) new-cxn)
        ;; update monitor
        (setf (invented-or-adopted agent) t)
        ;; notify
        (notify event-adopt-start new-cxn)
        ;; return created construction
        new-cxn)

      ;; CASE 2: if nothing is found, invent a new cxn
      (when (get-configuration (experiment agent) :cluster-new-cxns)
        (let ((new-cxn (make-cxn agent meaning form)))
          ;; push the new construction
          (update-lexicon-inventory (lexicon agent) new-cxn)
          ;; update monitor
          (setf (invented-or-adopted agent) t)
          ;; notify
          (notify event-adopt-start new-cxn)
          ;; return created construction
          new-cxn)))))

(defmethod reset-adopt ((agent cle-agent) applied-cxn meaning)
  "Resets the cxn by resetting its meaning." 
  (reset-cxn agent applied-cxn meaning)
  ;; update monitor
  (setf (invented-or-adopted agent) t)
  ;; notify
  (notify event-adopt-start applied-cxn)
  ;; return created construction
  applied-cxn)
