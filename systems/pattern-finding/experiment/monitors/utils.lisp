(in-package :pf)

;;;; Success Buffer
(defun get-windowed-success (experiment)
  (let* ((buffer-size (get-configuration experiment :buffer-size))
         (interactions (first-n buffer-size (interactions experiment)))
         (successes (mapcar #'communicated-successfully interactions)))
    (/ (count t successes) (length interactions))))

(defun get-overall-success (experiment)
  (let ((successes (mapcar #'communicated-successfully (interactions experiment))))
    (/ (count t successes) (length (interactions experiment)))))

;;;; Repair buffer
;; access :applied-repair label in interactions

;;;; Failed corpus buffer
;; access :utterance and :gold-standard-meaning label in interactions
;; together with communicated-successfully slot

(defun add-cxns-to-wi (constructions)
  (dolist (cxn constructions)
    (add-element (make-html cxn))))

(defun categorial-links->s-dot (new-links)
  (let* ((graph-properties '((s-dot::fontcolor "#000000")
                             (s-dot::fontsize "10.0")
                             (s-dot::fontname "Helvetica")
                             (s-dot::rankdir "LR")))
         (node-names
          (remove-duplicates
           (loop for (from . to) in new-links
                 append (list from to))))
         (node-names-to-id
          (loop for name in node-names
                for id from 1
                collect (cons name id)))
         (all-edges
          (loop for (from . to) in new-links
                collect (cons (rest (assoc from node-names-to-id))
                              (rest (assoc to node-names-to-id)))))
         (s-dot-nodes
          (loop for (name . id) in node-names-to-id
                collect (graph-utils::categorial-network-node->s-dot name id)))
         (s-dot-edges
          (loop for (from-id . to-id) in all-edges
                collect (graph-utils::categorial-network-edge->s-dot from-id to-id))))
    `(s-dot::graph ,graph-properties
                   ,@s-dot-nodes
                   ,@s-dot-edges)))