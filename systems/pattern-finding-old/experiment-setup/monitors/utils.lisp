(in-package :pattern-finding-old)

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

(defun categorial-links->s-dot (new-links categorial-network)
  (let* ((g (fcg::graph categorial-network))
         (graph-properties '((s-dot::fontcolor "#000000")
                             (s-dot::fontsize "10.0")
                             (s-dot::fontname "Helvetica")
                             (s-dot::rankdir "LR")))
         (all-node-names
          (remove-duplicates
           (loop for (from . to) in new-links
                 append (list from to))))
         (all-node-ids
          (loop for node-name in all-node-names
                for id = (gethash node-name (graph-utils::nodes g))
                collect id))
         (all-edges
          (loop for (from . to) in new-links
                collect (cons (gethash from (graph-utils::nodes g))
                              (gethash to (graph-utils::nodes g)))))
         (s-dot-nodes
          (loop for node-name in all-node-names
                for node-id in all-node-ids
                collect (graph-utils::categorial-network-node->s-dot
                         node-name node-id)))
         (s-dot-edges
          (loop for (from-id . to-id) in all-edges
                for edge-weight = (graph-utils::edge-weight g from-id to-id)
                collect (graph-utils::categorial-network-edge->s-dot
                         from-id to-id
                         :weight edge-weight :directedp nil
                         :colored-edges-0-1 nil))))
    `(s-dot::graph ,graph-properties
                   ,@s-dot-nodes
                   ,@s-dot-edges)))