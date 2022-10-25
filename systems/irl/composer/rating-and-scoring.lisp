(in-package :irl)

;; #########################################
;; run-node-cost
;; -----------------------------------------

(defun run-node-cost (node composer)
  (let ((mode (get-configuration composer :node-cost-mode)))
    (node-cost node composer mode)))

(defgeneric node-cost (node composer mode)
  (:documentation "Give a cost to a node; lower == better"))

(defmethod node-cost ((node chunk-composer-node)
                      (composer chunk-composer)
                      (mode (eql :short-programs-with-few-primitives-and-open-vars)))
  "simple default function for rating nodes in the search tree"
  (let ((chunk (chunk node)))
    (/ (+ (depth node)                 ;; the less depth the better
          (length (open-vars chunk))   ;; less open vars are better
          (length (irl-program chunk)) ;; less primitives are better
          ;; less duplicate primitives are better
          ;; (bind statements are not considered)
          (let ((predictes (all-predicates (irl-program chunk))))
            (* 5 (- (length predictes)
                    (length (remove-duplicates predictes))))))
       ;; the higher the score the better
       (score chunk))))

(defmethod node-cost ((node chunk-composer-node)
                      (composer chunk-composer)
                      (mode (eql :depth-first)))
  (- (depth node)))

(defmethod node-cost ((node chunk-composer-node)
                      (composer chunk-composer)
                      (mode (eql :breadth-first)))
  (created-at node))
  

;; #########################################
;; run-chunk-score
;; -----------------------------------------

(defun run-chunk-score (node composer)
  (let ((mode (get-configuration composer :chunk-score-mode)))
    (chunk-score node composer mode)))

(defgeneric chunk-score (node composer mode)
  (:documentation "Computes a score for a chunk, a float
    between 0 (very bad) and 1 (very good)."))

(defmethod chunk-score ((node chunk-composer-node) (composer chunk-composer)
                        (mode (eql :source-chunks-average)))
  (average (mapcar #'score (source-chunks node))))
  
             