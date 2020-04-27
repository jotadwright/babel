(in-package :irl-2)

;; #########################################
;; run node rating fn
;; -----------------------------------------

(defun run-node-rating-fn (node composer)
  (let ((mode (get-configuration composer :node-rating-mode)))
    (rate-node node composer mode)))

(defgeneric rate-node (node composer mode)
  (:documentation "Give a rating to a node; lower == better"))

(defmethod rate-node ((node chunk-composer-node)
                      (composer chunk-composer)
                      (mode (eql :short-programs-with-few-primitives-and-open-vars)))
  "simple default function for rating nodes in the search tree"
  (let ((chunk (chunk node)))
    (/ (+ (node-depth node)            ;; the less depth the better
          (length (open-vars chunk))   ;; less open vars are better
          (length (irl-program chunk)) ;; less primitives are better
          ;; less duplicate primitives are better
          ;; (bind statements are not considered)
          (let ((predictes (all-predicates (irl-program chunk))))
            (* 5 (- (length predictes)
                    (length (remove-duplicates predictes))))))
       ;; the higher the score the better
       (score chunk))))

;; #########################################
;; run chunk scoring fn
;; -----------------------------------------

(defun run-chunk-scoring-fn (node composer)
  (let ((mode (get-configuration composer :chunk-scoring-mode)))
    (score-chunk node composer mode)))

(defgeneric score-chunk (node composer mode)
  (:documentation "Computes a score for a chunk, a float
    between 0 (very bad) and 1 (very good)."))

(defmethod score-chunk ((node chunk-composer-node) (composer chunk-composer)
                        (mode (eql :source-chunks-average)))
  (average (mapcar #'score (source-chunks node))))
  
             