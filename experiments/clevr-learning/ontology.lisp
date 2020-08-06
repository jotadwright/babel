(in-package :clevr-learning)

;;;; This file contains functions related to the
;;;; agent's ontology

(defun get-chunk (agent chunk-id)
  "Get the chunk with the given ID from the agent's ontology"
  (find chunk-id
        (get-data (ontology agent) 'programs)
        :key #'id))

(defun answer->category (ontology answer)
  "Find the category in the ontology that corresponds
   to the answer in string form."
  (let ((all-categories
         (loop for field in (fields ontology)
               for field-data = (get-data ontology field)
               when (listp field-data)
               append field-data)))
    (cond
     ((stringp answer)
      (let ((found (find (internal-symb (upcase (mkstr answer)))
                         all-categories :key #'id)))
        (if found found (parse-integer answer))))
     ((eql t answer)
      (find 'yes all-categories :key #'id))
     ((null answer)
      (find 'no all-categories :key #'id)))))

(define-event composer-chunk-added (chunk chunk))

(defun add-composer-chunk (ontology chunk)
  ;; add the chunk to the ontology's composer-chunks
  ;; unless when an identical chunk is already there
  ;; only check chunks with more than 1 predicate
  (let ((add-chunk-p
         (and (> (length (irl-program chunk)) 1)
              (loop for other-chunk in (get-data ontology 'composer-chunks)
                    always (or (= (length (irl-program other-chunk)) 1)
                               (not (equivalent-irl-programs? (irl-program chunk)
                                                              (irl-program other-chunk))))))))
    (when add-chunk-p
      (let ((new-chunk-id (make-id
                           (format nil "~{~a~^+~}"
                                   (mapcar #'first
                                           (irl-program chunk))))))
        (setf (id chunk) new-chunk-id)
        (push-data ontology 'composer-chunks chunk)
        (notify composer-chunk-added chunk)))))

(define-event chunk-added (chunk chunk))
(define-event chunk-removed (chunk chunk))
(define-event ontology-changed)

(defun add-chunk (ontology chunk)
  "Add the chunk to the ontology"
  (notify chunk-added chunk)
  (notify ontology-changed)
  (push-data ontology 'programs chunk))

(defun remove-chunk (ontology chunk)
  "Remove the chunk from the ontology"
  (notify chunk-removed chunk)
  (notify ontology-changed)
  (delete chunk (get-data ontology 'programs)))

(defun remove-unreachable-chunks (agent)
  (loop for chunk in (get-data (ontology agent) 'programs)
        unless (chunk-reachable-p agent chunk)
        do (remove-chunk (ontology agent) chunk)))

(defun chunk-reachable-p (agent chunk)
  "Check if the chunk is in use in some cxn"
  (or (find (id chunk) (constructions (grammar agent))
            :key #'(lambda (cxn) (attr-val cxn :meaning)))
      (when (eql (get-configuration agent :alignment-strategy) :lateral-inhibition)
        (find (id chunk) (get-data (blackboard (grammar agent)) 'trash)
              :key #'(lambda (cxn) (attr-val cxn :meaning))))))

(defun solution->chunk (agent solution &key (initial-score 0.5))
  "Store the irl-program AND the bind statements in a chunk"
  (make-instance
   'chunk
   :irl-program (append (bind-statements solution)
                        (irl-program (chunk solution)))
   :target-var (let* ((program (irl-program (chunk solution)))
                      (target-var (get-target-var program)))
                 (cons target-var
                       (get-type-of-var target-var program
                                        :primitive-inventory (primitives agent))))
   :open-vars (let* ((program (irl-program (chunk solution)))
                     (all-vars
                      (find-all-anywhere-if #'variable-p
                                            (append (bind-statements solution)
                                                    program)))
                     (open-vars
                      (set-difference
                       (get-open-vars program)
                       all-vars)))
                (mapcar #'(lambda (var)
                            (cons var (get-type-of-var var program
                                                       :primitive-inventory (primitives agent))))
                        open-vars))
   :score initial-score))

(defun find-equivalent-chunk (agent chunk)
  "Find a chunk in the ontology of the agent that has
   the same irl-program as the new chunk."
  (let ((all-chunks (find-data (ontology agent) 'programs)))
    (find (irl-program chunk) all-chunks
          :key #'irl-program
          :test #'equivalent-irl-programs?)))

(define-event chunk-rewarded (chunk chunk))
(define-event chunk-punished (chunk chunk))

(defun inc-chunk-score (chunk &key (delta 0.1) (upper-bound 1.0))
  (incf (score chunk) delta)
  (when (> (score chunk) upper-bound)
    (setf (score chunk) upper-bound))
  (notify chunk-rewarded chunk)
  chunk)

(defun dec-chunk-score (agent chunk &key (delta 0.1) (lower-bound 0.0)
                              (remove-on-lower-bound t))
  (decf (score chunk) delta)
  (when (< (score chunk) lower-bound)
    (if remove-on-lower-bound
      (let ((cxns-using-chunk-p (find (id chunk) (constructions (grammar agent))
                                      :key #'(lambda (cxn) (attr-val cxn :meaning)))))
        (if cxns-using-chunk-p
          (setf (score chunk) lower-bound)
          (remove-chunk (ontology agent) chunk)))
      (setf (score chunk) lower-bound)))
  (notify chunk-punished chunk)
  chunk)

