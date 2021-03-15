(in-package :irl)

;; ##########################################
;; run expand chunk fns
;; ------------------------------------------

(defun run-expand-chunk-fns (chunk composer)
  (loop for mode in (get-configuration composer :expand-chunk-modes)
        append (expand-chunk chunk composer mode)))


(defgeneric expand-chunk (chunk composer mode)
  (:documentation "Expand the chunk according to mode"))

(defmethod combine-chunk-program ((chunk chunk) (other-chunk chunk))
  "Adds the network of other chunk to chunk
   when the target variable of that chunk
   is compatble with an open variable in chunk"
  (loop for open-var in (open-vars chunk)
        for (open-var-id . open-var-type) = open-var
        when (or (subtypep open-var-type (cdr (target-var other-chunk)))
                 (subtypep (cdr (target-var other-chunk)) open-var-type))
        collect (let* ((variables-in-both-chunks 
                        (intersection 
                         (all-variables (irl-program chunk))
                         (all-variables (irl-program other-chunk))))
                       (substitutions 
                        (cons (cons (car (target-var other-chunk)) open-var-id)
                              (loop for var in variables-in-both-chunks
                                    collect (cons var (make-id var)))))
                       (other-chunk-2 (substitute-variables other-chunk substitutions)))
                  (make-instance 'chunk
                                 :target-var (if (irl-program chunk)
                                               (target-var chunk)
                                               (target-var other-chunk-2))
                                 :open-vars (append (remove open-var (open-vars chunk))
                                                    (open-vars other-chunk-2))
                                 :irl-program (append (irl-program chunk)
                                                      (irl-program other-chunk-2))
                                 :score (/ (+ (score chunk) (score other-chunk)) 2)))))

(defmethod expand-chunk ((chunk chunk) (composer chunk-composer)
                         (mode (eql :combine-program)))
  "Adds the network of another chunk when the target variable of that
   chunk is compatble with an open variable"
  (loop for other-chunk in (chunks composer)
        append (loop for new-chunk in (combine-chunk-program chunk other-chunk)
                     collect (cons new-chunk (list other-chunk)))))

(defmethod combine-chunk-call-pattern ((chunk chunk) (other-chunk chunk))
  "Adds the call pattern of other-chunk when the target variable of
   that chunk is compatble with an open variable in chunk"
  (loop for open-var in (open-vars chunk)
        for (open-var-id . open-var-type) = open-var
        when (or (subtypep open-var-type (cdr (target-var other-chunk)))
                 (subtypep (cdr (target-var other-chunk)) open-var-type))
        collect (let* ((variables-in-both-chunks 
                        (intersection
                         (all-variables (irl-program chunk))
                         (all-variables (irl-program other-chunk))))
                       (substitutions 
                        (cons (cons (car (target-var other-chunk)) open-var-id)
                              (loop for var in variables-in-both-chunks
                                    collect (cons var (make-id var))))))
                  (make-instance 'chunk
                                 :irl-program `(,@(irl-program chunk)
                                                ,(substitute-variables
                                                  `(,(id other-chunk)
                                                    ,(car (target-var other-chunk))
                                                    ,@(mapcar #'car (open-vars other-chunk)))
                                                  substitutions))
                                 :target-var (target-var chunk)
                                 :open-vars (append (remove open-var-id (open-vars chunk) :key #'car)
                                                    (substitute-variables
                                                     (open-vars other-chunk)
                                                     substitutions))
                                 :score (/ (+ (score chunk) (score other-chunk)) 2)))))

(defmethod expand-chunk ((chunk chunk) (composer chunk-composer)
                         (mode (eql :combine-call-pattern)))
  "Adds the call pattern of another chunk when the target variable of
   that chunk is compatble with an open variable"
  (loop for other-chunk in (chunks composer)
        append (loop for new-chunk in (combine-chunk-call-pattern chunk other-chunk)
                     collect (cons new-chunk (list other-chunk)))))

(defmethod recombine-open-variables ((chunk chunk) primitive-inventory)
  "Tries to account for open variables of 'chunk' by using primitives
   that are already in the irl program. Returns all possible
   solutions."
  (loop with irl-program = (irl-program chunk)
        for (open-var-id . open-var-type) in (open-vars chunk)
        append (loop for predicate in irl-program
                     for target-var-id = (cadr predicate)
                     for target-var-type 
                     = (get-type-of-var target-var-id predicate
                                        :primitive-inventory primitive-inventory)
                     when (and (not (eq target-var-id (car (target-var chunk))))
                               (not (find open-var-id (cdr predicate)))
                               (or (subtypep open-var-type target-var-type)
                                   (subtypep target-var-type open-var-type)))
                     collect (let ((new-chunk 
                                    (substitute-variables 
                                     chunk (list (cons open-var-id target-var-id)))))
                               (setf (open-vars new-chunk)
                                     (delete (find target-var-id (open-vars new-chunk) :key #'car)
                                             (open-vars new-chunk)))
                               new-chunk))))

(defmethod expand-chunk ((chunk chunk) (composer chunk-composer)
                         (mode (eql :recombine-open-variables)))
  "Tries to account for open variables of 'chunk' by using primitives
   that are already in the irl program. Returns all possible
   solutions."
  (declare (ignore composer))
  (loop for new-chunk in (recombine-open-variables chunk (primitive-inventory composer))
        collect (cons new-chunk nil)))

(defun link-open-variables (chunk)
  "Tries to make two open variables of compatible type equal and when
   it does, reduces the number of open variables of the chunk by one."
  (loop for (open-var . other-open-vars) on (open-vars chunk)
        for open-var-id = (car open-var)
        for open-var-type = (cdr open-var)
        append (loop for other-open-var in other-open-vars
                     for other-open-var-id = (car other-open-var)
                     for other-open-var-type = (cdr other-open-var)
                     when (or (subtypep open-var-type other-open-var-type)
                              (subtypep other-open-var-type open-var-type))
                     collect (let ((new-chunk 
                                    (substitute-variables 
                                     chunk (list (cons open-var-id other-open-var-id)))))
                               (setf (open-vars new-chunk)
                                     (delete (find other-open-var-id (open-vars new-chunk) :key #'car)
                                             (open-vars new-chunk)))
                               new-chunk))))

(defmethod expand-chunk ((chunk chunk) (composer chunk-composer)
                         (mode (eql :link-open-variables)))
  "Tries to make two open variables of compatible type equal and when
   it does, reduces the number of open variables of the chunk by one."
  (declare (ignore composer))
  (loop for new-chunk in (link-open-variables chunk)
        collect (cons new-chunk nil)))