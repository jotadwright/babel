(in-package :pattern-finding-old)

(defun compute-args (anti-unification-result)
  (let* ((connecting-vars
          ;; find args that connect the delta back to the generalisation
          (handle-connecting-vars anti-unification-result))
         (decoupled-vars
          ;; find variables that were decoupled in the generalisation
          (handle-decoupled-vars anti-unification-result connecting-vars))
         (args-to-far
          (append-data-fields connecting-vars decoupled-vars))
         (singleton-vars
          ;; find free variables in the delta that should be passed
          ;; along in the item-based cxn
          (handle-singleton-vars anti-unification-result args-to-far))
         (all-slot-args
          ;; combine them
          (append-data-fields args-to-far singleton-vars))
         (top-lvl-args
          ;; compute top lvl args for the item-based cxn
          (compute-top-lvl-args anti-unification-result all-slot-args)))
    ;; combine all args and return
    (append-data-fields all-slot-args top-lvl-args)))

(defun handle-connecting-vars (anti-unification-result)
  (with-slots (generalisation
               pattern-bindings
               source-bindings
               pattern-delta
               source-delta) anti-unification-result
    (let ((vars-in-generalisation
           (remove-duplicates (find-all-anywhere-if #'variable-p generalisation)))
          (connecting-vars (make-blackboard)))
      (loop for var in vars-in-generalisation
            for pattern-var = (first (rassoc var pattern-bindings))
            for source-var = (first (rassoc var source-bindings))
            when (and pattern-var source-var
                      (find-anywhere pattern-var pattern-delta)
                      (find-anywhere source-var source-delta))
            do (push-data connecting-vars :generalisation-slot-args var)
               (push-data connecting-vars :pattern-args  pattern-var)
               (push-data connecting-vars :source-args source-var))
      connecting-vars)))

(defun handle-decoupled-vars (anti-unification-result previous-vars)
  (with-slots (generalisation
               pattern-bindings
               source-bindings
               pattern-delta
               source-delta) anti-unification-result
    (let* ((pattern-delta-decoupled-link-vars
            (loop for (binding . rest) on pattern-bindings
                  when (find (car binding) rest :key #'car :test #'equalp)
                  collect (car binding)))
           (source-delta-decoupled-link-vars
            (loop for (binding . rest) on source-bindings
                  when (find (car binding) rest :key #'car :test #'equalp)
                  collect (car binding)))
           (decoupled-link-vars (make-blackboard)))
      (multiple-value-bind (longest-delta-key other-delta-key
                            longest-var-list other-var-list
                            longest-bindings other-bindings)
          (if (> (length pattern-delta-decoupled-link-vars) (length source-delta-decoupled-link-vars))
            (values :pattern-args :source-args
                    pattern-delta-decoupled-link-vars source-delta-decoupled-link-vars
                    pattern-bindings source-bindings)
            (values :source-args :pattern-args
                    source-delta-decoupled-link-vars pattern-delta-decoupled-link-vars
                    source-bindings pattern-bindings))
        (loop for var in longest-var-list
              for vars-in-generalisation = (mapcar #'cdr (find-all var longest-bindings :key #'car))
              for vars-in-other-delta = (loop for v in vars-in-generalisation
                                              collect (first (rassoc v other-bindings)))
              for vars-in-longest-delta = (make-list (length vars-in-generalisation) :initial-element var)
              do (loop for x in vars-in-longest-delta
                       for y in vars-in-generalisation
                       for z in vars-in-other-delta
                       unless (and (member x (get-data previous-vars longest-delta-key))
                                   (member y (get-data previous-vars :generalisation-slot-args))
                                   (member z (get-data previous-vars other-delta-key)))
                       do (push-data decoupled-link-vars longest-delta-key x)
                          (push-data decoupled-link-vars :generalisation-slot-args y)
                          (push-data decoupled-link-vars other-delta-key z))))
      decoupled-link-vars)))

(defun handle-singleton-vars (anti-unification-result previous-vars)
  (with-slots (generalisation
               pattern-bindings
               source-bindings
               pattern-delta
               source-delta) anti-unification-result    
    (let* ((pattern-delta-vars
            (remove-duplicates (find-all-anywhere-if #'variable-p pattern-delta)))
           (source-delta-vars
            (remove-duplicates (find-all-anywhere-if #'variable-p source-delta)))
           (singleton-pattern-vars
            (set-difference pattern-delta-vars (get-data previous-vars :pattern-args)))
           (singleton-source-vars
            (set-difference source-delta-vars (get-data previous-vars :source-args)))
           (singleton-args (make-blackboard)))
      (multiple-value-bind (longest-delta-key other-delta-key
                            longest-var-list other-var-list
                            longest-bindings other-bindings)
          (if (> (length singleton-pattern-vars) (length singleton-source-vars))
            (values :pattern-args :source-args
                    singleton-pattern-vars singleton-source-vars
                    pattern-bindings source-bindings)
            (values :source-args :pattern-args
                    singleton-source-vars singleton-pattern-vars
                    source-bindings pattern-bindings))
        (loop for var-i in longest-var-list
              for gen-var = (rest (assoc var-i longest-bindings))
              for var-j = (first (rassoc gen-var other-bindings))
              do (push-data singleton-args longest-delta-key var-i)
                 (push-data singleton-args :generalisation-slot-args (or gen-var (make-var 'arg)))
                 (push-data singleton-args other-delta-key (or var-j (make-var 'arg)))
                 (setf other-var-list (remove var-j other-var-list)))
        (loop for var-i in other-var-list
              for gen-var = (rest (assoc var-i other-bindings))
              for var-j = (first (rassoc gen-var longest-bindings))
              do (push-data singleton-args other-delta-key var-i)
                 (push-data singleton-args :generalisation-slot-args (or gen-var (make-var 'arg)))
                 (push-data singleton-args longest-delta-key (or var-j (make-var 'arg)))))
      singleton-args)))

(defun compute-top-lvl-args (anti-unification-result slot-args)
  (let ((unconnected-vars (get-unconnected-vars (generalisation anti-unification-result)))
        (slot-args (get-data slot-args :generalisation-slot-args))
        (top-lvl-args (make-blackboard)))
    (set-data top-lvl-args :top-lvl-args
              (set-difference unconnected-vars (intersection unconnected-vars slot-args)))
    top-lvl-args))