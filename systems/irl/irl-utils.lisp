(in-package :irl)

(export '(all-variables all-bind-statements all-predicates
          get-unconnected-vars get-target-var
          get-open-vars get-type-of-var))

(defun all-variables (irl-program)
  (find-all-anywhere-if #'variable-p irl-program))

(defun all-bind-statements (irl-program)
  (find-all 'bind irl-program :key #'car))

(defun all-predicates (irl-program)
  (find-all-if-not #'(lambda (x) (eql x 'bind))
                   irl-program :key #'car))

(defun get-unconnected-vars (irl-program)
  ;; find all unconnected variables, in other words, all those that
  ;; appear once in the irl-program
  ;; notice that this also includes the target-var
  (loop with variables = (all-variables irl-program)
        for var in (remove-duplicates variables)
        if (= (count var variables) 1)
        collect var))

(defun get-target-var (irl-program)
  ;; returns the one unconnected open-var that appears as first argument
  ;; of a primitive, nil otherwise
  (unless (length= 0 irl-program)
    (if (and (length= irl-program 1)
             (eq (car (first irl-program)) 'bind))
      (third (first irl-program))
      (let* ((open-vars (get-unconnected-vars irl-program))
             (target-vars (intersection open-vars (mapcar #'second irl-program)))
             (target-var (when (= 1 (length target-vars))
                           (first target-vars))))
        target-var))))

(defun get-open-vars (irl-program)
  ;; find all unconnected variables which are not target
  (set-difference
   (get-unconnected-vars irl-program)
   (mapcar #'second irl-program)))

(defun get-type-of-var (var program/predicate
                        &key (primitive-inventory *irl-primitives*))
  (let ((predicate (if (listp (first program/predicate))
                     (find var program/predicate :test #'member)
                     program/predicate)))
    (if (eql (first predicate) 'bind)
      (if (and (or (eql var (third predicate))
                   (eql var (fourth predicate)))
               (not (variable-p (second predicate))))
        (second predicate)
        'entity)
      (let* ((primitive (find-primitive (first predicate) primitive-inventory))
             (slot-spec (nth (- (position var predicate) 1)
                             (slot-specs primitive))))
        (slot-spec-type slot-spec)))))

(export '(irl-program-connected? irl-program-p))

(defun irl-program-connected? (irl-program)
  "Checks whether an irl program is connected. Returns t if so, the
   number of sub graphs and the sub graphs themselves"
  (loop with classes = nil
        with sub-networks = nil
        for x in irl-program
        for variables = (find-all-if #'variable-p x)
        for (cs subs) = (multiple-value-list
                         (loop for class in classes
                               for sub-network in sub-networks
                               when (loop for var in variables
                                          thereis (member var class))
                               collect class into cs
                               and
                               collect sub-network into subs
                               finally (return (values cs subs))))
        if cs
        do
        (loop for class in cs do (setf classes (remove class classes)))
        (push (remove-duplicates (reduce #'append (cons variables cs))) classes)
        (loop for sub in subs do (setf sub-networks (remove sub sub-networks)))
        (push (cons x (reduce #'append subs)) sub-networks)
        else
        do
        (push variables classes)
        (push (list x) sub-networks)
        finally
        (return (values
                 (< (length classes) 2)
                 (length classes)
                 sub-networks))))

(defun irl-program-p (thing &key (primitive-inventory *irl-primitives*))
  "returns t if thing conforms to the basic syntax of irl-programs
   list of bind statements (bind ...)  and irl-primitives (primitive ..)"
  (and (listp thing)
       (loop for s in thing
             always (and (listp s)
                         (or (eq (first s) 'bind)
                             (find-primitive (first s) primitive-inventory))))))
        